// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.LogicEngine.Datalog

open System.Collections.Generic
open NLog

open InfonSimplifier
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Z3
open Microsoft.Research.DkalBackends
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator.Datalog
open Microsoft.Research.Dkal.Globals

type EvidenceTree(head, args)=
    let _head: string = head
    let _args: EvidenceTree list = args

    member et.Head with get() = _head
    member et.Args with get() = _args

type DatalogLogicEngine(assemblyInfo: MultiAssembly) = 

  let log = LogManager.GetLogger("LogicEngine.Datalog")

  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let _context= new Context()
  let _infonSimplifier= new InfonSimplifier()
  let _dkalRelationDeclarations = assemblyInfo.Relations
  let _dkalFunctions= new Dictionary<string, Function>()

  do 
    _dkalRelationDeclarations |> Seq.iter (fun rd -> let fn = {Name = rd.Name;
                                                               RetType=Type.Infon;
                                                               ArgsType= rd.Args |> List.map (fun v -> v.Type);
                                                               Identity= None}
                                                     _dkalFunctions.[rd.Name] <- fn
                                          )

  member private se.FinalOutcome (infon: ITerm) =
    match infon with
    | ImpliesInfon (_, i) -> se.FinalOutcome i
    | Forall(_,t) -> se.FinalOutcome t
    | i -> i

  member private se.CanSign (principal: string) (infon: ITerm) =
    match se.FinalOutcome infon with
    | SaidInfon (PrincipalConstant(p), _) -> principal = p
    | _ -> false

  /// Lift all queries from infon that need to be forwarded to the substrates
  member private se.substrateQueries (infon: ITerm) = 
    seq {
        match infon with
        | AndInfon(infons) -> 
          yield! infons |>
                        Seq.fold (fun acc inf -> Seq.append acc (se.substrateQueries inf)) (seq [])
        | AsInfon(exp) -> yield exp
        | _ -> yield! []
    }

  member private se.purifyExplanation (explanation: Expr) =
    if explanation.IsEq then
        se.purifyExplanation (explanation.Args.[1])
    else if explanation.IsAnd then
        se.purifyExplanation (explanation.Args.[0])
    else
        explanation

  member private se.datalogToInfon (query: string) (relationTranslations: Dictionary<string, Ast.InfonFormula>) (regConsts:Dictionary<uint32, (Microsoft.Z3.Sort*Expr)>) =
    let rec instantiateTemplate(template: Ast.InfonFormula) (ndx: int) =
        match template with
        | Ast.AtomFormula(str, args) -> let fn= _dkalFunctions.[str];
                                        let argVars= fn.ArgsType |> List.mapi (fun i typ -> Var({Name= "Var"+(ndx+i).ToString(); Type= typ}))
                                        argVars, App(fn, argVars)
        | Ast.AndFormula(t1, t2) -> let var1, inf1= instantiateTemplate (t1) (ndx)
                                    let var2, inf2= instantiateTemplate (t2) (ndx+var1.Length)
                                    var1 @ var2, AndInfon([inf1; inf2])
        | Ast.ImpliesFormula(t1, t2) -> let var1, inf1= instantiateTemplate (t1) ndx
                                        let var2, inf2= instantiateTemplate (t2) (ndx+var1.Length)
                                        var1 @ var2, ImpliesInfon(inf1, inf2)
        | Ast.SpeechFormula(ppal, speech, temp) -> let vars, inf= instantiateTemplate (temp) (ndx+2)    // have to account for the principal and speech variable
                                                   let ppalVar= Var({ Name = "Var" + ndx.ToString(); Type = Type.Principal })
                                                   let speechVar= Var({ Name = "Var" + (ndx+1).ToString(); Type = Type.Principal }) // superfluous, just to keep datalog translator structure
                                                   [ppalVar;speechVar] @ vars, SaidInfon(ppalVar, inf)

    let relName= query.Split([|'('|], System.StringSplitOptions.RemoveEmptyEntries).[0]
    let datalogArgs= query.Split([|'('|], System.StringSplitOptions.RemoveEmptyEntries).[1].Replace(")","").Replace(".","").Replace(" ","")
                          .Split([|','|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.toList |> List.map (fun s -> uint32(s))
    let template= relationTranslations.[relName]
    let vars, infon= instantiateTemplate template 0
    // now we need to substitute the vars with the actual valuation in the explanation. That's why vars have been setup in the same order as in the datalog relation
    // TODO check that the datalog query has no vars, if it does we're in trouble (ie, what does it mean?)
    let mapping = new Dictionary<IVar, ITerm>()
    vars |> Seq.iteri (fun i v -> mapping.[v :?> IVar] <- z3ExprToDkal (_context.MkInt(datalogArgs.[i])) ((v :?> IVar).Type) (regConsts))
    let subst= Substitution(mapping)
    infon.Apply subst

  member private se.getSimpleEvidenceForInfon (infon: ITerm) =
    let knowledge= _infostrate.Value.Knowledge
    let evidences= knowledge |> Seq.collect (fun kn -> match kn with
                                                       | JustifiedInfon(inf, ev) when inf = infon -> [ev]
                                                       | _ -> [] )
                             |> Seq.toList
    if evidences.Length = 0 then
        EmptyEvidence
    else
        evidences.[0]

  member private se.buildEvidence (explanation: Expr) (relationTranslations: Dictionary<string, Ast.InfonFormula>) (query: BoolExpr) (regConsts: Dictionary<uint32, (Microsoft.Z3.Sort*Expr)>)=
    let rec buildFromPurifiedTree (tree: EvidenceTree) =
        // the tree has to be rooted at the infon we need evidence for
        let infon= se.datalogToInfon (tree.Head) (relationTranslations) (regConsts)

        if (tree.Args.Length = 0) then
            // this has to be an original infon. We need to check if we had a corresponding signed infon
            se.getSimpleEvidenceForInfon infon
        else
            let premise= if tree.Args.Length = 1 then buildFromPurifiedTree(tree.Args.[0])
                                                 else AndEvidence( tree.Args |> Seq.map (fun ev -> buildFromPurifiedTree(ev)) |> Seq.toList )
            ModusPonensEvidence(premise, infon)

    let rec purifyTree (tree: Expr)=
        let head= tree.FuncDecl.Name.ToString().Replace(".", "").Replace(" ", "").Split([|":-"|], System.StringSplitOptions.RemoveEmptyEntries).[0]
        let args= tree.Args |> Seq.map (fun a -> purifyTree(a)) |> Seq.toList
        EvidenceTree(head, args)

    // TODO build the evidence from the explanation tree.
    // first cleanup the AST so we get rid of all the :- and so on and end up with an actual "tree" rooted at the original tree's query itself
    let mutable query= query
    if query.IsAnd && query.Args.Length = 1 then
        query <- query.Args.[0] :?> BoolExpr
    let mutable explanationTree= purifyTree explanation
    while explanationTree.Head.Split([|'('|]).[0] <> query.FuncDecl.Name.ToString().Split([|'('|]).[0] do
        explanationTree <- explanationTree.Args.Item(0)

    if explanationTree.Args.Length > 1 then
        AndEvidence( explanationTree.Args |> Seq.map (fun exp -> buildFromPurifiedTree(exp)) |> Seq.toList )
    else
        buildFromPurifiedTree(explanationTree.Args.[0])

  member private se.mergeSubstitutionWithAnswer (subst:ISubstitution) (answer:Expr) (regVarNamesAndTypes:Dictionary<Expr, string*IType>)
                                                (mappedConstants: Dictionary<uint32, Microsoft.Z3.Sort*Expr>) (translatedConstants:List<Ast.Term>)=
    seq {
        if answer.IsTrue then
            yield subst
        else if answer.IsEq then
            // merge subst with this assignment
            // TODO is it right to assume that substitutions are just horn clauses?
            let (lhs, rhs)= if answer.Args.[0].IsVar then answer.Args.[0],answer.Args.[1] else answer.Args.[1],answer.Args.[0]
            // complete the correct value, which needs to be backtranslated from the translation chain...
            // CHECK: WHY AM I GETTING const = const CLAUSES sometimes??!?!?
            if (not (lhs.IsConst && rhs.IsConst)) then
                yield subst.Extend( {Name= fst(regVarNamesAndTypes.[lhs]); Type= snd(regVarNamesAndTypes.[lhs])}, z3ExprToDkal (rhs) (snd(regVarNamesAndTypes.[lhs]))
                                                                                                                               (mappedConstants) )
            else 
                yield subst
        else if answer.IsAnd then
            // merge subst with all merged args. Get substs for each arg and combine them all
            let subsToCombine= answer.Args |> Seq.map (fun arg -> se.mergeSubstitutionWithAnswer subst arg regVarNamesAndTypes mappedConstants translatedConstants)
            let merged = ( subsToCombine |> Seq.fold (fun acc valsForClause -> 
                                                            valsForClause |> Seq.collect (fun valuation -> acc |> Seq.map (fun sub -> valuation.ComposeWith sub))
                                                     ) (seq {yield Substitution.Id})
                         )
            yield! merged
        else if answer.IsOr then
            // create one different merge for each arg
            let newSubs= answer.Args |> Seq.collect (fun arg -> se.mergeSubstitutionWithAnswer subst arg regVarNamesAndTypes mappedConstants translatedConstants)
            yield! newSubs
        else yield! []
    }

  member private se.ignoreSubstrate (infon:ITerm) =
    match infon with
    | AndInfon(infons) -> let ignored= infons |> Seq.fold (fun acc inf -> Seq.append acc [se.ignoreSubstrate inf]) (seq []) |> Seq.toList
                          AndInfon(ignored)
    | AsInfon(exp) -> EmptyInfon
    | _ -> infon


  interface ILogicEngine with
    member se.Start () = ()
    member se.Stop () = ()

    member se.set_Infostrate (infostrate: IInfostrate) =
      _infostrate <- Some infostrate

    member se.get_Infostrate () =
      _infostrate.Value

    member se.set_SignatureProvider (signatureProvider: ISignatureProvider) =
      _signatureProvider <- Some signatureProvider

    member se.get_SignatureProvider () =
      _signatureProvider.Value

    // lifted verbatim from SimpleEngine as this is done on a realm outside datalog
    member se.CheckJustification (evidence: ITerm) = 
      match evidence with
      | SignatureEvidence(PrincipalConstant(ppal), inf, SubstrateConstant(signature)) when signature.GetType() = typeof<int> -> 
        if se.CanSign ppal inf && _signatureProvider.Value.CheckSignature inf ppal (signature :?> int) then
          Some inf
        else
          log.Warn("Spoofed/Invalid signature {0} from {1} on {2}", signature, ppal, inf)
          None
      | ModusPonensEvidence(e1, e2) ->
        match (se :> ILogicEngine).CheckJustification e1, (se :> ILogicEngine).CheckJustification e2 with
        | Some i1, Some (ImpliesInfon(i1', i2)) when i1 = i1' ->
          Some i2
        | _ ->
          log.Warn("Malformed/Unchecked modus ponens proof on {0} and {1}", e1, e2)
          None
      | AndEvidence(evidences) ->
        let infons = List.collect (fun evidence ->  match (se :> ILogicEngine).CheckJustification evidence with
                                                    | Some i -> [i]
                                                    | None -> []) evidences
        if infons.Length = evidences.Length then
          Some <| AndInfon(infons)
        else
          log.Warn("Malformed/Unchecked conjunction proof on {0}", evidence)
          None
      | AsInfonEvidence(query) ->
        if SubstrateDispatcher.Solve [query] [Substitution.Id] |> Seq.isEmpty |> not then
          Some <| AsInfon(query)
        else
          log.Warn("Non-true asInfon evidence at {0}", query)
          None
      | ConcretizationEvidence(ev, subst) ->
        match (se :> ILogicEngine).CheckJustification ev with
        | Some generalProof -> 
          let concreteProof = match generalProof with
                              | :? ForallTerm as ft -> ft.Instantiate subst
                              | _ -> generalProof.Apply subst
          Some concreteProof
        | None -> None
      | _ -> 
        log.Warn("Unhandled evidence type at {0}", evidence)
        None

    /// Obtain a list of Substitution with accompanying side conditions (AsInfon
    /// ITerms). Then return only those Substitutions that satisfy all their 
    /// side conditions.
    // TODO refactor, this is getting huge...
    member se.Derive (target: ITerm) (substs: ISubstitution seq) = 
      log.Debug("Derive {0}", target)
      let datalogTranslator= DatalogTranslator()
      let knowledge= _infostrate.Value.Knowledge
      let originalTarget= target
      let target= se.ignoreSubstrate target

      // will need to know which infons are justified to build proofs
      let justifiedInfons= knowledge |> Seq.collect (fun exp -> match exp with
                                                                | JustifiedInfon(inf, ev) -> [inf]
                                                                | _ -> []
                                                    )
      seq {
        for subst in substs do
            let fp= _context.MkFixedpoint()
            let pars = _context.MkParams()
            pars.Add(":generate-explanations", true)
            fp.Parameters <- pars
            // note we add FAKE_TRUE_RELATION to the "knowledge" so it is taken as a fact by the translator
            let program= datalogTranslator.translateInferenceProblem ( (knowledge |> Seq.map (fun kn -> kn.Apply(subst))
                                                                                  |> Seq.map (fun term -> _infonSimplifier.simplify(term)) |> Seq.append [FAKE_TRUE_FACT] |> Seq.toList,
                                                                        [_infonSimplifier.simplify(target.Apply(subst))])
                                                                     )
            let regRels= new Dictionary<string, FuncDecl>()
            let regConsts= new Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>()
            let invRegConsts= new Dictionary<uint32, Microsoft.Z3.Sort*Expr>()
            let regSorts= new Dictionary<Sort, Microsoft.Z3.Sort>()
            let regVarNames=  new Dictionary<Expr, string*IType>()

            // Z3 allows finite domain sorts, BUT
            // 1) they MUST have a numerical representation
            // 2) when declaring them, you must declare its size. BUT its size is actually the max possible value + 1 (actually representing the range 0..max)
            let fresh= ref (uint32(0))
            program.Declarations |> Seq.iter ( fun decl -> match decl with
                                                           | SortDeclarationPart(sortDecl) ->
                                                                let sort= _context.MkFiniteDomainSort(sortDecl.Name, uint64(program.Sorts.[sortDecl.Name].Count + 1))
                                                                regSorts.[sortDecl.Name] <- sort
                                                                try 
                                                                    regConsts.[sort] <- new Dictionary<Expr, uint32>()
                                                                    program.Sorts.[sortDecl.Name].Values
                                                                        |> Seq.iter ( fun value -> 
                                                                                        let cst= _context.MkConst(value.ToString(), sort)
                                                                                        regConsts.[sort].[cst] <- !fresh
                                                                                        invRegConsts.[!fresh] <- (sort :> Microsoft.Z3.Sort, cst)
                                                                                        fresh := !fresh + uint32(1)
                                                                                    )
                                                                with e -> ()
                                                           | _ -> ()
                                             )

            program.Declarations |> Seq.iter ( fun decl -> match decl with
                                                           | RelationDeclarationPart(relDecl) -> 
                                                                let argsSorts= relDecl.Args
                                                                                 |> Seq.collect (fun arg -> seq {yield regSorts.[snd arg]})
                                                                                 |> Seq.toArray
                                                                regRels.[relDecl.Name] <- _context.MkFuncDecl(relDecl.Name, argsSorts, _context.BoolSort)
                                                                fp.RegisterRelation(regRels.[relDecl.Name])
                                                           | _ -> () // ? TODO
                                             )

            let freshVar= ref (uint32(0))
            
            // TODO problem with varDefs, they are being kept from rule to rule
            // therefore if a rule uses variables with the same name that denote distinct types, it will eventually clash.
            // variables are NOT global to the program but just local to the rule
            // TODO make it so
            let purifiedRules= program.Rules |> Seq.toList |> List.mapi  (fun i rule -> _infonSimplifier.renameVars(rule, i))
            let purifiedQueries= program.Queries |> Seq.toList |> List.mapi  (fun i rule -> _infonSimplifier.renameVars(rule, i + purifiedRules.Length))

            let varDefs= purifiedRules
                         |> Seq.append purifiedQueries
                         |> Seq.collect (fun rulepart -> match rulepart with
                                                         | RulePart(AtomRule(relation)) -> seq {yield relation}
                                                         | RulePart(ImpliesRule(head, body)) -> seq {yield! head::body}
                                                         | _ -> seq {yield! []}
                                        )
                         |> Seq.collect (fun rel -> let dom= regRels.[rel.Name].Domain
                                                    let signature= datalogTranslator.RelationSignatures.[rel.Name]
                                                    rel.Args |> Seq.mapi (fun i arg -> (arg, signature.[i], dom.[i]))
                                        )
                         |> Seq.fold (fun (acc:Dictionary<string,Expr>) arg -> match arg with
                                                                               | (VarTerm(s), typ, sort) -> let v= _context.MkBound(!freshVar, sort)
                                                                                                            if typ.IsSome then
                                                                                                                let dkalType= dkalTypeFromString(typ.Value)
                                                                                                                regVarNames.[v] <- (s, dkalType)
                                                                                                            // vars without type are nonfunctional ones, but we still need them
                                                                                                            acc.[s] <- v
                                                                                                            freshVar := !freshVar + uint32(1)
                                                                                                            acc

                                                                               | _ -> acc
                                     ) (new Dictionary<string,Expr>())
            purifiedRules |> Seq.append purifiedQueries
                          |> Seq.iter ( fun rp -> match rp with
                                                  | RulePart(AtomRule(relationRule)) -> 
                                                        let rule= _infonSimplifier.relationToBoolExpr(relationRule, regRels, regConsts, varDefs, _context)
                                                        fp.AddRule(rule)
                                                  | RulePart(ImpliesRule(head, body)) ->
                                                        let rule= _infonSimplifier.impliesRuleToBoolExpr(head, body, regRels, regConsts, varDefs, _context)
                                                        fp.AddRule(rule)
                                                  | _ -> ()
                                      )

            // TODO this doesn't really work with more than one query. Fix this, we need to make one unique encompassing query.
            (*
            let sat= program.Queries |> Seq.fold (fun res query -> match res with
                                                                   | Status.SATISFIABLE -> match query with
                                                                                           | RulePart(ImpliesRule(head, body)) ->
                                                                                                let q= relationToBoolExpr(head, regRels, regConsts, regVars, varDefs, _context)
                                                                                                let res= fp.Query(q)
                                                                                                res
                                                                                           | _ -> failwith "Error!"
                                                                   | _ as x -> x
                                                 ) Status.SATISFIABLE
            *)
            let andQueries= purifiedQueries |> Seq.collect (fun query -> match query with
                                                                         | RulePart(ImpliesRule(head,body)) -> [head]
                                                                         | _ -> failwith "Error - queries should be encoded as ImpliesRule"
                                                           )
                                            |> Seq.map (fun query -> _infonSimplifier.relationToBoolExpr(query, regRels, regConsts, varDefs, _context))
            let query= _context.MkAnd(andQueries |> Seq.toArray)
            // TODO remember that FakeTrue is EmptyInfon!
            let translations= datalogTranslator.TranslatedRelations
            let sat= fp.Query(query)
            if sat = Status.SATISFIABLE then
                let derivation= fp.GetAnswer()
                // TODO: CHECK THIS the answer seems to be an AND expression (or a single EQ expression, being equivalent to an AND of a single expression)
                // the last argument of this AND expression is an EQ expression equalling a spurious variable with the derivation itself
                // the derivation is an AST of the derivation process, intermediate nodes are intermediate derivations and the leaves are facts
                // so for example a node having k children is interpreted as n1 & n2 & ... & nk -> node
                // The explanation is an EQ expression for which we need the second term
                let (actualAnswer, explanation) = if (derivation.IsAnd && derivation.Args.Length = 1) || derivation.IsEq then
                                                    // it is only the derivation
                                                    _context.MkTrue(), if derivation.IsAnd then derivation.Args.[0] else derivation
                                                  else
                                                    let andArgs= Array.sub derivation.Args 0 (derivation.Args.Length - 1) |> Array.map (fun x -> x :?> BoolExpr)
                                                    _context.MkAnd(andArgs), derivation.Args.[derivation.Args.Length - 1]
                let explanation= se.purifyExplanation explanation
                let answer= _infonSimplifier.remapVariables (actualAnswer, query, _context)
                let possibleSubsts= [subst] |> Seq.collect (fun solvedSub -> se.mergeSubstitutionWithAnswer solvedSub answer regVarNames invRegConsts datalogTranslator.ConstantsMapping.Values)
                
                // TODO check if we need to build a justification for this infon

                let evidence, evVar = match target with
                                      | JustifiedInfon(inf, ev) -> Some (se.buildEvidence explanation translations query invRegConsts), Some ev
                                      | _ -> None, None

                let nonEvidentialSubsts= possibleSubsts |> Seq.collect (fun possibleSub -> SubstrateDispatcher.Solve (se.substrateQueries (originalTarget.Normalize())) [possibleSub])
                if evidence.IsNone then
                    yield! nonEvidentialSubsts
                else
                    if ((se :> ILogicEngine).CheckJustification evidence.Value).IsSome then
                        yield! nonEvidentialSubsts |> Seq.map (fun sub -> sub.Extend(evVar.Value :?> IVar, evidence.Value))
                    else
                        yield! []
      }

    member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
        failwith "Not implemented"
