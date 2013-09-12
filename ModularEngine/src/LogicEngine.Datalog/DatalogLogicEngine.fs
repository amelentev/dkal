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
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Z3
open Microsoft.Research.DkalBackends
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator.Datalog

type DatalogLogicEngine() = 

  let log = LogManager.GetLogger("LogicEngine.Datalog")

  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let _context= new Context()
  let _infonSimplifier= new InfonSimplifier()

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

  member private se.mergeSubstitutionWithAnswer (subst:ISubstitution) (answer:Expr) (regVarNamesAndTypes:Dictionary<Expr, string*IType>)
                                                (mappedConstants: Dictionary<uint32, Microsoft.Z3.Sort*Expr>) (translatedConstants:List<Ast.Term>)=
    seq {
        if answer.IsTrue then
            yield subst
        else if answer.IsEq then
            // merge subst with this assignment
            // TODO is it right to assume that substitutions are just horn clauses?
            let (lhs, rhs)= if answer.Args.[0].IsVar then answer.Args.[0],answer.Args.[1] else answer.Args.[1],answer.Args.[0]
            // TODO complete the correct value, which needs to be backtranslated from the translation chain...
            // yield subst.Extend( {Name= fst(regVarNamesAndTypes.[lhs]); Type= snd(regVarNamesAndTypes.[lhs])}, Constant(mappedConstants.[int(rhs.ToString())]))

            // WHY AM I GETTING const = const CLAUSES??!?!?
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

    /// Obtain a list of Substitution with accompanying side conditions (AsInfon
    /// ITerms). Then return only those Substitutions that satisfy all their 
    /// side conditions.
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


      // TODO create the program before working over each substitution
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
            let regVars= new Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>()
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
                                                                    regVars.[sort] <- new Dictionary<Expr, uint32>()
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
            let varDefs= program.Rules
                         |> Seq.append program.Queries
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
                                                                                                            regVars.[sort].[v] <- !freshVar
                                                                                                            freshVar := !freshVar + uint32(1)
                                                                                                            acc

                                                                               | _ -> acc
                                     ) (new Dictionary<string,Expr>())
            program.Rules |> Seq.append program.Queries
                          |> Seq.iter ( fun rp -> match rp with
                                                  | RulePart(AtomRule(relationRule)) -> 
                                                        let rule= _infonSimplifier.relationToBoolExpr(relationRule, regRels, regConsts, regVars, varDefs, _context)
                                                        fp.AddRule(rule)
                                                  | RulePart(ImpliesRule(head, body)) ->
                                                        let rule= _infonSimplifier.impliesRuleToBoolExpr(head, body, regRels, regConsts, regVars, varDefs, _context)
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
            let andQueries= program.Queries |> Seq.collect (fun query -> match query with
                                                                         | RulePart(ImpliesRule(head,body)) -> [head]
                                                                         | _ -> failwith "Error - queries should be encoded as ImpliesRule"
                                                           )
                                            |> Seq.map (fun query -> _infonSimplifier.relationToBoolExpr(query, regRels, regConsts, regVars, varDefs, _context))
            let query= _context.MkAnd(andQueries |> Seq.toArray)
            let translations= datalogTranslator.TranslatedRelations
            let sat= fp.Query(query)
            if sat = Status.SATISFIABLE then
                let derivation= fp.GetAnswer()
                // TODO: CHECK THIS the answer seems to be an AND expression (or a single EQ expression, being equivalent to an AND of a single expression)
                // the last argument of this AND expression is an EQ expression equalling a spurious variable with the derivation itself
                // the derivation is an AST of the derivation process, intermediate nodes are intermediate derivations and the leaves are facts
                // so for example a node having k children is interpreted as n1 & n2 & ... & nk -> node
                let actualAnswer= if derivation.Args.Length = 1 || derivation.IsEq then
                                    // it is only the derivation
                                    _context.MkTrue()
                                  else
                                    let andArgs= Array.sub derivation.Args 0 (derivation.Args.Length - 1) |> Array.map (fun x -> x :?> BoolExpr)
                                    _context.MkAnd(andArgs)
                let answer= _infonSimplifier.remapVariables (actualAnswer, query, _context)
                let possibleSubsts= [subst] |> Seq.collect (fun solvedSub -> se.mergeSubstitutionWithAnswer solvedSub answer regVarNames invRegConsts datalogTranslator.ConstantsMapping.Values)
                yield! possibleSubsts |> Seq.collect (fun possibleSub -> SubstrateDispatcher.Solve (se.substrateQueries (originalTarget.Normalize())) [possibleSub])
      }

    member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
        failwith "Not implemented"

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
 