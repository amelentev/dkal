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

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Z3
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator.Datalog
open InfonSimplifier

type DatalogLogicEngine() = 

  let log = LogManager.GetLogger("LogicEngine.Datalog")

  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let _context= new Context()

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

  member private se.mergeSubstitutionWithAnswer (subst:ISubstitution) (answer:Expr) (regVarNamesAndTypes:Dictionary<Expr, string*IType>) =
    seq {
        if answer.IsTrue then
            yield subst
        else if answer.IsEq then
            // merge subst with this assignment
            // TODO is it right to assume that substitutions are just horn clauses?
            let (lhs, rhs)= if answer.Args.[0].IsVar then answer.Args.[0],answer.Args.[1] else answer.Args.[1],answer.Args.[0]
            // TODO complete the correct value, which needs to be backtranslated from the translation chain...
            yield subst.Extend( {Name= fst(regVarNamesAndTypes.[lhs]); Type= snd(regVarNamesAndTypes.[lhs])}, Constant(0))
        else if answer.IsAnd then
            // merge subst with all merged args. Get substs for each arg and combine them all
            let subsToCombine= answer.Args |> Seq.map (fun arg -> se.mergeSubstitutionWithAnswer subst arg regVarNamesAndTypes)
            let merged = ( subsToCombine |> Seq.fold (fun acc valsForClause -> 
                                                            valsForClause |> Seq.collect (fun valuation -> acc |> Seq.map (fun sub -> valuation.ComposeWith sub))
                                                     ) (seq {yield Substitution.Id})
                         )
            yield! merged
        else if answer.IsOr then
            // create one different merge for each arg
            yield! answer.Args |> Seq.collect (fun arg -> se.mergeSubstitutionWithAnswer subst arg regVarNamesAndTypes)
        else yield! []
    }

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
      let substs = SubstrateDispatcher.Solve (se.substrateQueries target) substs

      seq {
        for subst in substs do
            let fp= _context.MkFixedpoint()
            let program= datalogTranslator.translateInferenceProblem ( (knowledge |> Seq.map (fun kn -> kn.Apply(subst))
                                                                                  |> Seq.map (fun term -> simplify(term)) |> Seq.toList,
                                                                        [simplify(target.Apply(subst))])
                                                                     )
            let regRels= new Dictionary<string, FuncDecl>()
            let regConsts= new Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>()
            let regSorts= new Dictionary<Sort, Microsoft.Z3.Sort>()
            let regVars= new Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>()
            let regVarNames=  new Dictionary<Expr, string*IType>()

            // Z3 allows finite domain sorts, BUT
            // 1) they MUST have a numerical representation
            // 2) when declaring them, you must declare its size. BUT its size is actually the max possible value + 1 (actually representing the range 0..max)
            program.Declarations |> Seq.iter ( fun decl -> match decl with
                                                           | SortDeclarationPart(sortDecl) ->
                                                                let sort= _context.MkFiniteDomainSort(sortDecl.Name, uint64(program.Sorts.[sortDecl.Name].Count + 1))
                                                                regSorts.[sortDecl.Name] <- sort
                                                                let fresh= ref (uint32(0))
                                                                try 
                                                                    regConsts.[sort] <- new Dictionary<Expr, uint32>()
                                                                    regVars.[sort] <- new Dictionary<Expr, uint32>()
                                                                    program.Sorts.[sortDecl.Name].Values
                                                                        |> Seq.iter ( fun value -> 
                                                                                        let cst= _context.MkConst(value.ToString(), sort)
                                                                                        regConsts.[sort].[cst] <- !fresh
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

            let fakeTrueFunc= _context.MkFuncDecl(FAKE_TRUE_RELATION, [||], _context.BoolSort)
            fp.AddFact(fakeTrueFunc, [||])
            let freshVar= ref (uint32(0))
            let varDefs= program.Rules
                         |> Seq.append program.Queries
                         |> Seq.collect (fun rulepart -> match rulepart with
                                                         | RulePart(AtomRule(relation)) -> seq {yield relation}
                                                         | RulePart(ImpliesRule(head, body)) -> seq {yield! head::body}
                                                         | _ -> seq {yield! []}
                                        )
                         |> Seq.collect (fun rel -> let dom= regRels.[rel.Name].Domain
                                                    rel.Args |> Seq.mapi (fun i arg -> (arg, dom.[i]))
                                        )
                         |> Seq.fold (fun (acc:Dictionary<string,Expr>) arg -> match arg with
                                                                               | (VarTerm(s), sort) -> let v= _context.MkBound(!freshVar, sort)
                                                                                                       acc.[s] <- v
                                                                                                       // TODO figure out the correct type of the variable
                                                                                                       regVarNames.[v] <- (s, Microsoft.Research.Dkal.Ast.Type.Principal)
                                                                                                       regVars.[sort].[v] <- !freshVar
                                                                                                       freshVar := !freshVar + uint32(1)
                                                                                                       acc
                                                                               | _ -> acc
                                     ) (new Dictionary<string,Expr>())
            program.Rules |> Seq.append program.Queries
                          |> Seq.iter ( fun rp -> match rp with
                                                  | RulePart(AtomRule(relationRule)) -> 
                                                        let rule= relationToBoolExpr(relationRule, regRels, regConsts, regVars, varDefs, _context)
                                                        fp.AddRule(rule)
                                                  | RulePart(ImpliesRule(head, body)) ->
                                                        let rule= impliesRuleToBoolExpr(head, body, regRels, regConsts, regVars, varDefs, _context)
                                                        fp.AddRule(rule)
                                                  | _ -> ()
                                      )
            let sat= program.Queries |> Seq.fold (fun res query -> match res with
                                                                   | Status.SATISFIABLE -> match query with
                                                                                           | RulePart(ImpliesRule(head, body)) ->
                                                                                                let q= relationToBoolExpr(head, regRels, regConsts, regVars, varDefs, _context)
                                                                                                let res= fp.Query(q)
                                                                                                res
                                                                                           | _ -> failwith "Error!"
                                                                   | _ as x -> x
                                                 ) Status.SATISFIABLE
            if sat = Status.SATISFIABLE then
                // TODO actually get the answer and build and yield the acceptable substitutions
                let answer= fp.GetAnswer()
                yield! se.mergeSubstitutionWithAnswer subst answer regVarNames
      }

    member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
        failwith "Not implemented"

    member se.CheckJustification (evidence: ITerm) = 
        failwith "Not implemented"
 