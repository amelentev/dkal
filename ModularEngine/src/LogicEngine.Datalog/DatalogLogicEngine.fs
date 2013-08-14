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

/// The SimpleEngine uses backwards propagation to derive all possible 
/// Substitutions that will satisfy the given query. Each Substitution will 
/// have an accompanying list of side conditions to be checked against the 
/// substrate(s). Only those Substitutions that pass the side conditions are
/// returned
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
            let regConsts= new Dictionary<Expr, uint32>()
            let fresh= ref (uint32(0))
            program.Declarations |> Seq.iter ( fun decl -> match decl with
                                                           | SortDeclarationPart(sortDecl) ->
                                                                let sort= _context.MkUninterpretedSort(sortDecl.Name)
                                                                try 
                                                                    program.Sorts.[sortDecl.Name].Values
                                                                        |> Seq.iter ( fun value -> 
                                                                                        let cst= _context.MkConst(value.ToString(), sort)
                                                                                        regConsts.[cst] <- !fresh
                                                                                        fresh := !fresh + uint32(1)
                                                                                    )
                                                                with e -> ()
                                                           | _ -> ()
                                             )
            program.Declarations |> Seq.iter ( fun decl -> match decl with
                                                           | RelationDeclarationPart(relDecl) -> 
                                                                let argsSorts= relDecl.Args
                                                                                 |> Seq.collect (fun arg -> seq {yield _context.MkUninterpretedSort(snd arg) :> Microsoft.Z3.Sort})
                                                                                 |> Seq.toArray
                                                                regRels.[relDecl.Name] <- _context.MkFuncDecl(relDecl.Name, argsSorts, _context.MkBoolSort())
                                                                fp.RegisterRelation(regRels.[relDecl.Name])
                                                           | _ -> () // ? TODO
                                             )

            let fakeTrueFunc= _context.MkFuncDecl(FAKE_TRUE_RELATION, [||], _context.MkBoolSort())
            fp.AddFact(fakeTrueFunc, [||])
            program.Rules |> Seq.iter ( fun rp -> match rp with
                                                  | RulePart(AtomRule(relationRule)) -> 
                                                        let (func, args) = relationRuleToFact(relationRule, regRels, regConsts, _context)
                                                        fp.AddFact(func, args)
                                                  | RulePart(ImpliesRule(head, body)) -> fp.AddRule( impliesRuleToFact(head, body, regRels, regConsts, _context) )
                                                  | _ -> ()
                                      )
            let sat= program.Queries |> Seq.fold ( fun res query -> match res with
                                                                    | Status.UNSATISFIABLE -> fp.Query( rulePartToBoolExpr(query, regRels, regConsts, _context) )
                                                                    | _ as x -> x
                                                 ) Status.UNSATISFIABLE
            if sat = Status.UNSATISFIABLE then
                yield subst
      }

    member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
        failwith "Not implemented"

    member se.CheckJustification (evidence: ITerm) = 
        failwith "Not implemented"


