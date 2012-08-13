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

namespace Microsoft.Research.Dkal.LogicEngine.PPIL

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.LogicEngine.Simple
open Microsoft.Research.Dkal.LogicEngine.PPIL.AST
open System.Collections.Generic
open NLog

type PPILogicEngine(solverFun : ITerm list -> ITerm list -> AST.Proof option list) =
    inherit SimpleLogicEngine()
    let log = LogManager.GetLogger("LogicEngine.PPIL")

    let rec extractSubstrateTerms = function
      | AsInfon(q) ->
        EmptyInfon, [q]
      | App(f, args) ->
        let args,qs = args |> List.map extractSubstrateTerms |> List.unzip
        App(f, args), qs |> List.concat
      | n -> n, []

    let rec extractJustifiedInfons = function
      | JustifiedInfon(i,e) as ji ->
        EmptyInfon, [(i,e)]
      | App(f, args) ->
        let args,qs = args |> List.map extractJustifiedInfons |> List.unzip
        App(f, args), qs |> List.concat
      | n -> n, []

    interface ILogicEngine with
      member se.Derive (target: ITerm) (substs: ISubstitution seq) = 
        log.Debug("Derive {0}", target)
        let target = target.Normalize()
        seq { for subst in substs do      
                for (subst, conds) in se.DoDerive subst target do
                  yield! SubstrateDispatcher.Solve conds [subst] }

    member private this.DoDerive (subst: ISubstitution) (query:ITerm) =
        let H = (this :> ILogicEngine).Infostrate.Knowledge |> Seq.map (fun x -> x.Apply subst)
        let query = query.Apply subst

        let query,conds1 = extractSubstrateTerms query
        let query,justifiedInfons = extractJustifiedInfons query
        let queries = query :: (justifiedInfons |> List.map fst)
        let H,conds2 = H |> Seq.map extractSubstrateTerms |> List.ofSeq |> List.unzip
        let conds = conds1 @ (conds2 |> List.concat)

        let rec collectConstants = function
            | App(f, args) ->
                args |> Seq.collect collectConstants
            | Const(c) ->
                Seq.singleton c
            | _ -> Seq.empty
        let uniq ss =
            let hs = HashSet()
            for s in ss do
                hs.Add(s) |> ignore
            hs |> List.ofSeq

        let all = Seq.append queries H
        let vars = uniq (all |> Seq.collect (fun x -> x.Vars) |> Seq.filter (fun x -> not(subst.DomainContains x)))
        let consts = dict (uniq (all |> Seq.collect collectConstants) |> Seq.groupBy (fun x -> x.Type))

        let rec substitutions (vars:IVar list) (subst:ISubstitution) =
            if vars=[] then 
                Seq.singleton subst
            else
                let var = vars.Head
                let ok,cs = consts.TryGetValue var.Type
                if ok then
                  seq { for c in cs do
                          yield! substitutions vars.Tail (subst.Extend(var, c)) }
                else
                  seq []
        let substs = substitutions vars subst

        seq {
          for subst in substs do
            let H = H |> List.map (fun x -> x.Apply(subst))
            let Q = queries |> List.map (fun q -> q.Apply(subst))
            let res = solverFun H Q
            if res |> List.forall Option.isSome then
              let res = res.Tail |> List.map Option.get
              let evars = justifiedInfons |> List.map snd
              let subst = (List.zip res evars) |> List.fold (fun subst (proof,evar) ->
                match subst with
                | Some(subst) -> 
                  let evid = this.constructEvidence proof
                  evar.UnifyFrom subst evid
                | _ -> None) (Some subst)
              if subst.IsSome then
                yield subst.Value,conds
        }

    member private this.constructEvidence = function
      | Hypothesis(ast) ->
        match ast.Common.orig with
        | JustifiedInfon(_, e) -> e
        | _ -> EmptyEvidence
      | ConjIntroduction(proofs,_) -> AndEvidence(proofs |> List.map this.constructEvidence)
      | ImplicationElimination(left,right,_) -> ModusPonensEvidence(this.constructEvidence left, this.constructEvidence right)
      | _ -> EmptyEvidence // TODO:
