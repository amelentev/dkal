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
open System.Collections.Generic
open NLog

type PPILogicEngine(solverFun) =
    let log = LogManager.GetLogger("LogicEngine.PPIL")

    let mutable _signatureProvider: ISignatureProvider option = None
    let mutable _infostrate: IInfostrate option = None
    let mutable _freshVarId = 0

    interface ILogicEngine with
        member x.Start() = ()
        member x.Stop() = ()
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
          seq { for subst in substs do
                  for (subst, conds) in se.DoDerive subst (target.Normalize()) do
                    yield! SubstrateDispatcher.Solve conds [subst] }

        member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) = 
            // todo
            substs

        member se.CheckJustification (evidence: ITerm) = 
            // todo
            None

    member private se.DoDerive (subst: ISubstitution) (query:ITerm) =
        let H = _infostrate.Value.Knowledge |> Seq.map (fun x -> x.Apply subst)
        let query = query.Apply subst

        let rec extractSubstrateTerms = function
          | AsInfon(q) ->
            EmptyInfon, [q]
          | App(f, args) ->
            let args,qs = args |> List.map extractSubstrateTerms |> List.unzip
            App(f, args), qs |> List.concat
          | _ as n -> n, []

        let query,conds1 = extractSubstrateTerms query
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
            [ for s in hs do yield s ]

        let all = Seq.singleton query |> Seq.append H
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
                else // todo:
                  //substitutions vars.Tail subst
                  seq []
        let substs = substitutions vars subst

        seq { for subst in substs do
                let H = H |> Seq.map (fun x -> x.Apply(subst)) |> List.ofSeq
                let Q = query.Apply(subst)
                if solverFun H [Q] = [true] then
                    yield subst,conds }
