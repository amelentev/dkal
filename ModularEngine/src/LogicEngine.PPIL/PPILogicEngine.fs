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

/// solverFun: hypotheses -> queries -> evidences for queries
type PPILogicEngine(solverFun : ITerm list -> ITerm list -> ITerm option list) =
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

    let rec extractForAllTerms = function
      | Forall(var, t) ->
        let t, vars = extractForAllTerms t
        t, (var :> ITerm) :: vars
      | u -> u, []

    let ieq (i1: ITerm) i2 = i1.Unify i2 = Some Substitution.Id

    let rec removeCommonPrefix pref args =
      match List.head args with
      | SaidInfon(phd,ihd) ->
        let next = args.Tail |> List.collect (function
            | SaidInfon(p,i) when ieq phd p -> [i]
            | _ -> [])
        if next.Length+1 = args.Length then
          removeCommonPrefix (phd::pref) (ihd::next)
        else
          args,pref
      | _ -> args,pref

    let rec addPrefix i = function
      | ppal :: tail -> addPrefix (SaidInfon(ppal, i)) tail
      | [] -> i

    interface ILogicEngine with
      member this.Derive (target: ITerm) (substs: ISubstitution seq) = 
        log.Debug("Derive {0}", target)
        let target = target.Normalize()
        seq { for subst in substs do      
                for (subst, conds) in this.DoDerive subst target do
                  yield! SubstrateDispatcher.Solve conds [subst] }

      member this.CheckJustification (evidence: ITerm) =
        let le = (this :> ILogicEngine)
        let check = le.CheckJustification
        let checkAndRemovePrefix args extra = 
          let ca = args |> List.map check |> List.collect Option.toList
          if ca.Length=args.Length then
            Some(removeCommonPrefix [] (ca @ extra))
          else None
        match evidence with
        | SignatureEvidence(PrincipalConstant(ppal), inf, SubstrateConstant(signature)) when signature.GetType() = typeof<int> ->
          if this.CanSign ppal inf && le.SignatureProvider.CheckSignature inf ppal (signature :?> int) then
            Some inf
          else
            log.Warn("Spoofed signature {0} from {1} on {2}", signature, ppal, inf)
            None
        | ModusPonensEvidence(e1, e2) ->
          match checkAndRemovePrefix [e1; e2] [] with
          | Some([i1; ImpliesInfon(i1', i2)], pref) when ieq i1 i1' ->
            Some (addPrefix i2 pref)
          | _ ->
            log.Warn("Malformed modus ponens proof on {0} and {1}", e1, e2)
            None
        | ImplicationIntroductionEvidence(concl, res) ->
          match checkAndRemovePrefix [concl] [res] with
          | Some([concl; ImpliesInfon(premise, concl')], _) when ieq concl' concl -> 
            Some res
          | _ ->
            log.Warn("Malformed implication introduction proof from {0} to {1}", concl, res)
            None
        | AndEvidence(evidences) ->
          match checkAndRemovePrefix evidences [] with
          | Some (args, pref) ->
            Some(addPrefix (AndInfon args) pref)
          | _ ->
            log.Warn("Malformed conjunction proof on {0}", evidence)
            None
        | AndEliminationEvidence(conj, res) ->
          match checkAndRemovePrefix [conj] [res] with          
          | Some([AndInfon args; x], _) when args |> List.exists (ieq x) ->
            Some res
          | _ ->
            log.Warn("Malformed conjunction elimination proof from {0} to {1}", conj, res)
            None
        | OrIntroductionEvidence(disj, res) ->
          match checkAndRemovePrefix [disj] [res] with
          | Some([disj; OrInfon(args)], pref) when args |> List.exists (ieq disj) ->
            Some res
          | _ ->
            log.Warn("Malformed disjunction introduction proof from {0} to {1}", disj, res)
            None
        | AsInfonEvidence(query) ->
          if SubstrateDispatcher.Solve [query] [Substitution.Id] |> Seq.isEmpty |> not then
            Some <| AsInfon(query)
          else
            log.Warn("Non-true asInfon evidence at {0}", query)
            None
        | ConcretizationEvidence(ev, subst) ->
          match le.CheckJustification ev with
          | Some generalProof -> 
            let concreteProof = match generalProof with
                                | :? ForallTerm as ft -> ft.Instantiate subst
                                | _ -> generalProof.Apply subst
            Some concreteProof
          | None -> None
        | _ -> 
          log.Warn("Unhandled evidence type at {0}", evidence)
          None

    member private this.DoDerive (subst: ISubstitution) (query:ITerm) =
        let H = (this :> ILogicEngine).Infostrate.Knowledge |> Seq.map (fun x -> x.Apply subst)
        let query = query.Apply subst

        let query,forallvars = extractForAllTerms query
        let query,conds1 = extractSubstrateTerms query
        let query,justifiedInfons = extractJustifiedInfons query
        let queries = query :: (justifiedInfons |> List.map fst)
        let H,conds2 = H |> Seq.map extractSubstrateTerms |> List.ofSeq |> List.unzip
        let conds = conds1 @ (conds2 |> List.concat)

        let rec collectConstants = function
            | App(f, args) ->
                args |> Seq.collect collectConstants
            | Const(c) ->
                Seq.singleton (c:>ITerm)
            | _ -> Seq.empty
        let uniq ss =
            let hs = HashSet()
            for s in ss do
                hs.Add(s) |> ignore
            hs |> List.ofSeq

        let all = Seq.append queries H
        let forallvars = HashSet(forallvars)
        let vars = uniq (all |> Seq.collect (fun x -> x.Vars) |> Seq.filter (fun x -> not(subst.DomainContains x) && not(forallvars.Contains x)))
        let consts = uniq (Seq.append forallvars (all |> Seq.collect collectConstants))
        let consts = consts |> List.collect (function // TODO: type inheritance?
                                            | Collection([]) as ec ->
                                              consts |> List.map (fun c -> c.Type) |> List.filter (fun t -> t :? Type.CollectionType) |> List.map (fun t -> t, ec) // empty collection has any collection type
                                            | _ as c -> [c.Type, c])
        let consts = dict (consts |> Seq.groupBy fst |> Seq.map (fun x -> fst x, snd x |> Seq.map snd))

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
              let subst = (List.zip res evars) |> List.fold (fun subst (evid,evar) ->
                match subst with
                | Some(subst) -> 
                  evar.UnifyFrom subst evid
                | _ -> None) (Some subst)
              if subst.IsSome then
                yield subst.Value,conds
        }

type BPILogicEngine() =
  inherit PPILogicEngine(PPILSolver.solveBPIL)

type SPILogicEngine() = 
  inherit PPILogicEngine(PPILSolver.solveSPILhash)

type TPILogicEngine() =
  inherit PPILogicEngine(PPILSolver.solveTPIL)

type TSPILogicEngine() =
  inherit PPILogicEngine(PPILSolver.solveTSPIL_DS)

type TSPIL2LogicEngine() =
  inherit PPILogicEngine(PPILSolver.solveTSPIL2)