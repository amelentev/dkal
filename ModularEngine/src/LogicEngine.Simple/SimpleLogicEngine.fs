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

namespace Microsoft.Research.Dkal.LogicEngine.Simple

open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

/// The SimpleEngine uses backwards propagation to derive all possible 
/// Substitutions that will satisfy the given query. Each Substitution will 
/// have an accompanying list of side conditions to be checked against the 
/// substrate(s). Only those Substitutions that pass the side conditions are
/// returned
type SimpleLogicEngine() = 

  let log = LogManager.GetLogger("LogicEngine.Simple")

  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let mutable _freshVarId = 0

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
    /// MetaTerms). Then return only those Substitutions that satisfy all their 
    /// side conditions.
    member se.Derive (target: ITerm) (substs: ISubstitution seq) = 
      seq { for subst in substs do      
              for (subst, conds) in se.DoDerive [] (subst, seq []) (target.Normalize()) do
                yield! SubstrateDispatcher.Solve conds [subst] }

    member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
      seq { for subst in substs do
              for (subst, conds, proof) in se.DoDeriveJustification infon (subst, seq []) do
                match proof.UnifyFrom subst proofTemplate with
                | Some subst -> yield! SubstrateDispatcher.Solve conds [subst]
                | None -> () }

    member se.CheckJustification (evidence: ITerm) = 
      match evidence with
      | SignatureEvidence(PrincipalConstant(ppal), inf, SubstrateConstant(signature)) when signature.GetType() = typeof<int> -> 
        if se.CanSign ppal inf && _signatureProvider.Value.CheckSignature inf ppal (signature :?> int) then
          Some inf
        else
          log.Warn("Spoofed signature {0} from {1} on {2}", signature, ppal, inf)
          None
      | ModusPonensEvidence(e1, e2) ->
        match (se :> ILogicEngine).CheckJustification e1, (se :> ILogicEngine).CheckJustification e2 with
        | Some i1, Some (ImpliesInfon(i1', i2)) when i1 = i1' ->
          Some i2
        | _ ->
          log.Warn("Malformed modus ponens proof on {0} and {1}", e1, e2)
          None
      | AndEvidence(evidences) ->
        let infons = List.collect (fun evidence ->  match (se :> ILogicEngine).CheckJustification evidence with
                                                    | Some i -> [i]
                                                    | None -> []) evidences
        if infons.Length = evidences.Length then
          Some <| AndInfon(infons)
        else
          log.Warn("Malformed conjunction proof on {0}", evidence)
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

  member private see.FinalOutcome (infon: ITerm) =
    match infon with
    | ImpliesInfon (_, i) -> see.FinalOutcome i
    | Forall(_,t) -> see.FinalOutcome t
    | i -> i

  member private see.CanSign (principal: string) (infon: ITerm) =
    match see.FinalOutcome infon with
    | SaidInfon (PrincipalConstant(p), _) -> principal = p
    | _ -> false
  
  /// Given a prefix (list of principal MetaTerms) a current Substitution with 
  /// side conditions (AsInfo MetaTerms) and a target infon MetaTerm to derive
  /// this method will recursively derive the target infon depending on its
  /// structure.
  member private se.DoDerive (pref: ITerm list) ((subst, conds): ISubstitution * ISubstrateQueryTerm seq) (infon: ITerm) = 
    match infon with
    | AndInfon(infons) -> 
      // In the case of conjunction we start with the current substitution and side conditions and 
      // continue accumulating these by calling recursively on each of the infons in the conjunction
      Seq.fold (fun substs infon -> Seq.collect (fun s -> se.DoDerive pref s infon) substs) (seq [(subst, conds)]) infons
    | EmptyInfon -> 
      // Empty infon is always satisfiable by the current substitution and side conditions
      seq [(subst, conds)]
    | SaidInfon(ppal, infon) ->
      // Said infons are handled recursively by pushing the principal term into the prefix
      se.DoDerive (ppal :: pref) (subst, conds) infon
    | Var(v) when subst.DomainContains v ->
      // If a variable is part of the current substitution it is applied and we call recursively
      se.DoDerive pref (subst, conds) (subst.Apply v)
    | AsInfon(exp) ->
      // AsInfon(...) is stored as a new side condition, unless it is inside a non-empty prefix
      if pref.IsEmpty then
        seq [(subst, seq {yield! conds; yield exp})]
      else
        failwith "asInfon(...) under prefix"
    | JustifiedInfon(inf, ev) when pref = [] ->
      // Justified infons are treated separately since we need to construct proofs for these
      let unifyEv (subst', conds', pr') =
        match ev.UnifyFrom subst' pr' with
          | Some s -> seq [(s, seq {yield! conds; yield! conds'})]
          | None -> seq []
      se.DoDeriveJustification inf (subst, conds) |> Seq.collect unifyEv
    | templ ->
      // For every other case we call se.InfonsWithPrefix(..) which will give us a list of
      // substitutions, each of which will have a list of infon MetaTerms (preconditions) that 
      // need to be  satisfied in order for that substitution to be returned. This is were the 
      // backwards chaining happens, since we recursively check all the preconditions one by one 
      // (with checkOne)
      let rec checkOne = function
          | (substConds, pre :: pres) -> 
            se.DoDerive [] substConds pre |> Seq.collect (fun s -> checkOne (s, pres))
          | (substConds, []) ->
            seq [substConds]
      se.InfonsWithPrefix subst pref templ 
        |> Seq.map (fun (s, ps) -> ((s, conds), ps))
        |> Seq.collect checkOne

  /// Given a current Substitution, a prefix (list of principal MetaTerms) and 
  /// a template infon MetaTerm to derive this method will return a list of 
  /// Substitutions that satisfy the given template, each of which will have a
  /// list of preconditions (infon MetaTerms) that need to be verified in order
  /// for that Substitution to be a real solution
  member se.InfonsWithPrefix (subst: ISubstitution) (pref: ITerm list) (template: ITerm) =
    let res = ref []
    let rec stripPrefix subst prefixUnif preconds suff = 
      let immediate = function
        | ([], i) ->
          let rec unifyAndSimpl (s: ISubstitution option) (ts: (ITerm * ITerm) list) = 
            match ts with
            | [] -> s
            | (a, b) :: ts ->
              match s with
                | None -> None
                | Some s -> unifyAndSimpl (a.UnifyFrom s b) ts
          match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
            | Some subst ->
              res := (subst, preconds) :: !res
            | None -> ()
        | _ -> ()
             
      function
      | ((t1: ITerm) :: pref, SaidInfon (t2, i)) ->
        match t1.UnifyFrom subst t2 with
          | Some subst -> 
            stripPrefix subst prefixUnif preconds (fun i -> suff (SaidInfon (t2, i))) (pref, i)
          | None ->
            stripPrefix subst ((t1, t2) :: prefixUnif) preconds (fun i -> suff (SaidInfon (t2, i))) (pref, i)
      | (pref, AndInfon(infons)) ->
        List.iter (fun infon -> 
                    stripPrefix subst prefixUnif preconds suff (pref, infon)) infons
      | (pref, ImpliesInfon(a, b)) as t ->
        immediate t
        stripPrefix subst prefixUnif (suff a :: preconds) suff (pref, b)
      | (pref, Var v) when subst.DomainContains v ->
        stripPrefix subst prefixUnif preconds suff (pref, subst.Apply v)
      | t -> immediate t
    
    List.iter (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, k)) (_infostrate.Value.Knowledge |> Seq.toList)
    !res

  member se.DoDeriveJustification (infon: ITerm) ((subst, conds): ISubstitution * ISubstrateQueryTerm seq) =
    let straight goal =
      let aux infon = 
        match infon with
        | JustifiedInfon(inf, pr) -> se.TryDeriveJustification (subst, conds) pr (goal, inf)
        | _ -> seq []
      Seq.collect aux _infostrate.Value.Knowledge
    match infon with
    | AsInfon(exp) as goal ->
      seq [(subst, seq {yield! conds; yield exp}, AsInfonEvidence(exp))]
    | AndInfon(infons) as goal ->
      let parts = List.fold 
                    (fun res infon -> 
                      seq { for (subst, conds, evLeft) in res do
                              for (subst, conds, evRight) in se.DoDeriveJustification infon (subst, conds) do
                                yield (subst, conds, AndEvidence([evLeft; evRight]).Normalize())}) 
                    (seq [(subst, conds, EmptyEvidence)]) 
                    infons
      seq {yield! straight goal; yield! parts}
    | goal -> straight goal

  member private se.FreshVar (t: IType) =
    let ret = {Name = "SimpleLogicEngine#Var" + _freshVarId.ToString(); Type = t}
    _freshVarId <- _freshVarId + 1
    ret

  member private se.TryDeriveJustification ((subst, conds): ISubstitution * ISubstrateQueryTerm seq) (pr: ITerm) ((goal, inf): ITerm * ITerm) =
    let straight (goal: ITerm, premise: ITerm) = 
      match premise.UnifyFrom subst goal with
        | Some s -> seq [(s, conds, pr.Apply s)]
        | None -> seq []
    match inf with
    | :? ForallTerm as ft ->
      let i, pr = 
        if subst.DomainContains ft.Var then
          let newFt, s' = ft.ChangeVarName subst 
          (newFt :?> ForallTerm).Term, pr.Apply s'
        else
          ft.Term, pr
      se.TryDeriveJustification (subst, conds) pr (goal, i)
    | ImpliesInfon(i1, i2) ->
      let v = se.FreshVar Type.Evidence
      let derivePremise (subst, conds, (goalPr:ITerm)) =
        let updateProof (subst, conds, premisePr) =
          let repl = ModusPonensEvidence(premisePr, pr)
          (subst, conds, goalPr.Apply <| Substitution.Id.Extend(v, repl))
        let tmp = se.DoDeriveJustification i1 (subst, conds)
        tmp |> Seq.map updateProof
      seq {yield! straight (goal, inf); yield! Seq.collect derivePremise (se.TryDeriveJustification (subst, conds) (Var v) (goal, i2))}
    | inf -> straight (goal, inf)
