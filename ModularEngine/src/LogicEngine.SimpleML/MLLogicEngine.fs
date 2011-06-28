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

namespace Microsoft.Research.Dkal.LogicEngine.ML

open System.Collections.Generic

open Microsoft.Research.Dkal.Substrate // for SubstrateDispatcher
open Microsoft.Research.Dkal.Interfaces // for ISignatureProvider and IInfostrate
open MLType

module MLLogicEngineImpl =
  let log = ref ""
  let _freshVarId = ref 0
  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None

  let rec finalOutcome (infon: term) =
    match infon with
    | App(ImpliesInfon, [_; i]) -> finalOutcome i
    //| App(ImpliesInfon, _) -> failwith "Wrong number of arguments for ImpliesInfon"
    | Forall(_,t) -> finalOutcome t
    | i -> i

  let canSign (principal: principal) (infon: term) =
    match finalOutcome infon with
    | App(SaidInfon, [Const(PrincipalConstant(p)); _]) -> principal = p
    | _ -> false

  let rec checkJustification (evidence: term) =
    match evidence with
    | App(SignatureEvidence, [Const(PrincipalConstant(ppal)); inf;
                              Const(SubstrateConstant(signature))]) ->
      if canSign ppal inf &&
         Utilities.value_checkSignature _signatureProvider inf ppal signature
      then
        Some inf
      else
        //log.Warn("Spoofed signature {0} from {1} on {2}", signature, ppal, inf)
        None      
    | App(ModusPonensEvidence, [e1; e2]) ->
      match checkJustification e1, checkJustification e2 with
      | Some i1, Some (App(ImpliesInfon, [i1'; i2])) when i1 = i1' ->
        Some i2
      | _ ->
        //log.Warn("Malformed modus ponens proof on {0} and {1}", e1, e2)
        None  
    | App(AndEvidence, evidences) ->
        let infons = List.collect (fun evidence ->  
                                    match checkJustification evidence with
                                    | Some i -> [i]
                                    | None -> []) evidences
        if List.length infons = List.length evidences then
          Some <| App(AndInfon, infons)
        else
          //log.Warn("Malformed conjunction proof on {0}", evidence)
          None
    | App(AsInfonEvidence, [SubstrateQueryTerm(query)]) ->
      if Utilities.substrateDispatcher_solve [query] [Subst.id] |> 
           List.isEmpty |> not
      then
        Some <| App(AsInfon, [SubstrateQueryTerm(query)])
      else
        //log.Warn("Non-true asInfon evidence at {0}", query)
        None    
    | ConcretizationEvidence(ev, subst) ->
      match checkJustification ev with
      | Some generalProof -> 
        let concreteProof = match generalProof with
                            | Forall _ -> Term.instantiate generalProof subst
                            | _ -> Term.apply generalProof subst
        Some concreteProof
      | None -> None
    | _ -> 
      //log.Warn("Unhandled evidence type at {0}", evidence)
      None

  let freshVar (t: typ) =
    let ret = {
      name = "SimpleLogicEngine#Var" + Utilities.int_to_string !_freshVarId;
      typ = t}
    _freshVarId := !_freshVarId + 1
    ret

  let rec tryDeriveJustification ((subst, conds): substitution * substrateQueryTerm list)
                             (pr: term) ((goal, inf): term * term) =
      let straight (goal: term, premise: term) = 
        match Term.unifyFrom premise subst goal with
          | Some s -> [(s, conds, Term.apply pr s)]
          | None -> []
      match inf with
      | Forall(v, t) as ft ->
        let i, pr = 
          if Subst.domainContains subst v then
            let newFt, s' = Term.changeVarName ft subst
            t, Term.apply pr s'
          else
            t, pr
        tryDeriveJustification (subst, conds) pr (goal, i)
      | App(ImpliesInfon, [i1; i2]) ->
        let v = freshVar Evidence
        let derivePremise (subst, conds, (goalPr:term)) =
          let updateProof (subst, conds, premisePr) =
            let repl = App(ModusPonensEvidence, [premisePr; pr]) // TODO: see Builders.fs and Primitives.fs
            (subst, conds, Term.apply goalPr <| Subst.extend Subst.id (v, repl))
          let tmp = doDeriveJustification i1 (subst, conds)
          tmp |> List.map updateProof
        (straight (goal, inf)) @ 
        (List.collect derivePremise 
          (tryDeriveJustification (subst, conds) (Var v) (goal, i2)))
      | inf -> straight (goal, inf)
      
  and doDeriveJustification (infon: term) ((subst, conds): substitution * substrateQueryTerm list) =
    let straight goal =
      let aux infon = 
        match infon with
        | App(JustifiedInfon, [inf; pr]) -> 
            tryDeriveJustification (subst, conds) pr (goal, inf)
        | _ -> []
      List.collect aux (Utilities.value_knowledge _infostrate ())
    match infon with
    | App(AsInfon, [ SubstrateQueryTerm(exp) ]) as goal ->
      [subst, conds @ [ exp ], App(AsInfonEvidence, [ SubstrateQueryTerm(exp) ])]
    | App(AndInfon, infons) as goal ->
      let parts = 
        List.fold 
          (fun res infon ->
            List.foldBack
              (fun (subst, conds, evLeft) res2 -> // TODO: fix the performance decrease
                (List.foldBack
                  (fun (subst, conds, evRight) res3 ->
                    (subst, conds, 
                     App(AndEvidence, [evLeft; evRight])) :: res3)
                  (doDeriveJustification infon (subst, conds)) []
                ) @ res2) res [])
          ([(subst, conds, App(EmptyEvidence, []))]) 
          infons
      (straight goal) @ parts
    | goal -> straight goal

  let infonsWithPrefix (subst: substitution) (pref: term list) (template: term) =
    let res = ref []
    let rec stripPrefix subst prefixUnif preconds suff = 
      let immediate = function
        | ([], i) ->
          let rec unifyAndSimpl (s: substitution option) (ts: (term * term) list) = 
            match ts with
            | [] -> s
            | (a, b) :: ts ->
              match s with
                | None -> None
                | Some s -> unifyAndSimpl (Term.unifyFrom a s b) ts
          match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
            | Some subst ->
              res := (subst, preconds) :: !res
            | None -> ()
        | _ -> ()
           
      function
      | ((t1: term) :: pref, App(SaidInfon, [t2; i])) ->
        match Term.unifyFrom t1 subst t2 with
          | Some subst -> 
            stripPrefix subst prefixUnif preconds 
              (fun i -> suff (App(SaidInfon, [t2; i]))) (pref, i)
          | None ->
            stripPrefix subst ((t1, t2) :: prefixUnif)
              preconds (fun i -> suff (App(SaidInfon, [t2; i]))) (pref, i)
      | (pref, App(AndInfon, infons)) ->
        List.iter (fun infon -> 
                    stripPrefix subst prefixUnif preconds suff (pref, infon)) infons
      | (pref, App(ImpliesInfon, [a; b])) as t ->
        immediate t
        stripPrefix subst prefixUnif (suff a :: preconds) suff (pref, b)
      | (pref, Var v) when Subst.domainContains subst v ->
        stripPrefix subst prefixUnif preconds suff (pref, Subst.apply subst v)
      | t -> immediate t
  
    List.iter (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, k)) 
      (Utilities.value_knowledge _infostrate ())
    !res

  let rec doDerive (pref: term list) 
               ((subst, conds): substitution * substrateQueryTerm list)
               (infon: term) = 
      match infon with
      | App(AndInfon, infons) -> 
        List.fold (fun substs infon ->
                   List.collect 
                     (fun s -> doDerive pref s infon) substs
                  ) [(subst, conds)] infons
      | App(EmptyInfon, [])-> 
        [(subst, conds)]
      | App(SaidInfon, [ppal; infon]) ->
        doDerive (ppal :: pref) (subst, conds) infon
      | Var(v) when Subst.domainContains subst v ->
        doDerive pref (subst, conds) (Subst.apply subst v)
      | App(AsInfon, [SubstrateQueryTerm(exp)]) ->
        if List.isEmpty pref then
          [(subst, conds @ [ exp ])]
        else
          failwith "asInfon(...) under prefix"
      | App(JustifiedInfon, [inf; ev]) when pref = [] ->
        let unifyEv (subst', conds', pr') =
          match Term.unifyFrom ev subst' pr' with
            | Some s -> [(s, conds @ conds')]
            | None -> []
        doDeriveJustification inf (subst, conds) |> List.collect unifyEv
      | templ ->
        let rec checkOne = function
            | (substConds, pre :: pres) -> 
              doDerive [] substConds pre |> List.collect (fun s -> checkOne (s, pres))
            | (substConds, []) ->
              [substConds]
        infonsWithPrefix subst pref templ 
          |> List.map (fun (s, ps) -> ((s, conds), ps))
          |> List.collect checkOne

  let derive (target: term) (substs: substitution list) : substitution list =
    List.foldBack
      (fun subst res ->
        (List.foldBack
          (fun (subst, conds) res2 ->
            (Utilities.substrateDispatcher_solve conds [subst]) @ res2)
          (doDerive [] (subst, []) (target)) []
        ) @ res)
      substs []

  let deriveJustification (infon: term) (proofTemplate: term)
                          (substs: substitution list) : substitution list =
    List.foldBack 
      (fun subst res -> 
        (List.foldBack 
          (fun (subst, conds, proof) res2 ->
            (match Term.unifyFrom proof subst proofTemplate with
             | Some subst -> Utilities.substrateDispatcher_solve conds [subst]
             | None -> []
            ) @ res2)
          (doDeriveJustification infon (subst, [])) []
        ) @ res)
      substs []