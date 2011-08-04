(*
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
*)

(* The SimpleEngine uses backwards propagation to derive all possible       *)
(* Substitutions that will satisfy the given query. Each Substitution will  *)
(* have an accompanying list of side conditions to be checked against the   *) 
(* substrate(s). Only those Substitutions that pass the side conditions are *)
(* returned                                                                 *)
module LogicEngine
  open TypeHeaders
  open Types
  open Term 

  extern reference Utilities {
    language="F#";
    dll="LogicEngine.FStar";
    namespace="Microsoft.Research.Dkal.LogicEngine.FStar";
    classname="Utilities"}
  extern Utilities val _value_knowledge : optionFS IInfostrate 
                                          -> listFS term
  extern Utilities val _value_checkSignature : optionFS ISignatureProvider
                                               -> term 
                                               -> principal
                                               -> object 
                                               -> bool
  extern Utilities val _substrateDispatcher_solve : listFS ISubstrateQueryTerm
                                                    -> listFS substitution
                                                    -> listFS substitution

  val value_knowledge : option IInfostrate 
                        -> list term
  let value_knowledge oi = PrimsListOfList (_value_knowledge (
                                              OptionOfPrimsOption oi))
  val value_checkSignature : option ISignatureProvider 
                             -> term 
                             -> principal
                             -> object 
                             -> bool
  let value_checkSignature oi t p o = _value_checkSignature 
                                        (OptionOfPrimsOption oi) t p o
  val substrateDispatcher_solve : list ISubstrateQueryTerm 
                                  -> list substitution 
                                  -> list substitution
  let substrateDispatcher_solve isl sl = 
    PrimsListOfList (_substrateDispatcher_solve 
                       (ListOfPrimsList isl) (ListOfPrimsList sl))

  let log = ref ""
  let _freshVarId = ref 0

  val finalOutcome : term -> term
  let rec finalOutcome (infon: term) =
    match infon with
    | App ImpliesInfon [_; i] -> finalOutcome i
    | Forall _ t -> finalOutcome t
    | i -> i

  let canSign (principal: principal) (infon: term) =
    match finalOutcome infon with
    | App SaidInfon [Const(PrincipalConstant(p)); _] -> principal = p
    | _ -> false
   
  (* Checks the given evidence and builds the infon that it justifies *)
  val checkJustification : option ISignatureProvider -> term -> option term
  let rec checkJustification _signatureProvider (evidence: term) = 
    match evidence with
      | App SignatureEvidence [Const(PrincipalConstant(ppal)); inf;
                               Const(SubstrateConstant(signature))] ->
          if canSign ppal inf &&
            value_checkSignature _signatureProvider inf ppal signature
          then
            Some inf
          else
            (* //log.Warn("Spoofed signature {0} from {1} on {2}", 
                          signature, ppal, inf) *)
            None      
      | App ModusPonensEvidence [e1; e2] ->
          (match checkJustification _signatureProvider e1, 
                 checkJustification _signatureProvider e2 with
             | Some i1, Some (App ImpliesInfon [i1'; i2]) when i1 = i1' ->
                 Some i2
             | _ ->
                 (* //log.Warn("Malformed modus ponens proof on {0} and {1}",
                               e1, e2) *)
                 None )
      | App AndEvidence evidences ->
          let infons = collect 
            (fun evidence ->  
               match checkJustification _signatureProvider evidence with
                 | Some i -> [i]
                 | None -> []) evidences in
            if length infons = length evidences then
              Some(App AndInfon infons)
            else
              (* //log.Warn("Malformed conjunction proof on {0}", evidence) *)
              None
      | App AsInfonEvidence [SubstrateQueryTerm query] ->
          if (not(isEmpty(substrateDispatcher_solve [query] [id]))) then
            Some(App AsInfon [(SubstrateQueryTerm query)])
          else
            (* //log.Warn("Non-true asInfon evidence at {0}", query) *)
            None
      | ConcretizationEvidence ev subst ->
          (match checkJustification _signatureProvider ev with
             | Some generalProof -> 
                 let concreteProof = 
                   match generalProof with
                     | Forall _ _ -> instantiate generalProof subst
                     | _ -> term_apply generalProof subst
                 in
                   Some concreteProof
             | None -> None)
      | _ -> (* //log.Warn("Unhandled evidence type at {0}", evidence) *)
             None

  (*open TranslationfromFStar *)
  (* Rk: Why can't I do TranslationfromFStar.optionFS on next line?? *)
  let checkJustificationWrapper (s:option ISignatureProvider) (e:term) 
    (*: optionFS term*) =
    OptionOfPrimsOption (checkJustification s e)

  val freshVar : typ -> var
  let freshVar (t: typ) =
    let name0 = (Concat "SimpleLogicEngine#Var" 
                  (string_of_any (read _freshVarId))) in
    let ret = {
      name = name0;
      typ = t} in
      (_freshVarId := (!_freshVarId) + 1);
      ret
    
  val tryDeriveJustification : 
    option IInfostrate 
    -> (substitution * list ISubstrateQueryTerm)
    -> term 
    -> (term * term)
    -> list (substitution * (list ISubstrateQueryTerm) * term)
  val doDeriveJustification : 
    option IInfostrate 
    -> term 
    -> (substitution * list ISubstrateQueryTerm) 
    -> list (substitution * (list ISubstrateQueryTerm) * term)

  (* close to stripPrefix, also matches on the infon inf *)
  let rec tryDeriveJustification 
      (_infostrate: option IInfostrate) 
      (pair_subst_conds: substitution * list ISubstrateQueryTerm)
        (* current substitution with associated conditions on the substrate *)
      (pr: term(*Evidence*))
        (* current proof *)
      (pair_goal_inf: term(*Infon*) * term(*Infon*))
        (* goal (= target = template) that we are trying to prove,            *)
        (* and infon from the infostrate that we know and try to match against *)
      : list (substitution * (list ISubstrateQueryTerm) * term(*Evidence*)) =
    let (subst, conds) = pair_subst_conds in
      let (goal, inf) = pair_goal_inf in
        (* same as immediate function of stripPrefix *)
        let straight = function 
          (* easy case: directly try to unify the premise and the goal *)
          (goal, premise) -> (match unifyFrom premise subst goal with
          (* Rk: impossible to put types of goal and premise, *)
          (* neither (goal:term, premise:term) *)
          (* nor (goal, premise) : term * term seem to work  *)                         
                                | Some s -> [(s, conds, term_apply pr s)]
                                | None -> [] ) in 
          match inf with
            | Forall v t ->
                (match
                   (* idea is just (t, pr), but this ensures there is no  *)
                   (* variable capture                                    *)
                   (if domainContains subst v 
                    then let (newFt, s') = changeVarName inf subst in
                           (t, term_apply pr s')
                    else
                      (t, pr))
                 (* then just not care about the forall v and recurse     *)
                 with (t, pr) -> tryDeriveJustification 
                                   _infostrate (subst, conds) pr (goal, t) )
            | App ImpliesInfon [i1; i2] -> 
                (* v is the evidence for i2; it will eventually be replaced *)
                let v = freshVar Evidence in 
                (* Rk: does not support pairs or triples in the arguments *)
                let derivePremise subst_conds_goalPr_triple = 
                  let (subst, conds, goalPr) = subst_conds_goalPr_triple in
                  let updateProof 
                    (subst_conds_premisePr_triple : 
                       substitution * (list ISubstrateQueryTerm) * term(*Evidence*))
                    : (substitution * (list ISubstrateQueryTerm) * term(*Evidence*)) = 
                    let (subst, conds, premisePr) = 
                      subst_conds_premisePr_triple in
                    (* premisePr is the proof of i1 ; pr is the proof of i1 => i2 *)
                    (* then repl is a proof of i2                                 *)
                    let repl = App ModusPonensEvidence [premisePr; pr] in
                      (* TODO: see Builders.fs and Primitives.fs *)
                      (subst, conds,
                        (term_apply goalPr 
                          (extend id v repl) : term(*Evidence*))) in 
                  let tmp = doDeriveJustification 
                              _infostrate i1 pair_subst_conds in
                  let (subst, conds) = pair_subst_conds in
                  (* TODO: make sure these are the right subst and conds *)
                    map updateProof tmp
                in 
                  append
                    (* if i1 => i2 is in the infostrate, we can try to use it directly... *)
                    (straight (goal, inf))
                    (* ... or try to use i2 and add i1 as a precondition                  *) 
                    (collect derivePremise 
                       (tryDeriveJustification 
                          _infostrate (subst, conds) (Var v) (goal, i2))) 
            | inf -> straight (goal, inf)

  (* same as doDerive, but also carries a justification for each result *)
  (* (except that no prefix is carried)                                 *)
  and doDeriveJustification 
        (_infostrate: option IInfostrate) (target: term(*Infon*))
        (pair_subst_conds: substitution * list ISubstrateQueryTerm) 
        : list (substitution * (list ISubstrateQueryTerm) * term(*Evidence*)) = 
    let (subst, conds) = pair_subst_conds in
      let straight goal =
        (* just runs tryDeriveJustification on all the JustifiedInfon of the infostrate *)
        let aux infon = 
          match infon with
            | App JustifiedInfon [inf; pr] -> 
               tryDeriveJustification _infostrate (subst, conds) pr (goal, inf)
            | _ -> []
        in
          collect aux (value_knowledge _infostrate)
      in
        (match target with
          | App AsInfon [ SubstrateQueryTerm(exp) ] ->
            (* AsInfon(...) is stored as a new side condition *)
               [(subst, append conds [exp], 
                 App AsInfonEvidence [(SubstrateQueryTerm(exp))])]
                  (* Rk2: cannot change SubstrateQueryTerm(exp) :: [] to [...] *)
          | App AndInfon infons ->(* TODO: fix the performance decrease *)
            (* In the case of conjunction we start with the current substitution and side conditions and *)
            (* continue accumulating these by calling recursively on each of the infons in the conjunction *)
              let parts = 
                (fold_left (* should be done more efficiently with a fold_right *)
                   (fun res infon ->
                      collect
                        (fun sce_triple -> 
                           let (subst2, conds2, evLeft) = sce_triple in
                             map
                               (fun sce_triple2 ->
                                   let (subst3, conds3, evRight) = 
                                     sce_triple2 in
                                     (subst3, conds3, 
                                       App AndEvidence (evLeft :: [evRight])))
                                       (* Bug? I think if evLeft=App AndEvidence lLeft,
                                          the evidence here should be (App AndEvidence lLeft) :: [evRight])
                                          otherwise it should just be (App AndEvidence evLeft) :: [evRight])
                                        *)
                               (doDeriveJustification 
                                   _infostrate infon (subst2, conds2))     
                        ) res)
                   [(subst, conds, App EmptyEvidence [])]
                   infons)
              in 
                append
                  (* to prove an and, we can prove the and(...) directly...*)
                  (straight target)
                  (* ... or prove each subterm one by one *)
                  parts
          | goal -> straight goal)
  
  val unifyAndSimpl : option substitution 
                      -> list (term * term)
                      -> option substitution
  (* Given a substitution s, tries to build a more precise substitution  *)
  (* that unifies all the pairs of terms in ts.                          *)
  (* Returns None if failed.                                             *)
  let rec unifyAndSimpl (s: option substitution) (ts: list (term * term)) = 
    match ts with
      | [] -> s
      | (a, b) :: ts ->
          (match s with
             | None -> None
             | Some s -> (match unifyAndSimpl (unifyFrom a s b) ts with
                          | Some s -> Some s
                          | None -> (match a with
                                     | App ImpliesInfon [i1; i2] -> 
                                        unifyFrom b s i2
                                     | _ -> None)))

  val stripPrefix : substitution 
                    -> list (term * term)
                    -> list term
                    -> (term -> term) 
                    -> ((list term) * term)
                    -> term 
                    -> ref (list (substitution * list term)) 
                    -> unit

  (* Tries to prove template using infon (=snd(pairPrefixInfon)), an infon *)
  (* from the infostrate. prefixUnif is a list of terms whose unification  *)
  (* has been delayed; preconds is a list of preconditions to be fulfilled,*)
  (* prefix (=fst(pairPrefixInfon)) and suff deal with the said prefixes.  *)
  (* The result is added to the list res through a side effect.            *)
  let rec stripPrefix (subst:substitution)
    (prefixUnif:list (term(*Principal|Infon*) * term(*Principal|Infon*)))
      (* a prefix of pairs of terms to be unified;     *)
      (* can be pairs of Principals or pairs of Infons *)
    (preconds:list term(*Infon*))
      (* preconditions already calculated and carried over in rec calls *)
    (suff:term(*Infon*) -> term(*Infon*)) 
      (* suff is a function that takes an infon and adds a prefix in front *)
      (* of it, of the form "p1 said p2 said [.]"                          *)
      (* it starts empty but plays with the prefix and the infon like this:*)
      (* if suff adds "p1; p2; p3", prefix is "p4; p5" and infon is "p4 said I"*)
      (* then it is the same as suff adding "p1; p2; p3; p4", prefix being "p5"*)
      (* and infon being I; that's what the SaidInfon recursive call is about. *)
    (pairPrefixInfon:((list term(*Principal*)) * term(*Infon*))) 
      (* a prefix pref and an infon i from the infostrate,   *)
      (* that we are trying to match template to             *)
      (* The prefix comes from the initial target, and is the*)
      (* prefix to which the infon's prefix SHOULD match     *)
    (template:term(*Infon*))
      (* the template = target *)
    (res:ref (list (substitution * list term(*Infon*)))) : unit = 
      (* the result that will be updated through side effects                *)
      (* it consists of a list of substitution and, associated with each     *)
      (* substitution, a list of preconditions that need to be fulfilled for *)
      (* this substitution to be a valid solution to the target              *)
    let immediate = function (* always called on pairPrefixInfon *)
      | ([], infon) ->
        (* we try to directly match template to the infon i from the infostrate; *)
        (* if we can fulfill all the other conditions in prefixUnif, that's a    *)
        (* solution to add to res.                                               *)
          (match unifyAndSimpl (Some subst) ((template, infon) :: prefixUnif) with
             | Some subst ->
                 (res := ((subst, preconds) :: (!res)))
             | None -> ())
      | _ -> () in 
      (match pairPrefixInfon with
         | ((t1 :: pref), App SaidInfon [t2; i]) -> (* Rk: , binds tighter than ::, weird*)
           (* if the infon from the infostrate is a said, we try to match it     *)
           (* against t1, the head of the prefix                                 *)
           (* only two cases where suff is changed *)
             (match unifyFrom t1 subst t2 with
                | Some subst -> 
                    stripPrefix subst prefixUnif preconds 
                      (fun j -> suff (App SaidInfon [t2;j]))
                      (pref, i) template res 
                | None -> (* Q: I think this case in unnecessary:              *)
                          (* if the call to unifyFrom returned no solution     *)
                          (* there is no reason why it would have a solution   *)
                          (* later. On all the pairs of prefixUnif, eventually *)
                          (* unifyFrom will be called (in unifyAndSimpl).      *)
                    stripPrefix subst ((t1, t2) :: prefixUnif)
                      preconds (fun j -> suff (App SaidInfon [t2;j]))
                      (pref, i) template res)
         | (pref, App AndInfon infons) ->
           (* if an and is in the infostrate, we can use any of it subterms as *)
           (* given. Q: we never try to use the and directly?                  *)
           (* A: probably because we already stripped down the target in       *)
           (* doDerive                                                         *)
             iterate (fun infon -> 
                        stripPrefix subst prefixUnif preconds suff 
                          (pref, infon) template res) infons
         | (pref, App ImpliesInfon [a; b] ) ->
           (* if a => b is in the infostrate, we can try to use it directly... *)
             immediate pairPrefixInfon;
           (* ... or try to use b and add a as a precondition                  *) 
             stripPrefix subst prefixUnif (suff a :: preconds) suff 
                                          (* only place where suff is actually used *)
               (pref, b) template res
         | (pref, Var v) when domainContains subst v ->
           (* If a variable is part of the current substitution it is applied *)
           (* and we call recursively *)
             stripPrefix subst prefixUnif preconds suff 
               (pref, subst_apply subst v) template res
         | pairPrefixInfon -> immediate pairPrefixInfon)

  (*   Heart #1 of backwards chaining                                        *)
  (* Given a current Substitution, a prefix (list of principal ITerms) and   *)
  (* a target infon ITerm to derive, this method will return a list of       *)
  (* Substitutions that satisfy the given template, each of which will have a*)
  (* list of preconditions (infon ITerms) that need to be verified in order  *)
  (* for that Substitution to be a real solution                             *)
  let infonsWithPrefix (_infostrate: option IInfostrate) (subst: substitution)
                       (pref: list term(*Principal*)) (target: term(*Infon*)) 
                       : list (substitution * list term(*Infon*)) =
    let res = ref [] in
      iterate (fun k -> stripPrefix subst [] [] (fun x -> x) 
                         (pref, k) target res)
              (value_knowledge _infostrate (* : list term(*Infon*)*));
      !res
    
  val doDerive : option IInfostrate 
                 -> list term 
                 -> (substitution * list ISubstrateQueryTerm) 
                 -> term 
                 -> list (substitution * list ISubstrateQueryTerm)
  val checkOne : option IInfostrate 
                 -> ( ((substitution * list ISubstrateQueryTerm) * list term )
                 -> list (substitution * list ISubstrateQueryTerm))

  (*  Given a prefix (list of principal ITerms) a current Substitution with *)
  (*  side conditions (AsInfo ITerms) and a target infon ITerm to derive    *)
  (*  this method will recursively derive the target infon depending on its *)
  (*  structure. *)
  let rec doDerive (_infostrate: option IInfostrate) (pref: list term(*Principal*)) 
      (* pref is a prefix of principals p said q said ... *) 
      (pair_subst_conds: substitution * list ISubstrateQueryTerm)
      (target: term(*Infon*)) =
    (* invariant: the result substitution is an extension of subst? (check it) *)
    let (subst, conds)= pair_subst_conds in
      match target with
        | App AndInfon infons -> 
          (* In the case of conjunction we start with the current substitution and side conditions and *)
          (* continue accumulating these by calling recursively on each of the infons in the conjunction *)
            (* Q: this fold_left indicates that the returned list is order insensitive *)
            fold_left (fun substs infon ->
                         collect 
                           (fun s -> doDerive _infostrate pref s infon) substs)
              [(subst, conds)] infons
        | App EmptyInfon [] -> 
          (* Empty infon is always satisfiable by the current substitution and side conditions *)
            [(subst, conds)]
        | App SaidInfon [ppal; infon] ->
          (* Said infons are handled recursively by pushing the principal term into the prefix *)
            doDerive _infostrate (ppal :: pref) (subst, conds) infon
        | Var(v) when domainContains subst v ->
          (* If a variable is part of the current substitution it is applied and we call recursively *)
            doDerive _infostrate pref (subst, conds) (subst_apply subst v)
        | App AsInfon [SubstrateQueryTerm(exp)] ->
          (* AsInfon(...) is stored as a new side condition, unless it is inside a non-empty prefix *)
            if isEmpty pref 
            then [(subst, append conds [exp])] (* Q: why not cons? seems to be order sensitive here. *)
            else failwith "asInfon(...) under prefix"
        | App JustifiedInfon [inf; ev] when pref = [] ->
          (* Justified infons are treated separately since we need to construct proofs for these *)
          (* we only keep (subst, conds) whose associated evidence unifies with ev *)
            let unifyEv triple = 
              let (subst', conds', ev') = triple in
                match unifyFrom ev subst' ev' with
                  | Some s ->
                      let x = append conds conds' in  (* Q: are the conditions contained in conds not already contained in conds'? *)
                        [(s, x)]
                  | None -> [] in (* : list (substitution * list ISubstrateQueryTerm) *)
              collect unifyEv 
                (doDeriveJustification _infostrate inf (subst, conds))
        (* Q: what about ImpliesInfon? *)
        | target ->
          (* For every other case we call se.InfonsWithPrefix(..) which will give us a list of *)
          (* substitutions, each of which will have a list of infon ITerms (preconditions) that *)
          (* need to be  satisfied in order for that substitution to be returned. This is were the *)
          (* backwards chaining happens, since we recursively check all the preconditions one by one *)
          (* (with checkOne) *)
            (* Rk: Bug? We really should have a nested let rec *)
            (*             let rec checkOne = function *)
            (*               | (substConds, pre :: pres) ->  *)
            (*                   collect (fun s -> checkOne (s, pres)) (doDerive _infostrate [] substConds pre) *)
            (*               | (substConds, []) -> *)
            (*                   substConds :: [] in *)
            collect 
              (checkOne _infostrate) 
              (map 
                 (fun (substitution, preconds) -> ((substitution, conds), preconds))
                 (infonsWithPrefix _infostrate subst pref target)) (* : list (substitution * list term))) *)
  
  (*   Heart #2 of backwards chaining                                                    *)
  (* Given the _infostrate, and a list of                                                *)
  (* ((subst:substitution, conds:list ISubstrateQueryTerm), preconds:list Term(*Infon*)) *)
  (* such that some target (not in the arguments) would be satisfied by subst if         *)
  (* the conds are true in the substrate and if the preconds (preconditions) are true    *)
  (* in the infostrate (i.e., as infons), derives each of the preconds to get a list of  *)
  (* pairs (subst, conds), more precise than the initial one                             *)
  (* such that subst satisfies the target if conds are true in the                       *)
  (* substrate. Recursive on the set of preconditions.                                   *)
  (* The preconds are poured into (subst, conds) to make it more precise                 *)
  and checkOne _infostrate = function
      | (substConds, precond :: preconds) ->
        (* substConds2 is more precise than substConds: it satisfies precond             *)
          collect (fun substConds2 -> checkOne _infostrate (substConds2, preconds)) 
            (doDerive _infostrate [] substConds precond)
      | (substConds, []) ->
          [substConds]
  
  (* Obtain a list of Substitution with accompanying side conditions (AsInfon *)
  (* ITerms) [call to doDerive]. Then return only those Substitutions that    *)
  (* satisfy all their side conditions [call to substrateDispatcher_solve].   *)
  let derive (_infostrate: option IInfostrate) (target: term(*Infon*)) 
             (substs: list substitution) : list substitution =
    collect
      (fun subst ->
         (collect
            (fun (subst, conds) -> 
               (substrateDispatcher_solve conds [subst]))
            (doDerive _infostrate [] (subst, []) target)))
      substs
      
  let deriveWrapper (_i: option IInfostrate) (t: term) (s: list substitution) 
                    (*: (TranslationfromFStar.listFS substitution) *)=
    (ListOfPrimsList (derive _i t s)
     (* : TranslationfromFStar.listFS substitution *)) 
    (* Rk: cannot seem to annotate with type *)

  let deriveJustification 
      (_infostrate: option IInfostrate) (target: term(*Infon*)) (proofTemplate: term(*Evidence*))
      (substs: list substitution) : list substitution =
    collect
      (fun subst ->
         (collect
            (fun (subst, conds, proof) -> (* Note: much like derive, except we check that ...*)
               (match unifyFrom proof subst proofTemplate with (* ... the proof found is compatible with the proof template requested *)
                  | Some subst -> substrateDispatcher_solve conds [subst]
                  | None -> []))
            (doDeriveJustification _infostrate target (subst, []))))
      substs
      
  let deriveJustificationWrapper (_i: option IInfostrate) (i: term) 
                                 (p: term) (s: list substitution) 
                                 (*: listFS substitution*) =
    ListOfPrimsList (deriveJustification _i i p s)