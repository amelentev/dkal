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

module LogicEngine
  open TypeHeaders
  open Types
  open Term 

  extern reference Utilities {language="F#";
                              dll="LogicEngine.FStar";
                              namespace="Microsoft.Research.Dkal.LogicEngine.FStar";
                              classname="Utilities"}
  extern Utilities val _value_knowledge : optionFS IInfostrate -> listFS term
  extern Utilities val _value_checkSignature : optionFS ISignatureProvider -> term -> principal -> object -> bool
  extern Utilities val _substrateDispatcher_solve : listFS ISubstrateQueryTerm -> listFS substitution -> listFS substitution

  val value_knowledge : option IInfostrate -> list term
  let value_knowledge oi = PrimsListOfList (_value_knowledge (OptionOfPrimsOption oi))
  val value_checkSignature : option ISignatureProvider -> term -> principal -> object -> bool
  let value_checkSignature oi t p o = _value_checkSignature (OptionOfPrimsOption oi) t p o
  val substrateDispatcher_solve : list ISubstrateQueryTerm -> list substitution -> list substitution
  let substrateDispatcher_solve isl sl = PrimsListOfList (_substrateDispatcher_solve (ListOfPrimsList isl) (ListOfPrimsList sl))

  let log = ref ""
  let _freshVarId = ref 0

  val finalOutcome : term -> term
  let rec finalOutcome (infon: term) =
    match infon with
    | App((ImpliesInfon, [_; i])) -> finalOutcome i
    | Forall((_,t)) -> finalOutcome t
    | i -> i

  let canSign (principal: principal) (infon: term) =
    match finalOutcome infon with
    | App((SaidInfon, [Const(PrincipalConstant(p)); _])) -> principal = p
    | _ -> false
    
  val checkJustification : option ISignatureProvider -> term -> option term
  let rec checkJustification _signatureProvider (evidence: term) = 
    match evidence with
      | App((SignatureEvidence, [Const(PrincipalConstant(ppal)); inf;
                                 Const(SubstrateConstant(signature))])) ->
          if canSign ppal inf &&
            value_checkSignature (*!*)_signatureProvider inf ppal signature
          then
            Some inf
          else
            (* //log.Warn("Spoofed signature {0} from {1} on {2}", signature, ppal, inf) *)
            None      
      | App((ModusPonensEvidence, [e1; e2])) ->
          (match checkJustification _signatureProvider e1, checkJustification _signatureProvider e2 with
             | Some i1, Some (App((ImpliesInfon, [i1'; i2]))) when i1 = i1' ->
                 Some i2
             | _ ->
                 (* //log.Warn("Malformed modus ponens proof on {0} and {1}", e1, e2) *)
                 None )
      | App((AndEvidence, evidences)) ->
          let infons = collect (fun evidence ->  
                                  match checkJustification _signatureProvider evidence with
                                    | Some i -> [i]
                                    | None -> []) evidences in (* Rk: F# does not need the 'in' *)
            if length infons = length evidences then
              Some(App(AndInfon, infons)) (* Rk: no support for |> and <| ? *)
            else
              (* //log.Warn("Malformed conjunction proof on {0}", evidence) *)
              None
      | App((AsInfonEvidence, [SubstrateQueryTerm(query)])) ->
          if (not(isEmpty(substrateDispatcher_solve [query] [id]))) then
            (*Some(App(AsInfon, (SubstrateQueryTerm(query)) :: [])) *) (* Rk: can't build a list as in [1], has to use 1::[] *)
            Some(App(AsInfon, [ (SubstrateQueryTerm query) ]))
          else
            (* //log.Warn("Non-true asInfon evidence at {0}", query) *)
            None    
      | ConcretizationEvidence((ev, subst)) ->
          (match checkJustification _signatureProvider ev with
             | Some generalProof -> 
                 let concreteProof = match generalProof with
                   | Forall _ -> instantiate generalProof subst
                   | _ -> term_apply generalProof subst
		 in
                   Some concreteProof
             | None -> None) (* Rk: no need for parenthesis or 'in' in F# *)
      | _ -> 
          (* //log.Warn("Unhandled evidence type at {0}", evidence) *)
	  None

  (*open TranslationfromFStar *)(* Rk: Why can't I do TranslationfromFStar.optionFS on next line?? *)
  let checkJustificationWrapper (s:option ISignatureProvider) (e:term) (*: optionFS term*) =
    OptionOfPrimsOption (checkJustification s e)

  val freshVar : typ -> var
  let freshVar (t: typ) =
    let name0 = (Concat "SimpleLogicEngine#Var" (string_of_any (read _freshVarId))) in
    let ret = {
      name = name0;
      typ = t} in
      (_freshVarId := (!_freshVarId) + 1);
      ret
    
  val tryDeriveJustification : option IInfostrate -> (substitution * list ISubstrateQueryTerm) -> term 
                            -> (term * term) -> list (substitution * (list ISubstrateQueryTerm) * term)
  val doDeriveJustification  : option IInfostrate -> term -> (substitution * list ISubstrateQueryTerm) 
                           -> list (substitution * (list ISubstrateQueryTerm) * term)

  let rec tryDeriveJustification 
      (_infostrate: option IInfostrate) (pair_subst_conds: substitution * list ISubstrateQueryTerm)
      (pr: term) (pair_goal_inf: term * term) =
    match pair_subst_conds with (subst, conds) -> 
      match pair_goal_inf with (goal, inf) -> 
        let straight = function (goal, premise) -> (* Rk: impossible to put types of goal and premise, neigher (goal:term, premise:term) nor (goal, premise) : term * term seem to work  *)
          (match unifyFrom premise subst goal with
             | Some s -> [(s, conds, term_apply pr s)]
             | None -> [] ) in 
          match inf with
            | Forall((v, t)) -> (* Rk: 'as' construct not supported *)
                (match
                   (if domainContains subst v then
                      (match changeVarName inf subst with (newFt, s') ->
                         t, term_apply pr s')
                    else
                      t, pr)
		 with (i, pr) ->
                   tryDeriveJustification _infostrate (subst, conds) pr (goal, i) ) 
            | App((ImpliesInfon, [i1; i2])) -> 
                let v = freshVar Evidence in (* Rk: does not support pairs or triples in the arguments *)
                let derivePremise subst_conds_goalPr_triple = 
                  let (subst, conds, goalPr) = subst_conds_goalPr_triple in (* Rk: does not seem to be able to parenthesize this match *)
                  let updateProof (subst_conds_premisePr_triple : substitution * (list ISubstrateQueryTerm) * term) : (substitution * (list ISubstrateQueryTerm) * term) = 
                    let (subst, conds, premisePr) = subst_conds_premisePr_triple in
                    let repl = App(ModusPonensEvidence, [premisePr; pr]) in (* TODO: see Builders.fs and Primitives.fs *)
                      ((subst : substitution), 
                       (conds : list ISubstrateQueryTerm),
                       (term_apply goalPr (extend id v repl : substitution) : term)) in 
                  let tmp = doDeriveJustification _infostrate i1 pair_subst_conds in
                    (match pair_subst_conds with (subst, conds) ->
                       map updateProof tmp
                    )
	        in 
	          append
                    (straight (goal, inf))
                    (collect derivePremise 
                       (tryDeriveJustification _infostrate (subst, conds) (Var v) (goal, i2))) 
            | inf -> straight (goal, inf)
                
  and doDeriveJustification (_infostrate: option IInfostrate) (infon: term) (pair_subst_conds: substitution * list ISubstrateQueryTerm) = 
    match pair_subst_conds with (subst, conds) ->
      let straight goal =
        let aux infon = 
          match infon with
            | App((JustifiedInfon, [inf; pr])) -> 
                tryDeriveJustification _infostrate (subst, conds) pr (goal, inf)
            | _ -> []
	in
          collect aux (value_knowledge _infostrate)
      in
        (match infon with
           | App((AsInfon, [ SubstrateQueryTerm(exp) ])) ->
               (subst, append conds (exp :: []), App(AsInfonEvidence, SubstrateQueryTerm(exp) :: [])) :: []
           | App((AndInfon, infons)) -> (* TODO: fix the performance decrease *)
               let parts = 
                 (fold_left 
                    (fun res infon ->
                       fold_right
                         (fun sce_triple res2 -> 
                            let (subst, conds, evLeft) = sce_triple in
                              append 
				(fold_right
                                   (fun sce_triple2 res3 ->
                                      let (subst, conds, evRight) = sce_triple2 in
                                        (subst, conds, 
                                         App(AndEvidence, evLeft :: evRight :: [])) :: res3)
                                   (doDeriveJustification _infostrate infon (subst, conds)) []
                                ) 
				res2
			 ) res [])
                    ((subst, conds, App(EmptyEvidence, [])) :: []) 
                    infons
		 )
	       in 
                 append (straight infon) []
           | goal -> straight goal 
        ) 
  
  val unifyAndSimpl : option substitution -> list (term * term) -> option substitution
  let rec unifyAndSimpl (s: option substitution) (ts: list (term * term)) = 
    match ts with
      | [] -> s
      | (a, b) :: ts ->
          (match s with
             | None -> None
             | Some s -> unifyAndSimpl (unifyFrom a s b) ts)

  val stripPrefix : substitution -> list (term * term) -> list term -> (term -> term) 
                -> ((list term) * term) -> term -> ref (list (substitution * list term)) -> unit
  let rec stripPrefix subst prefixUnif preconds suff t template res = 
    let immediate = function
      | ([], i) ->
          (match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
             | Some subst ->
                 (res := ((subst, preconds) :: (!res)))
             | None -> ())
      | _ -> () in 
      (match t with
         | ((t1 :: pref), App((SaidInfon, [t2;i]))) -> (* Rk: type annotation on t1 not supported *)
             (match unifyFrom t1 subst t2 with
                | Some subst -> 
                    stripPrefix subst prefixUnif preconds 
                      (fun j -> suff (App(SaidInfon, [t2;j]))) (pref, i) template res
                | None ->
                    stripPrefix subst ((t1, t2) :: prefixUnif)
                      preconds (fun j -> suff (App(SaidInfon, [t2;j]))) (pref, i) template res) 
         | (pref, App((AndInfon, infons))) ->
             iterate (fun infon -> 
                        stripPrefix subst prefixUnif preconds suff (pref, infon) template res) infons
         | (pref, App((ImpliesInfon, a :: b :: []))) ->
             immediate t;
             stripPrefix subst prefixUnif (suff a :: preconds) suff (pref, b) template res
         | (pref, Var v) when domainContains subst v ->
             stripPrefix subst prefixUnif preconds suff (pref, subst_apply subst v) template res
         | t -> immediate t)

  let infonsWithPrefix (_infostrate: option IInfostrate) (subst: substitution) (pref: list term) (template: term) =
    let res = ref [] in
      iterate (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, k) template res) 
        (value_knowledge _infostrate);
      !res
    
  val doDerive : option IInfostrate -> list term -> (substitution * list ISubstrateQueryTerm) -> term 
              -> list (substitution * list ISubstrateQueryTerm)
  val checkOne : option IInfostrate -> ( ((substitution * list ISubstrateQueryTerm) * list term )
              -> list (substitution * list ISubstrateQueryTerm))

  (*  Given a prefix (list of principal ITerms) a current Substitution with  *)
  (*  side conditions (AsInfo ITerms) and a target infon ITerm to derive *)
  (*  this method will recursively derive the target infon depending on its *)
  (*  structure. *)
  let rec doDerive (_infostrate: option IInfostrate) (pref: list term) (* pref is a prefix of principals p said q said ... *) 
      (pair_subst_conds: substitution * list ISubstrateQueryTerm)
      (infon: term) = (* infon is the target *)
    let (subst, conds)= pair_subst_conds in  (* Rk: does not support patterns in formal parameters *)
      match infon with
        | App((AndInfon, infons)) -> 
            (* Q: this fold_left indicates that the returned list is order insensitive *)
            fold_left (fun substs infon ->
                         collect 
                           (fun s -> doDerive _infostrate pref s infon) substs)
              ((subst, conds) :: []) infons
        | App((EmptyInfon, []))-> 
            (subst, conds) :: []
        | App((SaidInfon, [ppal; infon])) ->
            doDerive _infostrate (ppal :: pref) (subst, conds) infon
        | Var(v) when domainContains subst v ->
            doDerive _infostrate pref (subst, conds) (subst_apply subst v)
        | App((AsInfon, [SubstrateQueryTerm(exp)])) ->
            if isEmpty pref 
            then [(subst, append conds [exp])] (* Q: why not cons? seems to be order sensitive here. *)
            else failwith "asInfon(...) under prefix"
        | App((JustifiedInfon, [inf; ev])) when pref = [] ->
            let unifyEv triple = 
              let (subst', conds', pr') = triple in
                match unifyFrom ev subst' pr' with
                  | Some s ->                      (*                       let x = (append conds conds' : list ISubstrateQueryTerm) in *)
                      let x = append conds conds' in 
                        [(s, x)]
                  | None -> [] in 
              (* ([] : list (substitution * list ISubstrateQueryTerm) )) in *)
              collect unifyEv (doDeriveJustification _infostrate inf (subst, conds))
        | templ ->
            (* Rk: Bug? We really should have a nested let rec *)
            (*             let rec checkOne = function *)
            (*               | (substConds, pre :: pres) ->  *)
            (*                   collect (fun s -> checkOne (s, pres)) (doDerive _infostrate [] substConds pre) *)
            (*               | (substConds, []) -> *)
            (*                   substConds :: [] in *)
            collect 
              (checkOne _infostrate) 
              (map 
                 (fun (s, ps) -> ((s, conds), ps))
                 (infonsWithPrefix _infostrate subst pref templ)) (*  : list (substitution * list term))) *)
                
and checkOne _infostrate = function
    | (substConds, pre :: pres) ->
        collect (fun s -> checkOne _infostrate (s, pres)) (doDerive _infostrate [] substConds pre)
    | (substConds, []) ->
        substConds :: []
  
  let derive (_infostrate: option IInfostrate) (target: term) (substs: list substitution) : list substitution =
    fold_right
      (fun subst res ->
	 append
           (fold_right
              (fun (subst, conds) res2 -> 
                 append (substrateDispatcher_solve conds ([subst])) res2)
              (doDerive _infostrate [] (subst, []) (target)) [])
           res)
      substs []
      
  let deriveWrapper (_i: option IInfostrate) (t: term) (s: list substitution) (*: (TranslationfromFStar.listFS substitution) *)=
    (ListOfPrimsList (derive _i t s) (* : TranslationfromFStar.listFS substitution *)) (* Rk: cannot seem to annotate with type *)

  let deriveJustification 
      (_infostrate: option IInfostrate) (target: term) (proofTemplate: term)
      (substs: list substitution) : list substitution =
    fold_right
      (fun subst res -> 
	 append
           (fold_right
              (fun (subst, conds, proof) res2 -> 
		 append (* Note: much like derive, except we check that ...  *)
                   (match unifyFrom proof subst proofTemplate with (* ... the proof found is compatible with the proof template requested *)
                      | Some subst -> substrateDispatcher_solve conds [subst]
                      | None -> [])
		   res2)
              (doDeriveJustification _infostrate target (subst, [])) [])
	   res)
      substs []
      
  let deriveJustificationWrapper (_i: option IInfostrate) (i: term) (p: term) (s: list substitution) (*: listFS substitution*) =
    ListOfPrimsList (deriveJustification _i i p s)
