#light
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

module Term
open TypeHeaders
open TranslationFromFStar

  extern reference TranslationToFStar {language="F#";
                                       dll="TranslationToFStar";
                                       namespace="";
                                       classname="TranslationToFStar"}
  extern TranslationToFStar val FStarVarOfIVar : IVar -> Types.var
  extern TranslationToFStar val FStarTermOfITerm : ITerm -> Types.term
  extern TranslationToFStar val FStarSubstitutionOfISubstitution: ISubstitution -> Types.substitution

  val vars : Types.term -> list Types.var
  let rec vars (t: Types.term) : list Types.var =
    match t with
    | Types.ForallT v t -> (* Rk: error "Too many pattern variables" means ForallT(a, b) should be replaced with ForallT((a, b)) *)
      let termVars = HashSet_new(vars t) in
      ignore(HashSet_remove termVars v);
      HashSet_toList(termVars)
    | Types.App _ tl -> HashSet_toList( HashSet_new(collect (fun (a: Types.term) -> vars a) tl) )
    | Types.Var v -> [v]
    | Types.Const _ -> []
    | Types.ConcretizationEvidence t s -> 
      let ret = HashSet_new(vars t) in
      iterate 
        (fun v -> 
          if HashSet_contains ret v then
            ignore(HashSet_remove ret v) else ();
          HashSet_unionWith ret (vars (subst_apply s v)) )
        (domain s);
      HashSet_toList(ret)
    | Types.SubstrateQueryTerm t0 ->
      map FStarVarOfIVar (substrateQueryTerm_Vars t0)
    | Types.SubstrateUpdateTerm t0 ->
      map FStarVarOfIVar (substrateUpdateTerm_Vars t0)
      
  val boundVars : Types.term -> list Types.var 
  let rec boundVars (t: Types.term) : list Types.var =
    match t with
    | Types.ForallT v t -> HashSet_toList( HashSet_new(v :: (boundVars t)) )
    | Types.App _ tl -> HashSet_toList( HashSet_new(collect (fun (a: Types.term) -> vars a) tl) )
    | Types.Var _ -> []
    | Types.Const _ -> []
    | Types.ConcretizationEvidence t s -> 
      let ret = HashSet_new(boundVars t) in
      iterate
        (fun v -> 
           if HashSet_contains ret v
           then ignore(HashSet_remove ret v)
           else ();
           HashSet_unionWith ret (boundVars (subst_apply s v)) )
        (domain s);
      HashSet_toList(ret)  
    | Types.SubstrateQueryTerm t0 ->
      map FStarVarOfIVar (substrateQueryTerm_boundVars t0)
    | Types.SubstrateUpdateTerm t0 ->
      map FStarVarOfIVar (substrateUpdateTerm_boundVars t0)
      
  open Types (* TODO / Rk: why can't I access v.name 5 lines below without opening Types? *)

  val freshVar : Types.var -> list Types.var -> (Types.var * Types.substitution)
  let freshVar (var: Types.var) (otherVars: list Types.var) =
  (* from ForallTerm.fs *)
    let prefix = "FreshVar#" in
    let freshVars = filter (fun (v: Types.var) -> String_StartsWith (v.name : string) prefix) (var::otherVars) in (* Rk: need the type annotation around v.name, otherwise get a too refined type *)
    let freshNumbers = map (fun (v: Types.var) -> System_Int32_Parse(String_Substring (v.name : string) (String_Length prefix))) freshVars in
    let freshVarId = if isEmpty freshNumbers then 0 else (max freshNumbers) + 1 in
    let freshVar = { name = (Concat prefix (int_to_string freshVarId)); typ = (var.typ  : Types.typ)} in
    let subst = extend id var (Types.Var freshVar) in
    (freshVar, subst)
  
  val composeWith : Types.substitution -> Types.substitution -> Types.substitution
  val term_apply : Types.term -> Types.substitution -> Types.term
  let rec composeWith (s: Types.substitution) (s': Types.substitution) : Types.substitution =
  (* from Subst; moved here because of mutual recursions *)
    let newSubst = copy s in (* Rk: cannot put type annotation on newSubst; error "Feature not yet implemented: Non variable and application let binding\n Desugaring failed" *)
    iterate
      (fun v ->
         ignore (assign newSubst v (term_apply (subst_apply s' v) s)))
        (domain s');
    iterate
      (fun v -> 
         if not (domainContains s' v) 
         then ignore (assign newSubst v (subst_apply s v)) 
         else ())
        (domain s);
      newSubst
    
  (* Subst.composeWith (above) and Term.term_apply (below) are mutually recursive!! *)
  and term_apply (t: Types.term) (s: Types.substitution) : Types.term = 
    match t with
    | Types.Var v ->subst_apply s v 
    | Types.Const _ -> t 
    | Types.ForallT v0 t0 -> 
      (* the substitution is not applied to the quantified variable *)
      let s = forget s [v0] in
      (* check that there will be no variable capture *)
      let varsToCheck = HashSet_new(vars t0) in
      HashSet_intersectWith varsToCheck (domain s);
      let mappedVars = collect (fun (v': Types.var) -> vars (subst_apply s v')) (HashSet_toList varsToCheck) in
      if List_exists (fun v -> v = v0) mappedVars 
      then
        let (newVar, newVarSubst) = freshVar v0 (append (vars t0) mappedVars) in
        Types.ForallT newVar (term_apply (term_apply t0 newVarSubst) s)
      else
        Types.ForallT v0 (term_apply t0 s)
    | Types.App f tl -> Types.App f (map (fun t0 -> term_apply t0 s) tl) (* from TreeTerm.fs *)
    | Types.ConcretizationEvidence t1 s1 -> (* from ExplicitSubstitutionTerm.fs *)
      Types.ConcretizationEvidence t1 (composeWith s s1)
    | Types.SubstrateQueryTerm t0 -> 
      FStarTermOfITerm
        (substrateQueryTerm_apply t0 (ISubstitutionOfFStarSubstitution s) )
    | Types.SubstrateUpdateTerm t0 ->
      FStarTermOfITerm 
        (substrateUpdateTerm_apply t0 (ISubstitutionOfFStarSubstitution s) )
  
  val innerTerm : Types.term -> Types.term 
  let rec innerTerm (ft: Types.term) = (* from ForallTerm.fs *)
    match ft with 
    | Types.ForallT v t -> innerTerm t
    | _ -> ft
    
  val instantiate : Types.term -> Types.substitution -> Types.term
  let instantiate (ft: Types.term) (s: Types.substitution) = (* from ForallTerm.fs *)
    let remainingVars = HashSet_new(boundVars ft) in
    HashSet_exceptWith remainingVars (domain s); 
    let innerSubst = term_apply (innerTerm ft) s in
      fold_left (fun t v -> Types.ForallT v t)  
        innerSubst (HashSet_toList remainingVars)

  val changeVarName : Types.term -> Types.substitution -> (Types.term * Types.substitution)
  (* Change the variable of the ForallTerm ft such that it does not *)
  (* appear in s *)
  let changeVarName (ft: Types.term) (s:Types.substitution) = (* from ForallTerm.fs *)
    match ft with
    | Types.ForallT v t ->
      let (v', s') = 
        freshVar
          v 
          (append
           (fold_right
              (fun v acc -> append (vars (subst_apply s v)) acc)
              (domain s) [])
           (append (domain s) (vars t))) in
      (Types.ForallT v (term_apply t s'), s')
    | _ -> failwith "changeVarName can only be called on a ForallTerm"
    
  val unifyFromWhileLoop : list Types.term -> list Types.term -> Types.substitution -> ref bool -> ref Types.substitution -> ref int -> unit
  val unifyFrom : Types.term -> Types.substitution -> Types.term -> option Types.substitution
  let rec unifyFromWhileLoop (tlist1 : list Types.term) (tlist2 : list Types.term) s okSoFar ret i=
    if (!okSoFar) && (lessThan (!i) (length tlist1)) then (* Rk: need of parenthesis around condition in F* *)
     (match unifyFrom (term_apply (nth tlist1 (!i)) (!ret))
                      (!ret) 
                      (term_apply (nth tlist2 (!i)) (!ret)) with
      | Some s -> ret := s
      | None -> 
          ((okSoFar := false);
           (i := ((!i) + 1));
           unifyFromWhileLoop tlist1 tlist2 s okSoFar ret i))
    else ()

  and unifyFrom (t1: Types.term) (s: Types.substitution) (t2: Types.term) : option Types.substitution =
    match t1 with
    | Types.Var v1 -> (* from Variable.fs *)
      (match t2 with
      | _ when term_apply t1 s = term_apply t2 s -> Some s
      | _ when not(List_exists (fun v' -> v1 = v') (vars t2)) -> 
        if domainContains s v1 then
          unifyFrom (term_apply t1 s) s t2
        else
          Some(composeWith (extend id v1 (term_apply t2 s)) s)
      | _ -> None)
    | Types.Const c1 -> (* from Constants.fs *)
      (match t2 with
      | _ when t1 = term_apply t2 s -> Some s
      | Types.Var(v2) -> unifyFrom t2 s t1
      | _ -> None)
    | Types.ForallT v1 t1' -> (* from ForallTerm.fs *)
      (match t2 with
      | Types.ForallT v2 t2' ->
        (match unifyFrom (innerTerm t1) s (innerTerm t2) with
        | Some s ->
          if List_forall 
                (fun v -> 
                  if List_exists (fun v' -> v' = v) (boundVars t1) then
                    (match subst_apply s v with
                     | Types.Var v0 -> List_exists (fun v' -> v' = v0) (boundVars t2)
                     | _ -> false)
                  else if List_exists (fun v' -> v' = v) (boundVars t2) then
                    (match subst_apply s v with
                     | Types.Var v0 -> List_exists (fun v' -> v' = v0) (boundVars t1)
                     | _ -> false)
                  else 
                    true) 
                (domain s) then
            Some s
          else 
            None
        | _ -> None)
      | _ -> 
        unifyFrom t1' s t2) 
    | Types.App f1 tlist1 ->
      (match t2 with
      | Types.Var(v) -> unifyFrom t2 s t1
      | Types.App f2 tlist2
        when ((f1 = f2)
          && (length tlist1 = length tlist2)) -> (* Rk: need parenthesis *)
          let okSoFar = ref true in 
          let ret = ref s in
          let i = ref 0 in
        unifyFromWhileLoop tlist1 tlist2 s okSoFar ret i;
        if (!okSoFar) then
          Some (!ret)
        else
          None
      | _ -> None)
    | Types.ConcretizationEvidence et1 s1 ->
      match t2 with
      | Types.ConcretizationEvidence et2 s2 -> 
        unifyFrom (term_apply et1 s1) s2 (term_apply et2 s2)
      | _ -> unifyFrom t2 s t1
    | Types.SubstrateQueryTerm t0 ->
      option_map FStarSubstitutionOfISubstitution
        (substrateQueryTerm_unifyFrom t0(ISubstitutionOfFStarSubstitution s) (ITermOfFStarTerm t2) )
    | Types.SubstrateUpdateTerm t0 ->
      option_map FStarSubstitutionOfISubstitution
        (substrateQueryTerm_unifyFrom t0(ISubstitutionOfFStarSubstitution s) (ITermOfFStarTerm t2) )
      
  let unify (t1: Types.term) (t2: Types.term) : option Types.substitution =
    unifyFrom t1 (id) t2
    
