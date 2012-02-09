module Unify
open Types
open Subst

val fold_left2 : ('a -> 'b -> 'c -> option 'a) 
              -> option 'a -> list 'b -> list 'c -> option 'a
let rec fold_left2 f a lb lc =
  match a, (lb, lc) with
  | (None, (_, _)) -> None
  | (sa, ([], [])) -> sa
  | (Some(a), ((b::tb), (c::tc))) -> fold_left2 f (f a b c) tb tc
  | (_, _) -> raise "Error in fold_left2, lists are not the same size"

(* composes s with {x -> t} *)
val addSubst : s:substitution -> x:var -> t:term -> substitution
let addSubst s1 x t =
  let substxt = extendSubst (emptySubst ()) x t in
  fold_left
    (fun s y -> 
       match lookupVar s1 y with
	 | None -> raise "impos"
	 | Some(ty) -> extendSubst s y (subst ty substxt))
    substxt
    (domain s1)

val unify_aux: s1:substitution
            -> v2:vars 
            -> v1:vars
            -> t1:term    
            -> t2:term 
            -> option substitution

let rec unify_aux s1 v2 v1 t1 t2 : option substitution =
  match subst t1 s1, subst t2 s1 with
    | t1', t2' when (t1':term) = (t2':term) -> Some s1
    | Var x1, t2' when not(contains x1 (freeVars t2')) && contains x1 v1 ->
        Some (addSubst s1 x1 t2')
    | t1', Var x2 when not(contains x2 (freeVars t1')) && contains x2 v2 ->
        Some (addSubst s1 x2 t1')
    | App f1 tlist1, App f2 tlist2 -> 
        if ((f1 = f2) && (length tlist1 = length tlist2)) 
        then 
          (* let _ = println (strcat "Success Compared f1 = "  (strcat (string_of_any_for_coq f1) (strcat " with f2" (string_of_any_for_coq f2)))) in *)
	    fold_left2 
              (fun s t1' t2' -> unify_aux s v2 v1 t1' t2')
 	      (Some s1) 
              tlist1 
              tlist2
        else
          (* let _ = println (strcat "Failed Compared f1 = "  (strcat (string_of_any_for_coq f1) (strcat " with f2" (string_of_any_for_coq f2)))) in *)
            None
    | SubstrateQueryTerm t0, _ -> None
        (*       option_map FStarSubstitutionOfISubstitution *)
        (*         (substrateQueryTerm_unifyFrom t0 (ISubstitutionOfFStarSubstitution s1) *)
        (* 		                                 (ITermOfFStarTerm t2) ) *)
    | SubstrateUpdateTerm t0, _ -> None
        (*       option_map FStarSubstitutionOfISubstitution *)
        (*         (substrateUpdateTerm_unifyFrom t0 (ISubstitutionOfFStarSubstitution s1) *)
        (* 		                                 (ITermOfFStarTerm t2) ) *)
    | t1',t2' -> 
        (* let _ = println (strcat "Failed to unify t1 = "  (strcat (string_of_any_for_coq t1') (strcat " with t2" (string_of_any_for_coq t2')))) in *)
          None

    


(* ad hoc implementation of unification
     informally, only variables in u are available for unification. *)
(* precondition: u and xs must be disjoing *)
val unify: s1:substitution
        -> u:vars 
        -> xs:vars
        -> i:term    
        -> goal:term 
        -> option (s2:substitution{Extends s2 s1} * list term)
let unify s1 u xs i goal =
  match unify_aux s1 u xs i goal with
  | Some(s3) ->
      let s2 = 
        fold_left (fun s x -> match lookupVar s3 x with
		     | None -> raise "impos"
		     | Some t -> extendSubst s3 x t)
	  (emptySubst ()) (domain s3) in
      let l = map (fun x -> subst (Var x) s3) xs in
        assume (Extends s2 s1);
        Some(s2, l)
  | None -> None

val doMatch :  tm:term
             -> s0:substitution 
             -> xs:vars
             -> pat:term
             -> option (s1:substitution{(* Includes xs (Domain s1) && *)
                                        tm=(Subst pat s1)})
let doMatch tm s0 xs pat = 
  match unify s0 xs [] tm pat with 
    | Some ((s1, [])) -> 
        if (tm=(subst pat s1)) (* TODO: Kill these and prove directly from unify *)            
        then 
          (* let _ = println (strcat "Unifying tm= " (string_of_any_for_coq tm)) in  *)
          (* let _ = println (strcat "With pat =  " (string_of_any_for_coq pat)) in  *)
          (* let _ = println (strcat "Got unifier = " (string_of_any_for_coq s1)) in  *)
            Some s1
        else (* let _ = println (strcat "Got bad subst for unifier " (strcat (string_of_any_for_coq tm)  *)
             (*                                                         (strcat " ... "  *)
          (*                                                            (strcat (string_of_any_for_coq pat)  *)
             (*                                                               (strcat "..." (string_of_any_for_coq s1)))))) in  *)
          None
    | _ -> None
