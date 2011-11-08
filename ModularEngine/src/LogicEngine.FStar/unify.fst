module Unify
open TypeHeaders
open Types
open Subst
open TranslationFromFStar

val fold_left2 : ('a -> 'b -> 'c -> option 'a) 
              -> option 'a -> list 'b -> list 'c -> option 'a
let rec fold_left2 f a lb lc =
  match a, (lb, lc) with
  | (None, (_, _)) -> None
  | (sa, ([], [])) -> sa
  | (Some(a), ((b::tb), (c::tc))) -> fold_left2 f (f a b c) tb tc
  | (_, _) -> raise "Error in fold_left2, lists are not the same size"

val addSubst : s:substitution -> x:var -> t:term -> substitution
(* composes s with {x -> t} *)
let addSubst s1 x t =
  let substxt = extendSubst (emptySubst false) x t in
  fold_left
    (fun s y -> 
	   match lookupVar s1 y with
	   | None -> raise "impos"
	   | Some(ty) -> extendSubst s y (subst ty substxt))
    (emptySubst false)
    (domain s1)

(* ad hoc implementation of unification
     informally, only variables in u are available for unification. *)
(* precondition: u and xs must be disjoing *)
val unify: s1:substitution
        -> u:vars 
        -> xs:vars
        -> i:term    
        -> goal:term 
        -> option (s2:substitution{Extends s2 s1} * list term)
(* The "Extends s2 s1" condition needs to be relaxed; it's not true *)

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
  | App f1 tlist1, App f2 tlist2 
      when ((f1 = f2) && (length tlist1 = length tlist2)) ->
	  fold_left2 (fun s t1' t2' -> unify_aux s v2 v1 t1' t2')
	    (Some(s1)) tlist1 tlist2
  | SubstrateQueryTerm t0, _ ->
      option_map FStarSubstitutionOfISubstitution
        (substrateQueryTerm_unifyFrom t0 (ISubstitutionOfFStarSubstitution s1)
		                                 (ITermOfFStarTerm t2) )
  | SubstrateUpdateTerm t0, _ ->
      option_map FStarSubstitutionOfISubstitution
        (substrateUpdateTerm_unifyFrom t0 (ISubstitutionOfFStarSubstitution s1)
		                                 (ITermOfFStarTerm t2) )
  | _ -> None

let unify s1 u xs i goal =
  match unify_aux s1 u xs i goal with
  | Some(s3) ->
      let s2 = fold_left
             (fun s x -> 
			  match lookupVar s3 x with
			    | None -> raise "impos"
			    | Some t -> extendSubst s3 x t
			 ) (emptySubst false) (domain s3) in
      let l = map (fun x -> subst (Var x) s3) xs in
      if extends s2 s1 then (* This is cheating: the condition is almost never true *)
        Some(s2, l) else raise "Fix problem in unification"
  | None -> None
 
(* Spec?? What do we want this to do?
   In particular, do we want to add the variables in the foralls in front of
   p1 and p2 into the unification variables or not? *)
val unify_poly: s1:substitution
            -> uvars1:vars 
            -> uvars2:vars 
            -> p1:polyterm
            -> p2:polyterm
            -> option (s2:substitution{Extends s2 s1})
