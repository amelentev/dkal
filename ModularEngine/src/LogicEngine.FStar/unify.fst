module Unify
open TypeHeaders
open Types
open Subst
open TranslationFromFStar

val fold_left2: ('a -> 'b -> 'c -> option 'a)
             -> option 'a -> list 'b -> list 'c -> option 'a
let rec fold_left2 f oa lb lc =
  match ((oa, lb), lc) with
  | ((None, _), _) -> None
  | ((Some(a), []), []) -> oa
  | ((Some(a), b::tb), c::tc) -> fold_left2 f (f a b c) tb tc
  | _ -> failwith "Two lists not the same size in fold_left2"

(* ad hoc implementation of unification
     informally, only variables in u are available for unification. *)
val unify: s1:substitution (* this substitution will eventually be applied to goal *)
        -> u:vars  (* variables available for unification (in goal) *)
        -> xs:vars (* variables bound in i as foralls *)
        -> i:term    (* try to unify goal with i *)
        -> goal:term 
        -> option (s2:substitution{Extends s2 s1} * list term)
val unify_aux: s1:substitution
            -> u:vars
            -> xs:vars
            -> i:term
            -> goal:term 
            -> option (s2:substitution{Extends s2 s1})

let rec unify_aux s1 u xs i goal =
(* should do a pass of backwards chaining here
   see unifyAndSimpl in logicEngine.fst *)
  match i with
  | Types.Var v1 ->
    (match goal with
	| _ when (subst i s1:term) = subst goal s1 -> Some s1
	| _ when not(contains v1 (freeVars goal)) && contains v1 u ->
	   if inDomain s1 v1
	   then unify_aux s1 u xs (subst i s1) goal
	   else failwith "TODO" (* TODO Some(extendSubst s1 v1 (subst goal s1)) *)
	| _ -> None)
  | Types.Const c1 ->
    (match goal with
	 | _ when i = subst goal s1 -> Some(s1)
	 | Types.Var v2 -> unify_aux s1 xs u goal i
	 | _ -> None)
  | Types.App(f1, tlist1) ->
    (match goal with
	 | Types.Var v2 -> unify_aux s1 xs u goal i
	 (*| Types.App(f2, tlist2) when (f1 = f2) 
	                           && (length tlist1 = length tlist2) ->
         fold_left2 (fun (s:substitution{Extends s s1}) (t1:term) (t2:term) 
		               -> unify_aux s u xs t1 t2)
		   (Some(s1)) tlist1 tlist2 *)
	 | _ -> None)
  (*| Types.SubstrateQueryTerm t0 ->
      option_map (fun s2 -> (s2, substList (map (fun x -> Types.Var x) xs) s2))
        (option_map FStarSubstitutionOfISubstitution
          (substrateQueryTerm_unifyFrom t0 (ISubstitutionOfFStarSubstitution s1) 
		                                (ITermOfFStarTerm goal) ) )
  | Types.SubstrateUpdateTerm t0 ->
      option_map (fun s2 -> (s2, substList (map (fun x -> Types.Var x) xs) s2))
        (option_map FStarSubstitutionOfISubstitution
          (substrateQueryTerm_unifyFrom t0 (ISubstitutionOfFStarSubstitution s1)
		                                (ITermOfFStarTerm goal) ) )*)

let unify s1 u xs i goal =
  match unify_aux s1 u xs i goal with
  | None -> None
  | Some s2 -> Some(s2, substList (map (fun v -> Var(v)) xs) s2)


