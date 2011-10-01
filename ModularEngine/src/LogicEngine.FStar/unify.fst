module Unify
open TypeHeaders
open Types
open Subst

(* ad hoc implementation of unification
     informally, only variables in u are available for unification. *)
val unify: s1:substitution
        -> u:vars 
        -> xs:vars
        -> i:term    
        -> goal:term 
        -> option (s2:substitution{Extends s2 s1} * list term)


