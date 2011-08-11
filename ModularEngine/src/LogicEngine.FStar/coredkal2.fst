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

(************************************************)
(* build:                                       *)
(* fstar typeHeaders.fst types.fst coredkal.fst *)
(* or:                                          *)
(* make coredkal                                *)
(************************************************)

module CoreDKAL

(********************)
(* Type definitions *)
(********************)

open Types
open Util
open Typing
open Subst

val memInfostrate : i:term -> k:infostrate -> b:bool{b=true <=> (In i k)}
(* impl todo *)
  
(******************************)
(* Trusted external functions *)
(******************************)
(* Queries to external databases i.e., Substrate *)
type SubstrateSays :: substrate => ISubstrateQueryTerm => E
val check_substrate : s:substrate -> q:ISubstrateQueryTerm -> b:bool{b=true => SubstrateSays s q}
(* impl todo *)

(***********************)
(* Inductive judgments *)
(***********************)
logic function WithPrefix : prefix -> term -> term 
assume forall (i:term). (WithPrefix [] i) = i
assume forall (p:term) (pfx:prefix) (i:term). (WithPrefix (p::pfx) i) = (WithPrefix pfx (App SaidInfon [p; i]))

logic function WithPrefixL : prefix -> list term -> list term
assume forall (pfx:prefix). (WithPrefixL pfx []) = []
assume forall (pfx:prefix) (t:term) (tl:list term). (WithPrefixL pfx (t::tl)) = ((WithPrefix pfx t)::(WithPrefixL pfx tl))
assume forall (p:term) (pfx:prefix) (i:term). (WithPrefix (p::pfx) i) = (App SaidInfon [p; (WithPrefix pfx i)])

val withPrefix: pfx:prefix -> i:term -> j:term{j=(WithPrefix pfx i)}
let rec withPrefix pfx i = match pfx with 
  | [] -> i
  | p::pfx' -> (withPrefix pfx' (App SaidInfon [p; i]))

val stripPrefix: i:term -> pref:prefix -> j:term{(WithPrefix pref j)=i}
(* stripPrefix is a partial function *)

val withPrefixL: pfx:prefix -> i:list term -> j:list term{j=(WithPrefixL pfx i)}

type entails :: substrate => infostrate => varDecl => term => P =
  | Entails_Emp : 
       S:substrate -> K:infostrate -> G:varDecl
    -> pref:prefix
    -> WF S K G
    -> types G (WithPrefix pref (App EmptyInfon [])) Infon
    -> entails S K G (WithPrefix pref (App EmptyInfon []))

  | Entails_Hyp_Knowledge : 
       S:substrate -> K:infostrate -> G:varDecl
    -> i:term{In i K}
    -> WF S K G
    -> entails S K G i

  | Entails_Hyp_Substrate : 
       S:substrate -> K:infostrate -> G:varDecl
    -> q:ISubstrateQueryTerm{SubstrateSays S q}     (* Eventually, we get a signature from the DB about this fact *)
    -> WF S K G
    -> entails S K G (App AsInfon [(SubstrateQueryTerm q)])

  | Entails_Q_Intro : 
       S:substrate -> K:infostrate -> G:varDecl
    -> x:var -> i:term 
    -> entails S K (x::G) i
    -> entails S K G (ForallT x i)

  | Entails_Q_Inst : 
       S:substrate -> K:infostrate -> G:varDecl
    -> v:var -> i:term -> u:term 
    -> i':term{Subst i (Update EmptySubst v u) i'}
    -> entails S K G (ForallT v i)
    -> types G u v.typ
    -> entails S K G i'

  | Entails_And_Intro : 
       S:substrate -> K:infostrate -> G:varDecl
    -> ilist:list term -> pref:prefix
    -> MapL term (entails S K G) (WithPrefixL pref ilist)
    -> entails S K G (WithPrefix pref (App AndInfon ilist))

  | Entails_And_Intro2 : (* Simpler version with binary And *)
       S:substrate -> K:infostrate -> G:varDecl
    -> i:term -> j:term -> pref:prefix
    -> entails S K G (WithPrefix pref i)
    -> entails S K G (WithPrefix pref j)
    -> entails S K G (WithPrefix pref (App AndInfon [i;j]))

  | Entails_And_Elim : 
       S:substrate -> K:infostrate -> G:varDecl
    -> ilist:list term -> i:term{In i ilist} -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon ilist))
    -> entails S K G (WithPrefix pref i)

  | Entails_W_Imp_Intro : 
       S:substrate -> K:infostrate -> G:varDecl
    -> i:term -> j:term -> pref:prefix
    -> entails S K G (WithPrefix pref j)
    -> types G i Infon
    -> entails S K G (WithPrefix pref (App ImpliesInfon [i; j]))

  | Entails_Imp_Elim : 
       S:substrate -> K:infostrate -> G:varDecl
    -> i:term -> j:term -> pref:list term
    -> entails S K G (WithPrefix pref i)
    -> entails S K G (WithPrefix pref (App ImpliesInfon [i;j]))
    -> entails S K G (WithPrefix pref j)

  | Entails_Alpha :
      S:substrate -> K:infostrate -> G:varDecl
    -> x:var -> i:term -> y:var -> i':term{Subst i (Update EmptySubst x (Var y)) i'}
    -> entails S K G (ForallT y i')
    -> entails S K G (ForallT x i)

(*************************)
(* Derivation algorithms *)
(*************************)

(* prove as lemmas *)
assume forall (s1:substitution) (s2:substitution) (s3:substitution). 
            Extends s3 s2 && Extends s2 s1 => Extends s3 s1
assume forall (pref:prefix) (pref':prefix) (t:term) (t':term) (s:substitution). 
             (SubstList pref s pref' && Subst t s t')
            => Subst (WithPrefix pref t) s (WithPrefix pref' t')

type result :: _ = 
    (fun (s:substrate) (k:infostrate) (g:varDecl) (s3:substitution) (pref:prefix) (goal:term) => 
        (goal':term{Subst goal s3 goal'} * pref':prefix{SubstList pref s3 pref'} * entails s k g (WithPrefix pref' goal')))
        (* (goal':term{Subst goal s4 goal'} * pref':prefix{SubstList pref s4 pref'} * entails s k g (WithPrefix pref' goal'))) *)

val doDerive:   S:substrate 
             -> K:infostrate 
             -> G:varDecl
             -> U:varDecl             (* Variables available for unification in goal *)
     	     -> s1:substitution
             -> pref:prefix 
             -> goal:term
             -> option (s2:substitution{Extends s2 s1} *
                          (s3:substitution{Extends s3 s2} ->  (goal':term{Subst goal s3 goal'} *
                                                                 pref':prefix{SubstList pref s3 pref'} *
                                                                 entails S K G (WithPrefix pref' goal'))))

val unify: u:varDecl 
        -> s1:substitution
        -> i:term    
        -> goal:term (* u subsetof (freevars goal) *)
        -> option (s2:substitution{Extends s2 s1})
(* ad hoc implementation of unification
     informally, only variables in u are available for unification.
*)

val tryDerive:  S:substrate 
             -> K:infostrate 
             -> G:varDecl
             -> U:varDecl             (* Variables available for unification in goal *)
     	     -> s1:substitution
             -> pref:prefix 
             -> i:term{In i K}
             -> goal:term
             -> option (s2:substitution{Extends s2 s1} *
                          (s3:substitution{Extends s3 s2} -> (goal':term{Subst goal s3 goal'} *
                                                                pref':prefix{SubstList pref s3 pref'} *
                                                                entails S K G (WithPrefix pref' goal'))))
let tryDerive s k g u s1 pref i goal = 
  let pref_goal = withPrefix pref goal in
    match unify u s1 i pref_goal with 
      | None -> None
      | Some s2 -> 
          let continuation (s3:substitution{Extends s3 s2}) = 
            let goal' = subst goal s3 in  (* Subst goal s3 goal' *)
            let pref' = substList pref s3 in  (* SubstList pref s3 pref' *)
            let pref_goal' = withPrefix pref' goal' in  (* pref_goal'= (WithPrefix pref' goal') *)
              if (pref_goal':term) = i then 
                match checkWF s k g with 
                  | Some pf_wf -> 
                      let pf = Entails_Hyp_Knowledge s k g i pf_wf in 
                        ((goal', pref', pf) : 
                           (goal':term{Subst goal s3 goal'} *
                              pref':prefix{SubstList pref s3 pref'} *
                              entails s k g (WithPrefix pref' goal')))
                  | None -> raise "ill-formed environment"
              else raise "bad substitution" in 
            Some (s2, continuation)

(* result S K G s3 pref goal) *)
let rec doDerive s k g u s1 pref goal = 
  match map_one k (fun (i:term{In i k}) -> tryDerive s k g u s1 pref i goal) with (* should this be done last? *)
    | Some result -> Some result
    | None -> 
        match goal with 
          | App AndInfon [goal_i; goal_j] -> 
              (match doDerive s k g u s1 pref goal_i with 
                 | None -> None
                 | Some((s2, mkpf_i)) -> 
                     match doDerive s k g u s2 pref goal_j with 
                       | None -> None
                       | Some((s3, mkpf_j)) -> 
                           let continuation (s4:substitution{Extends s4 s3}) = 
                             let goal_i', pref_i', pf_i' = mkpf_i s4 in (* from lemma: Extends s4 s2 *) (* pf_i': entails s k g (WithPrefix pref_i' goal_i') *) 
                             let goal_j', pref_j', pf_j' = mkpf_j s4 in (* Extends s4 s3, directly *) (* pf_j': entails s k g (WithPrefix pref_j' goal_j') *)
                               if (pref_i'=pref_j') (* we know: Subst pref s4 pref_i' and  Subst pref s4 pref_j'; but not pref_i'=pref_j' *)
                               then
                                 let pf_and = Entails_And_Intro2 s k g goal_i' goal_j' pref_i' pf_i' pf_j' in
                                 let goal' = App AndInfon [goal_i'; goal_j'] in  (* pf_and: Entails s k g goal'*)
                                   ((goal', pref_i', pf_and)
                                      :(goal':term{Subst goal s4 goal'} * pref':prefix{SubstList pref s4 pref'} * entails s k g (WithPrefix pref' goal')))
                               else raise "bad substitution" in 
                             Some (s3, continuation))
                
          | App SaidInfon [p;j] ->
              (match doDerive s k g u s1 (p::pref) j with
                 | Some ((s2, mkpf_j)) ->  (* s2:substitution{Extends s2 s1} * (s3:subst{Extends s3 s2} -> (goal_j':term{Subst goal_j s3 goal_j'} *
                                              pref_j':prefix{SubstList (p::pref) s3 pref'} *
                                              entails s k g (WithPrefix pref_j' goal_j'))) *)
                     let continuation (s3:substitution{Extends s3 s2}) =
                       let goal_j', pref_j', pf_j' = mkpf_j s3 in (* SubstList (p::pref) s3 pref_j'; we should prove that pref_j' = p'::pref' where (Subst p s3 p') *)
                         match pref_j' with
                           | p'::pref' -> (* should prove that (Subst p s3 p') *)
                               let goal' = App SaidInfon [p'; goal_j'] in
                                 ((goal', pref', pf_j')
                                    : (goal':term{Subst goal s3 goal'} * pref':prefix{SubstList pref s3 pref'} * entails s k g (WithPrefix pref' goal')))
                           | [] -> raise "impos" in (* prove that this case is unreachable by asserting false *) 
                       Some (s2, continuation))

          | _ -> None (* todo *)
              
              
