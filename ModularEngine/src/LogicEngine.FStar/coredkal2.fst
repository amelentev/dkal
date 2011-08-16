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

val unify: u:varDecl 
        -> s1:substitution
        -> i:term    
        -> goal:term (* u subsetof (freevars goal) *)
        -> option (s2:substitution{Extends s2 s1})
(* ad hoc implementation of unification
     informally, only variables in u are available for unification.
*)

val allSharedPrefixes : u:varDecl
                     -> s1:substitution
                     -> i:term 
                     -> j:term 
                     -> list (s2:substitution{Extends s2 s1} * 
                              pfx2:prefix * 
                              i':term{Subst i s2 (WithPrefix pfx2 i')}  * 
                              j':term{Subst j s2 (WithPrefix pfx2 j')})
(* TODO *)


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

  | Entails_And_Elim : (* + well-typedness condition? *)
       S:substrate -> K:infostrate -> G:varDecl
    -> ilist:list term -> i:term{In i ilist} -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon ilist))
    -> entails S K G (WithPrefix pref i)
    
  | Entails_And_Elim1 : 
       S:substrate -> K:infostrate -> G:varDecl
    -> i1:term -> i2:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon [i1; i2]))
    -> entails S K G (WithPrefix pref i1)
    
  | Entails_And_Elim2 : 
       S:substrate -> K:infostrate -> G:varDecl
    -> i1:term -> i2:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon [i1; i2]))
    -> entails S K G (WithPrefix pref i2)
    
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
																 

(* tries to prove (withPrefix pref goal) using (infon);
   but really what we will eventually prove is
   (withPrefix pref_infon (withPrefix pref goal))
   and we really know (withPrefix pref_infon infon)
*)
val tryDerive:  S:substrate 
             -> K:infostrate 
             -> G:varDecl
             -> U:varDecl             (* Variables available for unification in goal *)
             -> s1:substitution
             -> pref:prefix 
             -> goal:term
             -> infon:term
             -> mkpf_infon:(s4:substitution{Extends s4 s1}
                             -> (infon':term{Subst infon s4 infon'} *
                                 entails S K G infon'))
             -> option (s2:substitution{Extends s2 s1} *
                          (s3:substitution{Extends s3 s2} 
                             -> (goal':term{Subst goal s3 goal'} *
                                 pref':prefix{SubstList pref s3 pref'} *
                                 entails S K G (WithPrefix pref' goal'))))
val tryDeriveWrapper:  S:substrate 
                    -> K:infostrate 
                    -> G:varDecl
                    -> U:varDecl             (* Variables available for unification in goal *)
                    -> s1:substitution
                    -> goal:term
                    -> infon:term{In infon K}
                    -> option (s2:substitution{Extends s2 s1} *
                                 (s3:substitution{Extends s3 s2} 
                                  -> (goal':term{Subst goal s3 goal'} *
                                        entails S K G goal')))

let rec tryDerive s k g u s1 pref goal infon mkpf_infon = 
  match infon with
  | App AndInfon [infon1; infon2] ->
      let mkpf_infon1 (s4:substitution{Extends s4 s1}) =
        let (infon', pf_infon') = mkpf_infon s4 in
        let infon1' = subst infon1 s4 in (* TODO: inefficient, should grab the one from infon', but need an inversion lemma *)
        let infon2' = subst infon2 s4 in
        if infon' = App AndInfon [infon1'; infon2']
        then ((infon1', Entails_And_Elim1 s k g infon1' infon2' [] pf_infon') :                             
                (infon1':term{Subst infon1 s4 infon1'} * entails s k g infon1'))
        else raise "impos" in (* TODO: unreachable, since infon1 and infon2 are q-free (so subst is deterministic) *)
      (match tryDerive s k g u s1 pref goal infon1 mkpf_infon1 with
         | Some res -> Some res
         | None -> 
            let mkpf_infon2 (s4:substitution{Extends s4 s1}) =
              let (infon', pf_infon') = mkpf_infon s4 in
              let infon1' = subst infon1 s4 in
              let infon2' = subst infon2 s4 in
              if infon' = App AndInfon [infon1'; infon2']
              then ((infon2', Entails_And_Elim2 s k g infon1' infon2' [] pf_infon') :                             
                      (infon2':term{Subst infon2 s4 infon2'} * entails s k g infon2'))
              else raise "impos" in 
              tryDerive s k g u s1 pref goal infon1 mkpf_infon1) 
        
  | App ImpliesInfon [i; j] ->
      (match doDerive s k g u s1 [] i with (* for now, []; later change it, cf. suff *)
         | None -> None
         | Some((s2, mkpf_i)) ->
             let mkpf_j (s4:substitution{Extends s4 s2}) =
               let (i', pref_i', pf_i') = mkpf_i s4 in
               let (infon', pf_infon') = mkpf_infon s4 in
               let j' = subst j s4 in
               if infon' = (App ImpliesInfon [i'; j']) 
               then ((j', Entails_Imp_Elim s k g i' j' pref_i' pf_i' pf_infon') :
                       (j':term{Subst j s4 j'} * entails s k g j'))
               else raise "impos" (* TODO: Unreachable *)
             in
               match tryDerive s k g u s2 pref goal j mkpf_j with 
                 | None -> None 
                 | Some ((s3, mkpf)) -> Some (s3, mkpf)) (* Need to repack the existential at a different type *)

  | _ -> 
      let pref_goal = withPrefix pref goal in
        match unify u s1 infon pref_goal with 
          | None -> None
          | Some s2 -> 
              let continuation (s3:substitution{Extends s3 s2}) = 
                let s3_infon, entails_s3_infon = mkpf_infon s3 in 
                let s3_goal = subst goal s3 in  (* Subst goal s3 s3_goal *)
                let s3_pref = substList pref s3 in  (* SubstList pref s3 s3_pref *)
                let s3_pref_goal = withPrefix s3_pref s3_goal in  (* s3_pref_goal = (WithPrefix s3_pref s3_goal) *)
                  if (s3_pref_goal:term) = s3_infon then 
                    ((s3_goal, s3_pref, entails_s3_infon) : 
                       (goal':term{Subst goal s3 goal'} *
                          pref':prefix{SubstList pref s3 pref'} *
                          entails s k g (WithPrefix pref' goal')))
                  else raise "bad substitution" in 
                Some (s2, continuation) 

and tryDeriveWrapper s k g u s1 goal infon = 
  let callTryDerive (arg :
                       (s2:substitution{Extends s2 s1} *
                          pref:prefix *
                          goal':term{Subst goal s2 (WithPrefix pref goal')} *
                          infon':term{Subst infon s2 (WithPrefix pref infon')}))
      : option (s3:substitution{Extends s3 s1} * 
                  (s4:substitution{Extends s4 s3} -> 
                    (goal_res:term{Subst goal s4 goal_res} * entails s k g goal_res))) = 

    let (s2, pref, goal', infon') = arg in 
    let mkpf_infon (s3:substitution{Extends s3 s2}) : 
        (infon'':term{Subst infon' s3 infon''} * entails s k g infon'') = 
      (match checkWF s k g with
         | None -> raise "ill-formed environment"
         | Some pf_wf -> (* this is P-kinded, I don't get it *)	    
	     let infon'' = subst infon' s3 in 
	       if (infon:term) = infon''
	       then (infon'', 
		     Entails_Hyp_Knowledge s k g infon pf_wf)
	       else (* TODO: Must use Entails_Alpha to conclude *)
                 raise "not a closed infon in infostrate") in
      match tryDerive s k g u s2 pref goal' infon' mkpf_infon with 
        | None -> None
        | Some((s3, mkpf)) -> (* Extends s3 s2 *)
            let mkpf_pair (s4:substitution{Extends s4 s3}) = 
              let (goal'', pref', entails_pf) = mkpf s4 in 
                (* Have:
                      Subst goal' s4 goal'' 
                      SubstList pref s4 pref' 
                      entails s k g (WithPrefix pref' goal'') *)
              let goal_res = withPrefix pref' goal'' in 
              let s4_goal = subst goal s4 in 
                (* Need: 
                      goal_res : Subst goal s4 goal_res
                      entails s k g goal_res *)
                if (s4_goal : term) = goal_res 
                then ((goal_res, entails_pf):
                        (goal_res:term{Subst goal s4 goal_res} * entails s k g goal_res))
                else raise "impos"
                  (* TODO: Should be able to use this and q-free goal to conclude unreachable
                     Subst goal s2 (WithPrefix pref goal') *) in 
              Some (s3, mkpf_pair) in 
  let prefixes = allSharedPrefixes u s1 goal infon in 
    map_one prefixes callTryDerive 
      

(* result S K G s3 pref goal) *)
and doDerive s k g u s1 pref goal = 
  match goal with
    | ForallT x i -> 
        if not (pref = [])
        then raise "impos"
        else 
          if containsVar g x 
          then (* alpha convert first *)  None
          else
            (match doDerive s k (x::g) u s1 pref i with 
               | None -> None
               | Some ((s2, mkpf)) -> 
                   let cont (s3:substitution{Extends s3 s2}) = 
                     let (i', _, entails_i') = mkpf s3 in 
                       (* Have: Subst i s3 i' 
                          entails s k (x::g) i'
                          
                          Need: goal':term
                          Subst (ForallT x i) s3 goal' 
                          entails s k g goal' *)
                     let goal' = ForallT x i' in 
                       if contains x (freeVarsSubst s3)
                       then raise "bad subst" (* TODO: carry invariant that (FreeVarsSubst s) disjoint from g *)
                       else 
                         let entails_goal' : entails s k g goal' =
                           Entails_Q_Intro s k g x i' entails_i' in 
                           ((goal', [], entails_goal') :
                              (goal':term{Subst goal s3 goal'} *
                                 pref':prefix{SubstList pref s3 pref'} *
                                 entails s k g (WithPrefix pref' goal'))) in 
                     Some (s2, cont))
            
    | App EmptyInfon [] -> 
        let continuation (s2:substitution{Extends s2 s1}) =
          let pref' = substList pref s2 in
            match checkWF s k g, 
              doType g (WithPrefix pref' (App EmptyInfon [])) with 
                | Some pf_wf, Some((Infon, pf_typ)) ->
                    ((goal, pref', Entails_Emp s k g pref' pf_wf pf_typ) :
                       (goal':term{Subst goal s2 goal'} *
                          pref':prefix{SubstList pref s2 pref'} *
                          entails s k g (WithPrefix pref' goal')))
                | None, _ -> raise "ill-formed environment"
                | _, None -> raise "ill-typed term"
        in Some(s1, continuation) 

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
                           let pf_and = Entails_And_Intro2 s k g
                             goal_i' goal_j' pref_i' pf_i' pf_j' in
                           let goal' = App AndInfon [goal_i'; goal_j'] in  (* pf_and: Entails s k g goal'*)
                             ((goal', pref_i', pf_and)
                                :(goal':term{Subst goal s4 goal'} *
                                    pref':prefix{SubstList pref s4 pref'} *
                                    entails s k g (WithPrefix pref' goal')))
                         else raise "bad substitution" in 
                       Some (s3, continuation))
          (*
            | App AsInfon [SubstrateQueryTerm q] ->
            (match pref with
            | [] ->
            (match check_substrate s q with
            | false -> None
            | true ->
            let continuation (s2:substitution{Extends s2 s1}) =
            let goal' = subst goal s2 in
            match checkWF s k g, goal' with
            | None, _ -> raise "ill-formed environment"
            | Some pf_wf, App AsInfon [SubstrateQueryTerm q'] ->
            let pf = Entails_Hyp_Substrate s k g q pf_wf in
	  (* I want to conclude with goal', but I only have
	    the proof of q, not the proof of goal' = AsInfon(q') *)
	  (* This is (again) an instance of the substitution rule *)
            let pref' = substList pref s2 in
            ((goal', pref', pf) 
            :(goal':term{Subst goal s2 goal'} *
            pref':prefix{SubstList pref s2 pref'} *
            entails s k g (WithPrefix pref' goal')))
	    | _ -> raise "impos"
            in
            Some(s1, continuation) )
            | _ -> raise "asInfon(...) under prefix")
          *)
    | App ImpliesInfon [goal_i; goal_j] ->
        (match doDerive s k g u s1 pref goal_j with
           | None -> None
           | Some((s2, mkpf_j)) ->
               let continuation (s3:substitution{Extends s3 s2}) =
                 let goal_j', pref_j', pf_j' = mkpf_j s3 in
                 let goal_i' = subst goal_i s3 in
                   match doType g goal_i' with
                     | Some((Infon, pf_typ)) ->
                         let pf_imp = Entails_W_Imp_Intro s k g 
                           goal_i' goal_j' pref_j' pf_j' pf_typ in
                         let goal' = App ImpliesInfon [goal_i'; goal_j'] in
                           ((goal', pref_j', pf_imp)
                              : (goal':term{Subst goal s3 goal'} *
                                   pref':prefix{SubstList pref s3 pref'} *
                                   entails s k g (WithPrefix pref' goal')))
                     | None -> raise "ill-typed term" in
                 Some (s2, continuation))
          
          
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
	  
    | _ -> 
        let pref_goal = withPrefix pref goal in 
        let call_tryDerive (infon:term{In infon k}) 
            : option (s2:substitution{Extends s2 s1} *
                        (s3:substitution{Extends s3 s2} -> (goal':term{Subst goal s3 goal'} *
                                                              pref':prefix{SubstList pref s3 pref'} *
                                                              entails s k g (WithPrefix pref' goal')))) = 
	  
          match tryDeriveWrapper s k g u s1 pref_goal infon with 
            | None -> None 
            | Some ((s2, mk_pref_goal_pair)) -> 
                let mk_pref_goal (s3:substitution{Extends s3 s2}) =
                  let pref_goal', entails_pref_goal' = mk_pref_goal_pair s3 in 
                    (* Subst pref_goal s3 pref_goal' *)
                    (* entails s k g pref_goal' *)
                    (* We know: pref_goal' = (s3 (WithPrefix pref goal)) *)
                  let pref' = substList pref s3 in (* SubstList pref s3 pref' *)
                  let goal' = subst goal s3 in 
                  let goal'' = stripPrefix pref_goal' pref' in  
                    (* entails s k g (WithPrefix pref' goal'') *)
                    (* Need: Subst goal s3 goal' 
                       and:  SubstList pref s3 pref'
                       and:  entails s k g (WithPrefix pref' goal') *)
                    if (goal' : term)=goal'' 
                    then 
                      ((goal', pref', entails_pref_goal'): 
                         (goal':term{Subst goal s3 goal'} *
                            pref':prefix{SubstList pref s3 pref'} *
                            entails s k g (WithPrefix pref' goal')))
                    else raise "Impos" (* TODO: if goal is q-free we're done *)  in 
                  Some (s2, mk_pref_goal) in
          map_one k call_tryDerive 

(* with (\* should this be done last? *\) *)
(*           | Some result -> Some result *)
(*           | None -> None *)
            
