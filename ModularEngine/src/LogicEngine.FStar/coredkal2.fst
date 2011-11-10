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

module InfonLogic
open TypeHeaders
open Types
open Util
open Subst
open Typing
open Unify

(*********************************************************************************)
(* Quotation prefixes *)
(*********************************************************************************)
logic function WithPrefix : prefix -> term -> term 
assume forall (i:term). (WithPrefix [] i) = i
assume forall (p:term) (pfx:prefix) (i:term). 
  (WithPrefix (p::pfx) i) = (WithPrefix pfx (App SaidInfon [p; i]))
val withPrefix: pfx:prefix -> i:term -> j:term{j=(WithPrefix pfx i)}
let rec withPrefix pfx i = match pfx with 
  | [] -> i
  | p::pfx' -> (withPrefix pfx' (App SaidInfon [p; i]))

(*********************************************************************************)
(* Alpha conversion (for polyterms in infostrate) *)
(*********************************************************************************)
type alphaEquiv :: polyterm => polyterm => P =
  | AEQ_Mono: i:term -> alphaEquiv (MonoTerm i) (MonoTerm i)

  | AEQ_Poly: 
       xs:vars -> t:term -> ys:vars 
    -> polytyping [] (ForallT xs t)
    -> wfG ys 
    -> varsTyEq xs ys
    -> alphaEquiv (ForallT xs t) 
                  (ForallT ys (Subst t (MkSubst xs (AsTerms ys))))

val alphaConvert: i:polyterm -> (j:polyterm * alphaEquiv i j)
let alphaConvert i = match i with 
  | MonoTerm t -> (i, AEQ_Mono t)
  | ForallT xs t -> 
      match doPolyTyping [] i with 
        | MkPartial ityping -> 
            let ys, pfzip = freshVars xs in
              match decideWFG ys with 
                | None -> raise "Impos"
                | Some wf_ys -> 
                    let s = mkSubst xs (asTerms ys) in 
                    let t' = subst t s in 
                      (ForallT ys t', AEQ_Poly xs t ys ityping wf_ys pfzip)
  
(*********************************************************************************)
(* Definition of DKAL entailment *)
(*********************************************************************************)
type entails :: substrate => infostrate => vars => term => P =
  | Entails_Emp : 
       S:substrate -> K:infostrate -> G:vars
    -> pref:prefix
    -> typing G (WithPrefix pref (App EmptyInfon [])) Infon
    -> entails S K G (WithPrefix pref (App EmptyInfon []))

  | Entails_And_Intro : (* Simpler version with binary And *)
       S:substrate -> K:infostrate -> G:vars
    -> i:term -> j:term -> pref:prefix
    -> entails S K G (WithPrefix pref i)
    -> entails S K G (WithPrefix pref j)
    -> entails S K G (WithPrefix pref (App AndInfon [i;j]))
    
  | Entails_And_Elim1 : 
       S:substrate -> K:infostrate -> G:vars
    -> i1:term -> i2:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon [i1; i2]))
    -> entails S K G (WithPrefix pref i1)
    
  | Entails_And_Elim2 : 
       S:substrate -> K:infostrate -> G:vars
    -> i1:term -> i2:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon [i1; i2]))
    -> entails S K G (WithPrefix pref i2)
    
  | Entails_W_Imp_Intro : 
       S:substrate -> K:infostrate -> G:vars
    -> i:term -> j:term -> pref:prefix
    -> entails S K G (WithPrefix pref j)
    -> typing G i Infon
    -> entails S K G (WithPrefix pref (App ImpliesInfon [i; j]))

  | Entails_Imp_Elim : 
       S:substrate -> K:infostrate -> G:vars
    -> i:term -> j:term -> pref:list term
    -> entails S K G (WithPrefix pref i)
    -> entails S K G (WithPrefix pref (App ImpliesInfon [i;j]))
    -> entails S K G (WithPrefix pref j)

  | Entails_Hyp_Substrate :
       S:substrate -> K:infostrate -> G:vars
    -> q:ISubstrateQueryTerm{SubstrateSays S q}     (* Eventually, we get a signature from the DB about this fact *)
    -> entails S K G (App AsInfon [(SubstrateQueryTerm q)])

  | Entails_Q_Elim:
       S:substrate -> K:infostrate -> G:vars
    -> xs:vars -> t:term -> is:list term 
    -> polyentails S K G (ForallT xs t)
    -> Zip term var (fun (i:term) (x:var) => typing G i x.typ) is xs
    -> entails S K G (Subst t (MkSubst xs is))

  | Entails_Poly:
       S:substrate -> K:infostrate -> G:vars
    -> i:term
    -> polyentails S K G (MonoTerm i)
    -> entails S K G i

and polyentails :: substrate => infostrate => vars => polyterm => P = 
  | Entails_Hyp_Knowledge :
       S:substrate -> K:infostrate -> G:vars
    -> i:polyterm{In i K}
    -> i':polyterm{In i K}
    -> polytyping [] i
    -> alphaEquiv i i'
    -> polytyping G i'
    -> polyentails S K G i'

  | Entails_Q_Intro :
       S:substrate -> K:infostrate -> G:vars
    -> i:term 
    -> wfG G
    -> entails S K G i 
    -> polyentails S K [] (ForallT G i)

  | Entails_Mono: 
      S:substrate -> K:infostrate -> G:vars
    -> i:term
    -> entails S K G i
    -> polyentails S K G (MonoTerm i)

(*************************)
(* Derivation algorithm  *)
(*************************)
(* prove as lemmas *)
assume Extends_trans: forall (s1:substitution) (s2:substitution) (s3:substitution). 
            Extends s3 s2 && Extends s2 s1 => Extends s3 s1
assume Prefix_subst_commute: forall (pref:prefix) (t:term) (s:substitution). 
          (Subst (WithPrefix pref t) s)=(WithPrefix (SubstList pref s) (Subst t s))

type kresult :: _ = (fun (s:substrate) (k:infostrate) (g:vars) (pref:prefix) (goal:term) (s2:substitution) => 
    Partial (entails s k g (Subst (WithPrefix pref goal) s2)))
type kpolyresult :: _ = (fun (s:substrate) (k:infostrate) (g:vars) (goal:polyterm) (s2:substitution) => 
    Partial (polyentails s k g (PolySubst goal s2)))
		
val doDerive:   S:substrate 
             -> K:infostrate 
             -> G:vars
             -> U:vars
             -> s1:substitution
             -> pref:prefix 
             -> goal:term
             -> option (s2:substitution{Extends s2 s1} *
                          (s3:substitution{Extends s3 s2} -> Partial (entails S K G (Subst (WithPrefix pref goal) s3))))

val tryDeriveAlpha:    S:substrate
                    -> K:infostrate
                    -> G:vars
                    -> U:vars 
                    -> s1:substitution
                    -> pref:prefix
                    -> goal:term
                    -> infon:polyterm{In infon K}
                    -> option (s2:substitution{Extends s2 s1} *
                                 (s3:substitution{Extends s3 s2} -> Partial (entails S K G (Subst (WithPrefix pref goal) s3))))

val tryDerive:  S:substrate
             -> K:infostrate
             -> G:vars
             -> U:vars
             -> s1:substitution
             -> pref:prefix
             -> goal:term
             -> infon:polyterm        (* closed polyterm *)
             -> mkpf_infon:(s4:substitution{Extends s4 s1} -> kpolyresult S K G infon s4)
             -> option (s2:substitution{Extends s2 s1} *
                          (s3:substitution{Extends s3 s2} -> Partial (entails S K G (Subst (WithPrefix pref goal) s3))))

let rec doDerive s k g u s1 pref goal =
  match goal with
    | App EmptyInfon [] ->
        let mkpf (s2:substitution{Extends s2 s1}) : kresult s k g pref goal s2 = 
          let pref' = substList pref s2 in 
          let goal' = withPrefix pref' (App EmptyInfon []) in 
          let ty, pf_typing = doTyping g goal' in 
            match ty with 
              | Infon -> MkPartial (Entails_Emp s k g pref' pf_typing)
              | _ -> raise "Unreachable" 
        in Some(s1, mkpf)

    | App AndInfon [goal_i; goal_j] ->
        (match doDerive s k g u s1 pref goal_i with
           | None -> None
           | Some((s2, mkpf_i)) ->
               match doDerive s k g u s2 pref goal_j with
                 | None -> None
                 | Some((s3, mkpf_j)) ->
                     let mkpf (s4:substitution{Extends s4 s3}) : kresult s k g pref goal s4 = 
                       match mkpf_i s4, mkpf_j s4 with (* from lemma: Extends s4 s2 *)
                         | MkPartial pf_i, MkPartial pf_j -> (* pf_x : (entails s k g (Subst (WithPrefix pref goal_x) s4)) *)
                             let pf_and = (Entails_And_Intro s k g 
                                             (subst goal_i s4)
                                             (subst goal_j s4)
                                             (substList pref s4)
                                             pf_i pf_j) in 
                               MkPartial pf_and
                     in Some (s3, mkpf))
 
    | App ImpliesInfon [goal_i; goal_j] ->
        (match doDerive s k g u s1 pref goal_j with
           | None -> None
           | Some((s2, mkpf_j)) ->
               let mkpf (s3:substitution{Extends s3 s2}) : kresult s k g pref goal s3 = 
                 match mkpf_j s3 with 
                   | MkPartial pf_j -> 
                       let goal_i' = subst goal_i s3 in
                       match doTyping g goal_i' with 
                         | (Infon, typing_i) -> 
                             let pf_imp = (Entails_W_Imp_Intro s k g 
                                             goal_i'
                                             (subst goal_j s3) 
                                             (substList pref s3) 
                                             pf_j
                                             typing_i) in 
                               MkPartial pf_imp 
                         | _ -> raise "Ill-typed formula"
               in Some (s2, mkpf))

    | App SaidInfon [p;j] -> 
        (match doDerive s k g u s1 (p::pref) j with (* Ugly---expansion necessary because of no structural subtyping on tuples? *)
           | None -> None
           | Some ((s2, mkpf_j)) -> Some (s2, mkpf_j))
               
    | App AsInfon [SubstrateQueryTerm q] ->
        (match pref with
           | [] -> 
               let mkpf (s2:substitution{Extends s2 s1}) : kresult s k g pref goal s2 =
                 let q' = substQuery q s2 in  (* returns a term, not a SubstrateQueryTerm *)
                   if check_substrate s q' 
                   then MkPartial (Entails_Hyp_Substrate s k g q')
                   else raise "Substrate query failed" 
               in Some (s1, mkpf)
           | _ -> None) (* AsInfon not allowed under prefix *)
	  
    | _ -> map_one k (tryDeriveAlpha s k g u s1 pref goal)

and tryDeriveAlpha s k g u s1 pref goal infon = 
  let infon', aeq = alphaConvert infon in 
    match doPolyTyping [] infon, doPolyTyping g infon' with 
      | MkPartial typing_infon, MkPartial typing_infon' -> 
          let mkpf (s2:substitution{Extends s2 s1}) : kpolyresult s k g infon' s2 = 
            match infon' with 
              | ForallT xs body -> 
                  if check_disjoint (freeVarsSubst s2) xs
                  then 
                    let body' = subst body s2 in 
                      if body = body'
                      then MkPartial (Entails_Hyp_Knowledge s k g infon infon' typing_infon aeq typing_infon')
                      else raise "TODO: prove using Domain(s2) = u, disjoint from (g \union xs)=Vars body"
                  else raise "Substitution domain"
                    
              | MonoTerm body -> 
                  let body' = subst body s2 in 
                    if body = body'
                    then MkPartial (Entails_Hyp_Knowledge s k g infon infon' typing_infon aeq typing_infon') 
                    else raise "TODO: prove using Domain(s2) = u, disjoint from (g \union xs)=Vars body"
          in 
            tryDerive s k g u s1 pref goal infon' mkpf

and tryDerive s k g u s1 pref goal infon mkpf_infon = 
  match infon with
    | ForallT xs body -> 
        let pref_goal = withPrefix pref goal in 
        (match unify s1 u xs body pref_goal with
           | None -> None 
           | Some((s2, insts)) -> 
               let mkpf (s3:substitution{Extends s3 s2}) : kresult s k g pref goal s3 = 
                 let insts = map (fun i -> subst i s3) insts in 
                   if not (check_disjoint (freeVarsSubst s3) xs)
                   then raise "TODO: Substitution domain" 
                   else
                     match doTypingList g insts xs, mkpf_infon s3 with
                       | None, _ -> raise "Ill-typed unification"
                       | Some typing_insts, MkPartial pf_infon -> 
                           let sinst = mkSubst xs insts in 
                           let body' = subst (subst body s3) sinst in 
                           let pref_goal' = subst pref_goal s3 in 
                             if (pref_goal':term)=body' 
                             then MkPartial (Entails_Q_Elim s k g xs (subst body s3) insts pf_infon typing_insts)
                             else raise "Unification error" (* TODO: prove unification correct *)
               in Some (s2, mkpf))
          
    | MonoTerm (App AndInfon [infon1; infon2]) ->
        let monoinfon = App AndInfon [infon1; infon2] in
        let mkpf_infon1 (s4:substitution{Extends s4 s1}) : kpolyresult s k g (MonoTerm infon1) s4 = 
          match mkpf_infon s4 with 
            | MkPartial pf_infon ->
                MkPartial (Entails_Mono s k g (subst infon1 s4)
                             (Entails_And_Elim1 s k g (subst infon1 s4) (subst infon2 s4) [] 
                                (Entails_Poly s k g (subst monoinfon s4) pf_infon))) in
        let mkpf_infon2 (s4:substitution{Extends s4 s1}) : kpolyresult s k g (MonoTerm infon2) s4 = 
          match mkpf_infon s4 with 
            | MkPartial pf_infon ->
                MkPartial (Entails_Mono s k g (subst infon2 s4)
                             (Entails_And_Elim2 s k g (subst infon1 s4) (subst infon2 s4) [] 
                                (Entails_Poly s k g (subst monoinfon s4) pf_infon))) in
          (match tryDerive s k g u s1 pref goal (MonoTerm infon1) mkpf_infon1 with
             | Some res -> Some res
             | None -> tryDerive s k g u s1 pref goal (MonoTerm infon2) mkpf_infon2)
    
	| MonoTerm (App AndInfon _) -> failwith "TODO And for 0, 1, 3 or more terms"
	        
    | MonoTerm (App ImpliesInfon [i; j]) ->
        let monoinfon = App ImpliesInfon [i; j] in
          (match doDerive s k g u s1 [] i with (* TODO: This is no longer backwards chaining. Fix. *)
             | None -> None
             | Some((s2, mkpf_i)) ->
                 let mkpf_j (s4:substitution{Extends s4 s2}) : kpolyresult s k g (MonoTerm j) s4 = 
                   match mkpf_infon s4, mkpf_i s4 with 
                     | MkPartial pf_infon, MkPartial pf_i -> 
                         MkPartial (Entails_Mono s k g (subst j s4)
                                      (Entails_Imp_Elim s k g 
                                         (subst i s4) (subst j s4) [] 
                                         pf_i 
                                         (Entails_Poly s k g (subst monoinfon s4) pf_infon)))
                 in 
                   match tryDerive s k g u s2 pref goal (MonoTerm j) mkpf_j with
                     | None -> None
                     | Some ((s3, mkpf)) -> Some (s3, mkpf)) (* Need to repack the existential at a different type *)

  | MonoTerm i -> 
      let pref_goal = withPrefix pref goal in
        match unify s1 u [] i pref_goal with
          | Some ((s2,[])) ->
              let mkpf (s3:substitution{Extends s3 s2}) : kresult s k g pref goal s3 =
                match mkpf_infon s3 with
                  | MkPartial pf_infon ->
                      let i' = subst i s3 in 
                      if (i':term) = (subst pref_goal s3)
                      then MkPartial (Entails_Poly s k g i' pf_infon)
                      else raise "Unification error" 
              in Some (s2, mkpf)
          | _ -> None

val deriveQuant: U:vars             (* Variables available for unification in goal (free in goal) *)
              -> S:substrate 
              -> K:infostrate 
              -> G:vars
              -> s0:substitution(* {(Domain s0) subset G} *)
              -> goal:term
              -> option (s:substitution(* {(Domain s)=U}  s extends s0 *) *
                         polyentails S K [] (ForallT G (Subst goal s)))
let rec deriveQuant u s k g s0 goal = 
  match decideWFG g with 
    | None -> None
    | Some wfg -> 
        match doDerive s k g u s0 [] goal with 
          | None -> None
          | Some ((s1, mkpf)) -> 
              match mkpf s1 with 
                | MkPartial pfs1 -> 
                    let res = Entails_Q_Intro s k g (subst goal s1) wfg pfs1 in 
                      Some ((s1, res))
                | _ -> None

  (*******************)
  (* Wrappers for F# *)
  (*******************) 
  (* mostly taken from logicEngine.fst *)

  let checkJustification (_signatureProvider : option ISignatureProvider) (evidence: term) = 
    failwith "checkJustification not supported in FStar Engine"

  let checkJustificationWrapper (s:option ISignatureProvider) (e:term) =
    OptionOfPrimsOption (checkJustification s e)

  val dropProof : 'a::* -> 'b::('a => P) -> (option (x:'a * 'b x) -> list 'a)
  let dropProof = function
  | None -> [ ]
  | Some((a, b)) -> [ a ]

  (* Obtain a list of Substitution with accompanying side conditions (AsInfon *)
  (* ITerms) [call to doDerive]. Then return only those Substitutions that    *)
  (* satisfy all their side conditions [call to substrateDispatcher_solve].   *)
  let derive (_infostrate: option infostrate) (target: term(*Infon*)) 
             (substs: list substitution) : list substitution =
  (* drop the constructive proof to send the substitutions to F#              *)
    match _infostrate with None -> [] | Some k ->
      let variables = freeVars target in
      collect
        (fun subst ->
          match (deriveQuant variables () k variables subst target) with
		  | None -> [ ]
		  | Some((a, b)) -> [ a ])
      substs
      
  let deriveWrapper (_i: option infostrate) (t: term) (s: list substitution) 
                    (*: (TranslationfromFStar.listFS substitution) *)=
    (ListOfPrimsList (derive _i t s)) 

  let deriveJustification 
      (_infostrate: option IInfostrate) (target: term(*Infon*)) (proofTemplate: term(*Evidence*))
      (substs: list substitution) : list substitution =
	failwith "deriveJustification not supported in FStar Engine"
      
  let deriveJustificationWrapper (_i: option IInfostrate) (i: term) 
                                 (p: term) (s: list substitution) 
                                 (*: listFS substitution*) =
    ListOfPrimsList (deriveJustification _i i p s)
