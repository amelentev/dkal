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
open Types
open Util
open Subst
open Typing
open Unify
open HilbertianQIL

(* val checkVarsTyEq : xs:vars -> ys:vars -> option (varsTyEq xs ys) *)
let checkVarsTyEq xs ys = zip_e<var, var, varTyEq> (fun x y -> (x.typ : typ)=y.typ) xs ys

val alphaConvertWith: i:polyterm -> ys:vars -> option (j:polyterm * alphaEquiv i j)
let alphaConvertWith i ys = match i with 
  | MonoTerm t -> Some (i, AEQ_Mono t)
  | ForallT xs t -> 
      match doPolyTyping [] i with 
        | MkPartial ityping -> 
            match checkVarsTyEq xs ys, decideWFG ys with 
              | Some pfzip, Some wf_ys -> 
                  let s = mkSubst xs (asTerms ys) in 
                  let t' = subst t s in 
                    Some ((ForallT ys t', AEQ_Poly xs t ys ityping wf_ys pfzip))
              | _ -> None
                  
                  
val alphaConvert: i:polyterm -> (j:polyterm * alphaEquiv i j)
let rec alphaConvert i = match i with 
  | MonoTerm t -> (i, AEQ_Mono t)
  | ForallT xs t -> 
      (match doPolyTyping [] i with 
         | MkPartial ityping -> 
             let ys, pfzip = freshVars xs in
               match decideWFG ys with 
                 | None -> raise "Impos"
                 | Some wf_ys -> 
                     let s = mkSubst xs (asTerms ys) in 
                     let t' = subst t s in 
                       (ForallT ys t', AEQ_Poly xs t ys ityping wf_ys pfzip))
  | JustifiedPoly p q d -> 
      let q', pf = alphaConvert q in 
      let j = JustifiedPoly p q' d in 
        (j, AEQ_Justified p q d q' pf)


(*************************)
(* Derivation algorithm  *)
(*************************)
(* prove as lemmas *)
assume Extends_refl: forall (s1:substitution). Extends s1 s1
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
                    -> i:infon{In i K}
                    -> option (s2:substitution{Extends s2 s1} *
                                 (s3:substitution{Extends s3 s2} -> Partial (entails S K G (Subst (WithPrefix pref goal) s3))))

val tryDerive:  S:substrate
             -> K:infostrate
             -> G:vars
             -> U:vars
             -> s1:substitution
             -> pref:prefix
             -> goal:term
             -> i:polyterm        (* closed polyterm *)
             -> mkpf_infon:(s4:substitution{Extends s4 s1} -> kpolyresult S K G i s4)
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

and tryDeriveAlpha s k g u s1 pref goal xinfon = 
  let xinfon', aeq = alphaConvert xinfon in 
    match doPolyTyping [] xinfon, doPolyTyping g xinfon' with 
      | MkPartial typing_infon, MkPartial typing_infon' -> 
          let mkpf (s2:substitution{Extends s2 s1}) : kpolyresult s k g xinfon' s2 = 
            let xinfon'' = polysubst xinfon' s2 in 
              if xinfon'=xinfon'' 
              then MkPartial (Entails_Hyp_Knowledge s k g xinfon xinfon' typing_infon aeq typing_infon')
              else raise "TODO: prove using Domain(s2) = u, disjoint from (g \union xs)=Vars body"
          in 
            tryDerive s k g u s1 pref goal xinfon' mkpf

and tryDerive s k g u s1 pref goal xinfon mkpf_infon = 
  match xinfon with
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

    | JustifiedPoly p xinfon' e -> 
        let mkpf_infon' (s2:substitution{Extends s2 s1}) : kpolyresult s k g xinfon' s2 = 
          match mkpf_infon s2 with
            | MkPartial pf_infon -> MkPartial (Entails_ElimJust s k g (subst p s2) (polysubst xinfon' s2) (subst e s2) pf_infon) in
          tryDerive s k g u s1 pref goal xinfon' mkpf_infon'
          
    | MonoTerm (App AndInfon [xinfon1; xinfon2]) ->
        let monoinfon = App AndInfon [xinfon1; xinfon2] in
        let mkpf_infon1 (s4:substitution{Extends s4 s1}) : kpolyresult s k g (MonoTerm xinfon1) s4 = 
          match mkpf_infon s4 with 
            | MkPartial pf_infon ->
                MkPartial (Entails_Mono s k g (subst xinfon1 s4)
                             (Entails_And_Elim1 s k g (subst xinfon1 s4) (subst xinfon2 s4) [] 
                                (Entails_Poly s k g (subst monoinfon s4) pf_infon))) in
        let mkpf_infon2 (s4:substitution{Extends s4 s1}) : kpolyresult s k g (MonoTerm xinfon2) s4 = 
          match mkpf_infon s4 with 
            | MkPartial pf_infon ->
                MkPartial (Entails_Mono s k g (subst xinfon2 s4)
                             (Entails_And_Elim2 s k g (subst xinfon1 s4) (subst xinfon2 s4) [] 
                                (Entails_Poly s k g (subst monoinfon s4) pf_infon))) in
          (match tryDerive s k g u s1 pref goal (MonoTerm xinfon1) mkpf_infon1 with
             | Some res -> Some res
             | None -> tryDerive s k g u s1 pref goal (MonoTerm xinfon2) mkpf_infon2)
    
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
          | _ -> 
              (match i with
                 | App JustifiedInfon [p;i';d] ->
                     let mkpf_i' (s2:substitution{Extends s2 s1}) : kpolyresult s k g (MonoTerm i') s2 = 
                       match mkpf_infon s2 with 
                         | MkPartial pf_infon ->
                             MkPartial (Entails_Mono s k g (subst i' s2)
                                          (Entails_J_Elim s k g (subst p s2) (subst i' s2) (subst d s2) [] 
                                             (Entails_Poly s k g (subst i s2) pf_infon))) in
                       tryDerive s k g u s1 pref goal (MonoTerm i') mkpf_i'
                 | _ -> None)
                


val deriveQuant: U:vars             (* Variables available for unification in goal (free in goal) *)
              -> S:substrate 
              -> K:infostrate 
              -> s0:substitution{Includes U (Domain s0)}
              -> pgoal:polyterm
              -> option (s:substitution{Includes U (Domain s)} *
                         polyentails S K [] (PolySubst (PolySubst pgoal s0) s))
let rec deriveQuant u s k s0 pgoal = 
  let pgoal' = polysubst pgoal s0 in
    match pgoal' with 
      | MonoTerm goal -> 
          (match doDerive s k [] u s0 [] goal with
             | None -> None
             | Some ((s1, mkpf)) -> 
                 (match mkpf s1 with 
                    | MkPartial pfs1 -> 
                        if (includes u (domain s1))
                        then let res = Entails_Mono s k [] (subst goal s1) pfs1 in 
                          Some (s1, res)
                        else None))
      | ForallT g goal -> 
          (match decideWFG g with 
             | None -> None
             | Some wfg -> 
                 (match doDerive s k g u s0 [] goal with
                    | None -> None
                    | Some ((s1, mkpf)) -> 
                        match mkpf s1 with 
                          | MkPartial pfs1 -> 
                              if   (check_disjoint (freeVarsSubst s1) g)
                                && (includes u (domain s1))  (* TODO: kill *)
                              then 
                                let res = Entails_Q_Intro s k g (subst goal s1) wfg pfs1 in 
                                  Some (s1, res)
                              else None))

      | JustifiedPoly p i e -> 
          map_one k (fun (i:infon{In i k}) -> match i with 
                       | JustifiedPoly q j f -> raise "NYI: Matching JustifiedPoly"
                       | _ -> None)
            

                              
(* (\*******************\) *)
(* (\* Wrappers for F# *\) *)
(* (\*******************\)  *)
(* (\* mostly taken from logicEngine.fst *\) *)

(* (\*   let checkJustification (_signatureProvider : option ISignatureProvider) (evidence: term) =  *\) *)
(* (\*     failwith "checkJustification not supported in FStar Engine" *\) *)

(* (\*   let checkJustificationWrapper (s:option ISignatureProvider) (e:term) = *\) *)
(* (\*     OptionOfPrimsOption (checkJustification s e) *\) *)

(*   val dropProof : 'a::* -> 'b::('a => P) -> (option (x:'a * 'b x) -> list 'a) *)
(*   let dropProof = function *)
(*   | None -> [ ] *)
(*   | Some((a, b)) -> [ a ] *)

(*   (\* Obtain a list of Substitution with accompanying side conditions (AsInfon *\) *)
(*   (\* ITerms) [call to doDerive]. Then return only those Substitutions that    *\) *)
(*   (\* satisfy all their side conditions [call to substrateDispatcher_solve].   *\) *)
(*   let derive (_infostrate: option infostrate) (target: term(\*Infon*\))  *)
(*              (substs: list substitution) : list substitution = *)
(*   (\* drop the constructive proof to send the substitutions to F#              *\) *)
(*     match _infostrate with None -> [] | Some k -> *)
(*       let variables = freeVars target in *)
(*       collect *)
(*         (fun subst -> *)
(*           match (deriveQuant variables (get_substrate ()) k variables subst target) with *)
(* 		  | None -> [ ] *)
(* 		  | Some((a, b)) -> [ a ]) *)
(*       substs *)
      
(* (\*   let deriveWrapper (_i: option infostrate) (t: term) (s: list substitution)  *\) *)
(* (\*                     (\\*: (TranslationfromFStar.listFS substitution) *\\)= *\) *)
(* (\*     (ListOfPrimsList (derive _i t s))  *\) *)

(* (\*   let deriveJustification  *\) *)
(* (\*       (_infostrate: option IInfostrate) (target: term(\\*Infon*\\)) (proofTemplate: term(\\*Evidence*\\)) *\) *)
(* (\*       (substs: list substitution) : list substitution = *\) *)
(* (\* 	failwith "deriveJustification not supported in FStar Engine" *\) *)
      
(* (\*   let deriveJustificationWrapper (_i: option IInfostrate) (i: term)  *\) *)
(* (\*                                  (p: term) (s: list substitution)  *\) *)
(* (\*                                  (\\*: listFS substitution*\\) = *\) *)
(* (\*     ListOfPrimsList (deriveJustification _i i p s) *\) *)
