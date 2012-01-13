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

(* A complete, incremental decision procedure for quantified infon logic *)
module QIL
open Types
open Util
open Subst
open Typing
open Unify
open HilbertianQIL

(* TODO: Use a better implementation of sets of terms *)
type set :: _ = list 
let singleton x = [x]

val setCons : 'a -> set 'a -> set 'a
let setCons x y = 
  if contains x y then y
  else x::y

val subformulas: term -> set term 
let rec subformulas x = match x with 
  | Var _ -> singleton x
  | Const _ -> singleton x
  | App AsInfon _ -> singleton x
  | App EmptyInfon _ -> singleton x
  | App (RelationInfon _) _ -> singleton x 
  | App AndInfon args -> setCons x (collect subformulas args)
  | App ImpliesInfon args -> setCons x (collect subformulas args)
  | App SaidInfon [p; i] -> setCons x (map (fun j -> App SaidInfon [p; j]) (subformulas i))
  | App JustifiedInfon [p; i; b] -> setCons x (subformulas i)
  | _ -> raise "Unexpected infon form"

type denv = {sf:set term; pfx:prefix; prins:set principal}

type dterm :: _ = fun (pfx:prefix) (K:infostrate) =>
    ((x:term * (S:substrate -> Partial (entails S K [] (WithPrefix pfx x)))))


(* ******************************************************************************** *)
(* Single-step derivations *)
(* ******************************************************************************** *)
val deriveAllOneStep : d:denv 
              -> k:infostrate
              -> from:list (dterm d.pfx k)
              -> list (dterm d.pfx k)
let deriveAllOneStep d k from = 
  let pfx = d.pfx in 
  let pairs : list (dterm pfx k) = product from from 
    (fun (i, pfi) (j, pfj) -> 
       let impElim =
         match i with
           | (App ImpliesInfon [i1;i2]) when j=i1 ->
               let dt:dterm pfx k = 
                 (i2, (fun s -> match pfi s, pfj s with 
                         | MkPartial ei, MkPartial ej -> 
                             MkPartial (Entails_Imp_Elim s k [] i1 i2 pfx ej ei))) in 
                 ([dt] : list (dterm pfx k))
           | _ -> [] in
       let andIntro:dterm pfx k =
         ((App AndInfon [i;j]),
          (fun s -> bindP (pfi s) (fun ei ->
                    mapP (pfj s) (fun ej -> 
                         Entails_And_Intro s k [] i j pfx ei ej)))) in 
         andIntro::impElim) in
      
    let singles : list (dterm pfx k) = coll from 
      (fun (i, pfi) -> 
         let andElim = match i with 
           | App AndInfon [i1;i2] -> 
               let dt1:dterm pfx k = 
                 (i1, (fun s -> mapP (pfi s) (fun ei -> Entails_And_Elim1 s k [] i1 i2 pfx ei))) in 
               let dt2:dterm pfx k = 
                 (i2, (fun s -> mapP (pfi s) (fun ei -> Entails_And_Elim2 s k [] i1 i2 pfx ei))) in 
                 [dt1;dt2]
           | _ -> [] in 

         let impIntro = coll d.sf
           (fun phi -> match phi with 
              | App ImpliesInfon [i1;i2] when i=i2 -> 
                  let i1_t, i1_typing = doTyping [] i1 in 
                    if i1_t = Infon then 
                      let dt:dterm pfx k = 
                        (phi, (fun s -> mapP (pfi s) (fun ei -> Entails_W_Imp_Intro s k [] i1 i2 pfx ei i1_typing))) in 
                        [dt]
                    else []
              | _ -> []) in 

         let jElim = match i with 
           | App JustifiedInfon [p;i1;b] -> 
               let dt:dterm pfx k = 
                 (i1, (fun s -> mapP (pfi s) (fun ei -> Entails_J_Elim s k [] p i1 b pfx ei))) in 
                 [dt]
           | _ -> [] in 
           
           append andElim (append impIntro jElim)) in 

    let axioms = from in 
    let newones = append pairs singles in 
    let _ = println (strcat "deriveOneStep with prefix " (string_of_any_for_coq pfx)) in
    let _ = match newones with 
      | [] -> println " derived nothing new"; ()
      | _ -> iterate (fun i -> println (strcat "derived: " (string_of_any_for_coq i)); ()) newones in
      (append pairs (append singles axioms) : list (dterm pfx k))

(* ******************************************************************************** *)
(* Boxing and unboxing quotations *)
(* ******************************************************************************** *)
val unbox:  p:term
         -> pi:prefix 
         -> k:infostrate
         -> from:list (dterm pi k)
         -> list (dterm (p::pi) k)
let unbox p pfx k from = 
  coll from (fun (i, pfi) -> match i with 
                     | App SaidInfon [q; j] when p=q -> 
                         let dt:dterm (p::pfx) k = (j,pfi) in 
                           [dt]
                     | _ -> [])

val box: p:term
      -> pfx:prefix
      -> k:infostrate
      -> from:list (dterm (p::pfx) k)  (* x where  (pfx (Said p x)) is derivable *)
      -> list (dterm pfx k)            (* y=Said p x where  (pfx y) is derivable *)
let box p pfx k from = coll from (fun (i, pfi) -> 
                                    let dt:dterm pfx k = ((App SaidInfon [p;i]), pfi) in 
                                      [dt])

(* ******************************************************************************** *)
(* Main derivation loop *)
(* ******************************************************************************** *)
val removeDups : list 'a -> ('a -> 'a -> bool) -> list 'a
let rec removeDups l f = match l with 
  | [] -> []
  | hd::tl -> let res = removeDups tl f in 
      if List_exists (fun hd' -> f hd hd') res
      then res
      else hd::res

val setUnion : pfx:prefix -> k:infostrate 
            -> list (dterm pfx k) 
            -> list (dterm pfx k) 
            -> list (dterm pfx k)
let setUnion pfx k l1 l2 = 
  removeDups (append l1 l2) (fun (i, _) (j, _) -> i=j)
  
val deriveAll:  d:denv 
             -> k:infostrate 
             -> list (dterm d.pfx k)
             -> list (dterm d.pfx k)
let rec deriveAll d k this_pfx = 
  match this_pfx with 
    | [] -> []
    | _ -> 
        let _ = println (strcat "deriveAll under pfx: " (string_of_any_for_coq d.pfx)) in 
        let pre = setUnion d.pfx k 
          (coll d.prins (fun pp -> 
                     let p = Const (PrincipalConstant pp) in
                     let this_p_pfx = unbox p d.pfx k this_pfx in 
                     let next_p_pfx = deriveAll ({d with pfx=(p::d.pfx)}) k this_p_pfx in 
                     let next_pfx : list (dterm d.pfx k) = box p d.pfx k next_p_pfx in
                       next_pfx))
          (deriveAllOneStep d k this_pfx) in 
        let next_pfx = filter (fun ((i, pfi) : dterm d.pfx k) -> 
                                 if contains ((withPrefix d.pfx i) : term) d.sf
                                 then (println (strcat "Retaining " (string_of_any_for_coq i)); true)
                                 else (println (strcat "Discarding " (string_of_any_for_coq i)); false)) pre in 
          if (length next_pfx) = (length this_pfx) 
          then next_pfx
          else deriveAll d k next_pfx
            

val deriveAllWrapper: sf:list term 
                   -> ps:list principal 
                   -> k:infostrate 
                   -> list (dterm [] k)
let deriveAllWrapper sf ps k = 
  let d = {sf=sf;
           pfx=[];
           prins=ps} in 
  let dterm_hyp (i:infon{In i k}) : list (dterm d.pfx k) =     match i with 
    | MonoTerm j -> 
        match doPolyTyping [] i with 
          | MkPartial pt -> 
              let dt:dterm d.pfx k = 
                (j, (fun s -> 
                       MkPartial (Entails_Poly s k [] j
                                    (Entails_Hyp_Knowledge s k [] i i pt (AEQ_Refl i) pt)))) in 
                [dt] in 
    deriveAll d k (collect_in k dterm_hyp)
  
