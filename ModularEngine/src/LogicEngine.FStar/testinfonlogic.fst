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

module TestInfonLogic
open Types
open Subst
open InfonLogic

let x = { name = "x" ; typ = Int32 } 

let y = { name = "y" ; typ = Int32 } 

let z = { name = "z" ; typ = Int32 } 

let f = { fname = "f" ; retType = Infon ; 
          argsType = [Int32; Int32] ; identity = None }

let g = { fname = "g" ; retType = Infon ; 
          argsType = [Int32; Int32] ; identity = None } 

let test (foo:bool) = 
  let k1 = (ForallT [x; y; z] (App ImpliesInfon
                                 [(App (RelationInfon f) [(Var x); (Var y)]);
                                  (App (RelationInfon g) [(Var y); (Var z)])])) in
  let k2 = MonoTerm (App (RelationInfon f) [(Const (Int 0)); (Const (Int 1))]) in
  let g  =  (App (RelationInfon g) ([(Const (Int 1) ); (Const (Int 2))])) in
   assume (CheckedInfon k1); 
   assume (CheckedInfon k2); 
   match doDerive () [k1; k2] [] [] [] [] g with
     | None -> println "none"
     | Some ((s, mkpf)) -> (println "Proved goal ... building proof"; println (string_of_any_for_coq (mkpf s)))

;; 

let _ = test false in ()
