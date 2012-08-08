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

namespace Microsoft.Research.Dkal.LogicEngine.PPIL

open System.Collections.Generic
open Stage4
open AST

module Stage5 = 

  let stage5 (H:IDictionary<int,AST>) (T:IDictionary<int,PrimalRecord>) =
      let hom (u:AST) = H.[u.Key]
      let homkey u = (hom u).Key
      let status (p:AST) =
          T.[homkey p].Status

      let rec proceed (u:AST) =
          let u = hom u
          let t = T.[u.Key]
          t.Status <- Pending
          let makepending v = if status v = Raw then proceed v
          // &e
          match u with
          | SetFormula(_,AndOp,args) ->
              for a in args do makepending a
          | _ -> ()
          // &i
          t.get (Side.AndOp) |> List.iter (function
              | SetFormula(_,AndOp,args) as w ->
                  let t = T.[homkey w]
                  if t.IncCounter() >=  t.NumChilds then
                    makepending w
              | _ -> ())
          // |i
          t.get (Side.OrOp) |> List.iter makepending
          // ->i
          t.get (ImplRight) |> List.iter makepending
          // ->e
          t.get (ImplLeft) |> List.iter (function
            | Implies(_,_,r) -> makepending r
            | _ -> failwith "impossible")
          match u with
          | Implies(_,l,r) when status l <> Raw ->
              makepending r
          | _ -> ()
          t.Status <- Processed

      for h in H do 
          if T.[h.Value.Key].Status=Pending then
            proceed h.Value