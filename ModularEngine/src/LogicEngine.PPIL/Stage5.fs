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
      let setstatus (p:AST) s =
          T.[homkey p].Status <- s

      let rec proceed (u:AST) =
          let u = hom u
          let t = T.[u.Key]
          t.Status <- Pending
          let makepending v = if status v = Raw then proceed v
          // &e
          match u with
          | Binary(_,AndOp,l,r) ->
              makepending l
              makepending r
          | _ -> ()
          // &i
          t.get (AndOp, Left) |> List.iter (function
              | Binary((_,_,_),AndOp,l,r) as w when status r <> Raw ->
                  makepending w
              | _ -> ())
          t.get (AndOp, Right) |> List.iter (function
              | Binary((_,_,_),AndOp,l,r) as w when status l <> Raw ->
                  makepending w
              | _ -> ())
          // |i
          (t.get (OrOp, Left) @ t.get(OrOp, Right)) |> List.iter makepending
          // ->i
          t.get (ImpliedOp, Right) |> List.iter makepending
          // ->e
          t.get (ImpliedOp, Left) |> List.iter (fun w -> makepending w.Right)
          match u with
          | Binary(_,ImpliedOp,l,r) when status l <> Raw ->
              makepending r
          | _ -> ()
          setstatus u Processed

      for h in H do 
          if T.[h.Value.Key].Status=Pending then
            proceed h.Value