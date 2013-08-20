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
open AST
open Stage4

module TPIL =
  let genericApplyTrans extraTraverse (H:IDictionary<int,AST>) (T:IDictionary<int,PrimalRecord>) = function
    | Implies(_, ul, ur) ->
      let homkey (u:AST) = H.[u.Key].Key
      let status p =  T.[homkey p].Status

      let constructPred ukey side f2 =
        let r = HashSet<int>()
        let q = Queue<int>()
        let qappend w =
          if not(r.Contains w) then
            q.Enqueue w
            r.Add w |> ignore
        qappend ukey
        while q.Count > 0 do
          let u = H.[q.Dequeue()]
          for v in T.[homkey u].get side do
            match v with
            | Implies(_,vl,vr) when status v <> Raw ->
              let w = homkey (f2 (vl,vr))
              qappend w
            | _ -> ()
          extraTraverse u side |> Seq.iter qappend
        r

      let Pl = constructPred (homkey ul) ImplRight fst
      let Pr = constructPred (homkey ur) ImplLeft snd

      [
      for t in T do
        match t.Value.Status, H.[t.Key] with
        | Raw, Implies(_, vl, vr) ->
          if Pl.Contains (homkey vl) && Pr.Contains (homkey vr) then
            yield H.[t.Key]
        | _ -> ()
      ]
    | _ -> []

  let applyTrans x = genericApplyTrans (fun _ _ -> []) x