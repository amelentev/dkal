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
open Microsoft.Research.Dkal.Ast.Infon
open AST

module Stage4 =
  type Side = Left | Right
  type Status = Raw | Pending | Processed

  type PrimalRecord() =
      /// only Binary AST
      let mutable map : Map<Operation*Side, list<AST>> = Map.empty 
      let mutable status = Raw

      member this.Status
          with get() = status
          and set(s) = status <- s

      member this.get k =
          match map.TryFind k with
          | Some(r) -> r
          | None -> []

      member this.Append k v =
          map <- map.Add(k, (v :: this.get k))

  let preprocess (H: IDictionary<int, AST>) (HY: list<AST>) =
      let T = Dictionary()
      let mutable pending = []

      let forallLeaders f = H |> Seq.iter (fun kv -> if kv.Key=kv.Value.Key then f(kv.Value))
      forallLeaders (fun h -> T.Add(h.Key, PrimalRecord()))

      let appendT key op side n = T.[key].Append (op,side) n

      let rec dfs = function
      | Binary(_,op,l,r) as n ->
          let (l,r) = H.[l.Key], H.[r.Key]
          appendT l.Key op Left n
          appendT r.Key op Right n
          dfs l
          dfs r
      | Rel((k,_,_), Primitives.EmptyInfon) ->
          T.[H.[k].Key].Status <- Pending
      | _ -> ()

      forallLeaders dfs

      HY |> List.iter (fun h -> T.[H.[h.Key].Key].Status <- Pending)
      T