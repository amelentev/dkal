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
  type Side = AndOp | OrOp | ImplLeft | ImplRight
  type Status = Raw | Pending | Processed

  type PrimalRecord(numChilds) =
      let mutable map : Map<Side, list<AST>> = Map.empty 
      let mutable status = Raw
      let mutable counter = 0

      member this.Status
          with get() = status
          and set(s) = status <- s

      member this.get k =
          match map.TryFind k with
          | Some(r) -> r
          | None -> []

      member this.Append k v =
          map <- map.Add(k, (v :: this.get k))

      member this.IncCounter() = counter <- counter + 1; counter

      member this.NumChilds = numChilds

  let preprocess (H: IDictionary<int, AST>) (HY: list<AST>) =
      let T = Dictionary()

      let forallLeaders f = H |> Seq.iter (fun kv -> if kv.Key=kv.Value.Key then f(kv.Value))
      forallLeaders (function
        | SetFormula(_,_,args) as h ->
          T.Add(h.Key, PrimalRecord(List.length args))
        | h -> T.Add(h.Key, PrimalRecord(0)))

      let appendT key side n = T.[key].Append side n

      let dfs = function
      | SetFormula(_,op,args) as n ->
          let args = args |> List.map (fun a -> H.[a.Key])
          let side = if op = AST.AndOp then AndOp else OrOp
          for a in args do
            appendT a.Key side n
      | Implies(_,l,r) as n ->
          let (l,r) = H.[l.Key], H.[r.Key]
          appendT l.Key ImplLeft n
          appendT r.Key ImplRight n
      | Rel(_, Primitives.EmptyInfon) as u ->
          T.[H.[u.Key].Key].Status <- Pending
      | _ -> ()

      forallLeaders dfs

      HY |> List.iter (fun h -> T.[H.[h.Key].Key].Status <- Pending)
      T