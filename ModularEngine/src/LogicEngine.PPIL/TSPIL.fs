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
open Stage2
open Stage4

/// Transitive SPIL
module TSPIL =
  /// is v1 is subset of v2, i.e. if v2 |= v1
  let isSubset (H: ASTMap) (V:TrieMap) v1 v2 =
    if V.[v1] <> V.[v2] then
      false
    else
      let homkey (v:AST) = H.[v.Common.key].Key
      let rec deset = function
      | SetFormula(_,_,[u]) -> deset u
      | u -> u
      match deset H.[v1], deset H.[v2] with
      | SetFormula(_,op1,list1), SetFormula(_,op2,list2) when op1=op2 ->
        let setcontains list1 list2 =
          let set = HashSet<int>()
          for v in list2 do
            set.Add(homkey v) |> ignore
          list1 |> Seq.forall (fun v -> set.Contains(homkey v))
        match op1 with
        | SetOperation.AndOp -> setcontains list1 list2
        | SetOperation.OrOp ->  setcontains list2 list1
      | r, SetFormula(_, SetOperation.AndOp, list) ->
        let hr = homkey r
        list |> Seq.exists (fun x -> hr = homkey x)
      | SetFormula(_, SetOperation.OrOp, list), r ->
        let hr = homkey r
        list |> Seq.exists (fun x -> hr = homkey x)
      | r1, r2 ->
        r1.Key = r2.Key
      | _ -> false

  type SetRelation() =
    inherit Dictionary<int, int list>()
    member x.Append key value =
      match x.TryGetValue key with
      | false, _ -> x.Add(key, [value])
      | true, lst -> x.[key] <- value :: lst

  /// generate subset and superset relations. O(N^2)
  let genSetContainmentRelation (H: ASTMap) V =
    let subsets = SetRelation()
    let supsets = SetRelation()
    let leaders = H.Keys |> Seq.filter (fun x -> x = H.[x].Key) |> Seq.toList
    let isSubset = isSubset H V
    for v1 in leaders do
      for v2 in leaders do
        if isSubset v1 v2 then
          supsets.Append v1 v2
          subsets.Append v2 v1
    // TODO: BFS and remove unnecessary edges. node -> (level,parents)
    subsets,supsets

  let traverseSubsets (subsets:SetRelation, supsets:SetRelation) (v:AST) = function
  | Side.ImplRight ->
    supsets.[v.Key]
  | Side.ImplLeft ->
    subsets.[v.Key]
  | _ -> failwith "impossible"

  let applyDisjunctionSetIntro (subsets:SetRelation, _) (H: ASTMap) (T: PrimalRecordMap) = function
    | SetFormula(_, SetOperation.OrOp, list) as s when list.Length>1 ->
      let hs = H.[s.Key]
      subsets.[hs.Key] |> List.map (fun x -> H.[x])
    | _ -> []
