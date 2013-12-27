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

/// Extension of TSPIL with (->/\i) rule.
/// p said ((A -> B) & (A -> C)) => p said (A -> B & C)
module TSPIL2 =
  type Graph = IDictionary<int, IDictionary<int,int>>
  type Graphs = IDictionary<Trie, Graph>

  let rec addImpl (H: ASTMap) (T: PrimalRecordMap) (prefix: Trie) (Gs: Graphs) (u: AST) (v: AST) =
    let hom (u:AST) = H.[u.Key]
    let u = hom u
    let v = hom v
    let addrec = addImpl H T prefix Gs
    let IC = Gs.[prefix]
    match IC.[u.Key].TryGetValue(v.Key) with
    | true,0 -> () // already derived
    | _,_ ->
      IC.[u.Key].[v.Key] <- 0
      // (/\e)
      match v with
      | SetFormula(_, SetOperation.AndOp, lst) ->
        for w in lst do
          addrec u (hom w)
      | _ -> ()
      // (/\i)
      for w in T.[v.Key].get Side.AndOp do
        if IC.[u.Key].[w.Key] = 1 then
          addrec u w
        else if IC.[u.Key].[w.Key] > 0 then
          IC.[u.Key].[w.Key] <- IC.[u.Key].[w.Key] - 1
      // forward recursion
      let leaders = Stage4.getLeaders H T
      for w in leaders do
        match IC.[v.Key].TryGetValue(w.Key) with
        | true,0 ->
          addrec u w
        | _,_ -> ()
      // backward recursion
      for w in leaders do
        match IC.[w.Key].TryGetValue(u.Key) with
        | true,0 ->
          addrec w v
        | _,_ -> ()

  let init H (vertices: TrieMap) T =
    let Gs = Dictionary() :> Graphs
    let leaders = Stage4.getLeaders H T
    let prefixes = new HashSet<Trie>(leaders |> Seq.map (fun u -> vertices.[u.Key]))
    for p in prefixes do
      let G = Dictionary() :> Graph
      Gs.Add(p, G)
      for u in leaders do
        G.Add(u.Key, Dictionary())
    for p in prefixes do
      let G = Gs.[p]
      for u in leaders do
        for v in leaders do
          match v with
          | SetFormula(_,SetOperation.AndOp,lst) ->
            G.[u.Key].Add(v.Key, List.length lst)
          | _ -> ()
        addImpl H T p Gs u u
    Gs
   
  let applyTrans2 H (vertices: TrieMap) T (Gs: Graphs) = function
  | Implies(_, ul, ur) as u ->
    addImpl H T vertices.[u.Key] Gs H.[ul.Key] H.[ur.Key]
    [
      for t in T do
        match t.Value.Status, H.[t.Key] with
        | Raw, (Implies(_, wl, wr) as v) ->
          match Gs.[vertices.[v.Key]].[H.[wl.Key].Key].TryGetValue(H.[wr.Key].Key) with
          | true,0 ->
            yield H.[t.Key]
          | _,_ -> ()
        | _ -> ()
    ]
  | _ -> []