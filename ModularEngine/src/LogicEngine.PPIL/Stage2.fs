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

module Stage2 =

  type Trie() =
      let mutable position = -1
      let childs = new Dictionary<string, Trie>()
      member this.Position
          with get() = position
          and set(p) = position <- p

      member this.add (s:string) =
          let (ok,child) = childs.TryGetValue s
          if ok then 
            child
          else 
            let child = new Trie()
            childs.Add(s, child)
            child

      override this.ToString() =
          let str = childs |> Seq.map (fun e -> e.Key + e.Value.ToString()) |> String.concat ","
          "("+str+")"

  let constrByPref trie = List.fold (fun (trie:Trie) p -> trie.add p) trie

  let rec constructTrie trie lst = function
    | Rel(c,_) as n ->
      let trie = constrByPref trie c.pref
      (n, trie) :: lst
    | Implies(c,l,r) as n ->
      let trie = constrByPref trie c.pref
      let lst = constructTrie trie lst l
      let lst = constructTrie trie lst r
      (n, trie) :: lst
    | SetFormula(c,_,args) as n ->
      let trie = constrByPref trie c.pref
      let lst = args |> List.fold (fun lst n -> constructTrie trie lst n) lst
      (n, trie) :: lst

  let constructNodesnVertices asts =
      let trie = Trie()
      let lst = asts |> List.collect (constructTrie trie [])

      let nodes = Dictionary()
      let vertices = Dictionary()
      // fill nodes & vertices
      lst |> List.iter (fun (n, v) ->
                          nodes.Add(n.Key, n)
                          vertices.Add(n.Key, v))
      (nodes, vertices)