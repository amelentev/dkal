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
open SuffixArray
open Stage2
open AST

module Stage3 =

  let homonomySufArr (input:string) HY QU ((nodes: ASTMap), (vertices: TrieMap)) =
      let sufarr = LCP.sufsort input
      let lcp = LCP.computeLCP(input, sufarr)

      (* debugging 
      for i in 0..input.Length-1 do
          let si = sufarr.[i]
          if nodes.ContainsKey si then
              printf "%d\t%d\t%s\n" si lcp.[i] (input.Substring(si)) *)

      let H = Dictionary()
      
      let mutable i = 0
      let mutable p1 = 0
      while i < input.Length do
          let si = sufarr.[i]
          match nodes.TryGetValue si with
          | true, n when lcp.[i] < n.Length ->
              p1 <- i
              vertices.[si].Position <- p1
              H.Add(si, n)
          | true, n ->
              let vi = vertices.[si]
              if vi.Position < p1 then
                  H.Add(si, n)
                  vi.Position <- i
              else
                  H.Add(si, H.[sufarr.[vi.Position]])
          | _ -> ()
          i <- i+1
      HY, QU, H

  /// Build a homonomy map and remove duplicates from set formulas. preserve order of childrens.
  /// average complexity = O(number of nodes)
  let homonomyHash _ HY Q ((nodes: ASTMap), (vertices: TrieMap)) =
    /// assign unique key for every local prefix
    let mutable ind = 0
    for v in vertices.Values do
      v.Position <- ind
      ind <- ind + 1

    /// remembered hashes. from node key to hash
    let hashcache = Dictionary<int, int>()
    /// hash function for prefix
    let prefhash (u:AST) = vertices.[u.Key].Position
    /// hash function for nodes
    let rec nhash = function
    | Rel(_,s) as u -> (prefhash u, s).GetHashCode()
    | Implies(_,l,r) as u -> (prefhash u, chash l, chash r).GetHashCode()
    | SetFormula(_,op,args) as u -> 
        let argshash = (args |> List.map chash |> List.reduce (^^^))
        (prefhash u, op, argshash).GetHashCode()
    /// hash function for nodes with memorization
    and chash (u: AST) =
      match hashcache.TryGetValue u.Key with
      | true,res -> res
      | _ ->
        let r = nhash u
        hashcache.Add(u.Key, r)
        r
    /// homonomy map
    let HO = Dictionary<int, AST>(nodes)
    let homkey (u:AST) = HO.[u.Key].Key
    /// make t1 and t2 as homonyms.
    let rec addhom (t1:AST) (t2:AST) =
      if t2.Key = homkey t2 then // is t2 original?
        HO.Remove(t2.Key) |> ignore
        HO.Add(t2.Key, HO.[t1.Key])
        true
      else
        assert (t1.Key = homkey t1) // t1 should be original
        addhom t2 t1
    /// equality and hash provider
    let equalityComparer = 
      { new IEqualityComparer<AST> with
        /// @precondition: childrens of t1 and t2 should be in globalset
        member x.Equals(t1: AST, t2: AST) =
          if homkey t1 = homkey t2 then
            true
          else
          if vertices.[t1.Key] = vertices.[t2.Key] then // same prefix?
            match t1, t2 with
            | Rel(_,s1), Rel(_,s2) when s1.Equals(s2) ->
              addhom t1 t2
            | SetFormula(_,op1,args1), SetFormula(_,op2,args2) when op1=op2 && args1.Length = args2.Length ->
              let leaders = HashSet<int>()
              for a in args1 do
                leaders.Add(homkey a) |> ignore
              if args2 |> List.forall (fun x -> leaders.Contains(homkey x)) then
                addhom t1 t2
              else false
            | Implies(_,a11,a12), Implies(_,a21,a22) ->
              if homkey a11 = homkey a21 && homkey a12 = homkey a22 then
                addhom t1 t2
              else false
            | _ -> false
          else false
        member x.GetHashCode(t: AST) = chash t
      }
    /// Set of all homonymy originals
    let globalset = new HashSet<AST>(equalityComparer)
    /// remove duplicates from set formulas
    let rec removedups = function
    | Rel(_) as self ->
      globalset.Add(self) |> ignore
      self
    | SetFormula(_, op, args) as self ->
      let args = args |> List.map removedups
      let childrenset = HashSet(equalityComparer)
      let args = args |> List.filter childrenset.Add
      let res = SetFormula(self.Common, op, args)
      globalset.Add(res) |> ignore
      if args.Length=1 then args.Head
      else res
    | Implies(_, al, ar) as self ->
      let al = removedups al
      let ar = removedups ar
      let self = Implies(self.Common, al, ar)
      globalset.Add(self) |> ignore
      self

    let HY = HY |> List.map removedups
    let Q = Q |> List.map removedups
    HY, Q, HO