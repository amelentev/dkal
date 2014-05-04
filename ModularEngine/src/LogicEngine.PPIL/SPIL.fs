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

/// SPIL randomized algorithm. O(n) average complexity for all inputs.
module SPIL =
  let rec initialReformat = function
  | SetFormula(k, AndOp, children) ->
    let newchildren =
      [for c in children |> List.map initialReformat do
        match c with
        | SetFormula(k,AndOp,ch) -> yield! ch  // TODO: preserve pref for prettyprinting
        | Rel(_, s) when s=trueLabel -> ()
        | other -> yield other]
    if newchildren.IsEmpty then Rel(k, trueLabel)
    else SetFormula(k, AndOp, newchildren)
  | SetFormula(k, OrOp, children) -> 
    SetFormula(k, OrOp, children |> List.map initialReformat)
  | Implies(k, a, b) -> 
    Implies(k, initialReformat a, initialReformat b)
  | Rel(_) as rel -> rel

  let makeParents nodes =
    let parent = Dictionary<int, AST>()
    let makeparents = function
      | SetFormula(k, _, children) as u ->
        for c in children do
          parent.[c.Key] <- u
      | Implies(k, a, b) as u ->
        parent.[a.Key] <- u
        parent.[b.Key] <- u
      | Rel(_) -> ()
    nodes |> Seq.iter makeparents
    parent

  let makeLeafKey =
    let leafLabels = Dictionary<string, int>()
    leafLabels.Add(trueLabel, 0) // reserve true label
    (fun s ->
      match leafLabels.TryGetValue(s) with
      | true,l -> l
      | false,_ ->
        let l = leafLabels.Count
        leafLabels.Add(s, l); l)

  let plagiarismCheckerHash (L: 'A array) =
    let hashmap = Dictionary<'A, int>()
    [|
      for i in 0..L.Length-1 ->
        match hashmap.TryGetValue(L.[i]) with
        | true,b -> b
        | false,_ ->
          hashmap.Add(L.[i], i)
          i
     |]
  /// 1d plagiarism checker
  let plagiarismChecker1 (A: int array) (p: int array) =
    let k = p.Length
    let B: int array = Array.zeroCreate k
    for j in 0..k-1 do
      if A.[p.[j]] < 0 then
        B.[j] <- j; A.[p.[j]] <- j
      else
        B.[j] <- A.[p.[j]]
    for j in 0..k-1 do // reinitialized A
      if B.[j] = j then
        A.[p.[j]] <- -1
    B
  /// plagiarism checker for lists array. L[i] should be the same size for all i.
  let rec plagiarismChecker (A: int array) (L: int list array) : int array =
    let k = L.Length
    if k=0 then Array.create 0 0
    else
      let R,L' = L |> Array.map (fun e -> e.Head, e.Tail) |> Array.unzip
      if L'.[0] = [] then plagiarismChecker1 A R
      else
        let B' = plagiarismChecker A L'
        let L'i = Array.init k (fun i -> ResizeArray<int>())
        let Ri = Array.init k (fun i -> ResizeArray<int>())
        let pos = Array.zeroCreate k
        for j in 0..k-1 do
          L'i.[B'.[j]].Add(j)
          Ri.[B'.[j]].Add(R.[j])
          pos.[j] <- L'i.[B'.[j]].Count - 1
        let B'i = Array.init k (fun i -> plagiarismChecker1 A (Ri.[i].ToArray()))
        let B = Array.zeroCreate k
        for j in 0..k-1 do
          let i = B'.[j] // i is original for j in L'
          let l = B'i.[i].[pos.[j]] // l is original for pos[j] in Ri
          B.[j] <- L'i.[i].[l]
        B
  /// return plagiarism checker for 4-int-tuples
  let plagiarismChecker4 A L =
    let t2list (a,b,c,d) = [a;b;c;d]
    plagiarismChecker A (L |> Array.map t2list)

  let setEquality s1 s2 =
    if List.length s1 <> List.length s2 then false
    else
      let hashset = HashSet<int>() // TODO: via array A
      for a in s1 do
        hashset.Add(a) |> ignore
      s2 |> Seq.forall (fun a -> hashset.Contains(a))

  let compressNodes A (workList: ((int*int*int*int)*AST) seq) (HO: ASTMap) counter =
    let B = plagiarismChecker4 A (workList |> Seq.map fst |> Seq.toArray)
    let U = workList |> Seq.map snd |> Seq.toArray
    [ for i in 0..B.Length-1 do
        let (u,w) = U.[i], U.[B.[i]]
        if i<>B.[i] then
          HO.[u.Key] <- w // replace u to w
        else
          HO.[u.Key] <- u // u is original
        yield! (counter u |> Option.toList)
    ]

  /// return a function which return the parent v of a child u if all other children of v are visited
  let makeCounter (parent: ASTMap) (nodes: AST seq) =
    let counter = Dictionary<int, int>()
    nodes |> Seq.iter (fun n -> counter.Add(n.Key, n.ChildrenCount))
    (fun (u: AST) ->
      match parent.TryGetValue u.Key with
      |true,v ->
        let k = v.Key
        counter.[k] <- counter.[k] - 1
        if counter.[k] = 0 then Some(v) else None
      |_ -> None)

  let rec reformatSetFormulas A (HO: ASTMap) counter = function
    | SetFormula(k, AndOp, ch) as u ->
      let ch = ch |> List.toArray |> Array.map (fun c -> HO.[c.Key])
      let newch = ch |> Array.map (fun c -> c.Key) |> plagiarismChecker1 A |> Array.mapi (fun i b -> i=b, i) |> Array.filter fst |> Array.map (fun (_,i) -> ch.[i])
      if newch.Length = 1 then
        let w = newch.[0]
        HO.[u.Key] <- w // replace u by child w
        counter u |> Option.bind (reformatSetFormulas A HO counter) // return parent if all children are visited
      else
        let newu = SetFormula(k, AndOp, newch |> Array.toList)
        HO.[u.Key] <- newu
        Some(newu)
    | Implies(k, a, b) as u -> 
      let newu = Implies(k, HO.[a.Key], HO.[b.Key])
      HO.[u.Key] <- newu
      Some(newu)
    | SetFormula(k, OrOp, [a; b]) as u ->
      let newu = SetFormula(k, OrOp, [HO.[a.Key]; HO.[b.Key]])
      HO.[u.Key] <- newu
      Some(newu)
    | SetFormula(_, OrOp, _) -> failwith "OR formulas should be binary"
    | Rel(_) as u -> Some(HO.[u.Key])

  let computeEL prefkey chKey leafKey = function
    | Rel(_, s) when s = trueLabel -> (0, 0, 0, 0) // true
    | Rel(_, s) as u -> (prefkey u, 0, leafKey s, 0)
    | Implies(_, a, b) as u -> (prefkey u, 1, a.Key, b.Key)
    | SetFormula(_, AndOp, ch) as u -> (0, 2, chKey ch u.Key, 0)
    | SetFormula(_, OrOp, ch) as u ->
      assert (ch.Length = 2)
      (prefkey u, 3, ch.[0].Key, ch.[1].Key)

  let makeChKey nodeKeys M =
    let rnd = System.Random()
    let hashes = Dictionary<int,int>()
    nodeKeys |> Seq.iter (fun k -> hashes.[k] <- rnd.Next(M))
    let equalityComparer = 
      { new IEqualityComparer<AST list> with
        member x.Equals(t1: AST list, t2: AST list) = 
          let keys = List.map (fun (u: AST) -> u.Key)
          setEquality (keys t1) (keys t2)
        member x.GetHashCode(t: AST list) = t |> List.map (fun u -> hashes.[u.Key]) |> List.reduce (^^^)
      }
    let hashtable = Dictionary<AST list, int>(equalityComparer)
    let chKey ch k =
      match hashtable.TryGetValue(ch) with
      |true,k1 -> k1
      |false,_ -> hashtable.Add(ch, k); k
    let clearChKey() = hashtable.Clear()
    chKey, clearChKey

  let compress H Q (nodes: ASTMap) (V: TrieMap) =
    let M = 1 + max (nodes.Keys |> Seq.max) (V.Values |> Seq.map (fun (t: Trie) -> t.Position) |> Seq.max)
    let A = Array.zeroCreate M
    let parent = makeParents nodes.Values
    let counter = makeCounter parent nodes.Values
    let prefkey (n:AST) = V.[n.Key].Position
    let leafKey = makeLeafKey
    let chKey,clearChKey = makeChKey nodes.Keys M
    let computeEL = computeEL prefkey chKey leafKey
    let HO = Dictionary<int, AST>()

    let mutable workList = nodes.Values |> Seq.filter (function |Rel(_) -> true |_ -> false)
    while not (Seq.isEmpty workList) do
      let tocompress = workList |> Seq.map (fun u -> computeEL u, u)
      workList <- compressNodes A tocompress HO counter |> List.choose (reformatSetFormulas A HO counter)
      clearChKey()
    let hom (u: AST) = HO.[u.Key]
    (H |> List.map hom, Q |> List.map hom, HO) // TODO: Evidence preserving

  let solve H Q =
    // stage1
    let (H,Q,_) = Stage1.stage1 H Q // construct parse tree
    let (N, V) = Stage2.constructNodesnVertices (H@Q) // construct prefix trie
    let H = H |> List.map initialReformat // initial reformatting
    let Q = Q |> List.map initialReformat
    Stage2.assignUniqueKeys V
    let (H,Q,HO) = compress H Q N V
    // stage2
    let T = Stage4.preprocess HO H
    let proofs = Stage5.stage5 N HO T (fun u -> []) Q
    Q |> List.map (fun (q: AST) ->
      match proofs.TryGetValue(HO.[q.Key].Key) with
      | true,proof -> Some proof
      | _ -> None)