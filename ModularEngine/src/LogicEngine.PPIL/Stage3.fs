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

  let homonomy (input:string) ((nodes:IDictionary<int, AST>), (vertices: IDictionary<int, Trie>)) =
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
      H
           