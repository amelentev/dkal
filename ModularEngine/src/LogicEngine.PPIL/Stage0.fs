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

open System.Text
open System.Collections.Generic
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open SuffixArray
open AST

module Stage0 =
  let rec flatConjuncts = function
    | AndInfon(args) ->
      let args = [ for a in args |> List.map flatConjuncts do
                    match a with
                    | AndInfon(aargs) -> yield! aargs
                    | other -> yield other ]
      AndInfon(args)
    | OrInfon(args) ->
      let args = [ for a in args |> List.map flatConjuncts do
                    match a with
                    | OrInfon(aargs) -> yield! aargs
                    | other -> yield other ]
      OrInfon(args)
    | App(f, args) ->
      App(f, args |> List.map flatConjuncts)
    | other -> other

  /// sort args lexicographically and remove duplications
  let cleanup args =
    let sb = StringBuilder()
    let len = Dictionary()
    for a in args do
      let start = sb.Length
      sb.Append(a.ToString()) |> ignore
      len.Add(start, (sb.Length-start, a))
    let sb = sb.ToString()
    let sufarr = LCP.sufsort(sb)
    let lcp = LCP.computeLCP(sb, sufarr)
    let mutable res = []
    for i in 0..sb.Length-1 do
      let si = sufarr.[i]
      match len.TryGetValue si with
      | true, (l,a) when lcp.[i] < l ->
        res <- a :: res
      | _ -> ()
    res |> List.rev

  /// cleanup regular ast to lexicographically sorted ast without duplications
  let rec translate = function
    | SaidInfon(pc, i) ->
      SaidInfon(pc, translate i)
    | AndInfon(args) ->
      match args |> List.map translate |> cleanup with
      | [arg] -> arg
      | args -> AndInfon(args)
    | OrInfon(args) ->
      match args |> List.map translate |> cleanup with
      | [arg] -> arg
      | args -> OrInfon(args)
    | ImpliesInfon(l, r) ->
      ImpliesInfon(translate l, translate r)
    | other -> other

  let stage0 ast =
    flatConjuncts ast |> translate