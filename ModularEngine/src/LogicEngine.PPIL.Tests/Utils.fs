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

namespace Microsoft.Research.Dkal.LogicEngine.PPIL.Tests

open System.Text
open System.Collections.Generic
open Fuchu

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.LogicEngine.PPIL
open Microsoft.Research.Dkal.LogicEngine.PPIL.AST
open Microsoft.Research.Dkal.LogicEngine.PPIL.Stage0
open Microsoft.Research.Dkal.LogicEngine.PPIL.Stage2
open Microsoft.Research.Dkal.LogicEngine.PPIL.Stage4

module Utils =
  let parse s =
      let parser = SimpleParser() :> IInfonParser
      parser.SetParsingContext (new ParsingContext("me"))
      let decls = "type Int = System.Int32 \n"+
                  "relation a() relation b() relation c() relation d() relation x() relation y() relation z() relation w() relation r(X:Int) \n" +
                  "relation needSum(X:Int, Y:Int) relation needProduct(X:Int, Y:Int) relation knowsMath(X:Dkal.Principal)"
      let rels = parser.ParseSignature (decls)
      parser.ParseInfon s

  let translate hyp que =
      let h = hyp |> List.map parse |> List.map Stage0.stage0
      let q = que |> List.map parse |> List.map Stage0.stage0
      let (H, Q, input) = Stage1.stage1 h q
      H, Q, input

  let RelationInfon (args: ITerm list) name = 
    let argtypes = [for a in args do yield a.Type]
    App({Name=name; RetType=Type.Infon; ArgsType=argtypes; Identity= None}, args)

  let debugHomonomy s (H:array<Option<AST>>) =
      printf "%s\n" s
      let tostr (h:AST) = s.Substring(h.Key, h.Length)
      H |> Array.iteri (fun i h ->
                      h |> Option.iter (fun h ->
                              if i = h.Key then
                                  printf "leader: %d %s\n" i (tostr h)
                              else
                                  printf "%d point to %d: %s\n" i h.Key (tostr h)
                              )
                          )

  let checkHomonomy (N:IDictionary<int,AST>) (H:IDictionary<int,AST>) =
      let count = ref 0
      H |> Seq.iter (fun kv -> // todo: check N instead
                      let i,h = kv.Key, kv.Value
                      Assert.Equal("homonomy fails", N.[i].ToString(), h.ToString())
                      if i = h.Key then
                          incr count
                    )
      !count

  let genericSolve solver hyp que =
      let hyp = hyp |> List.map parse
      let que = que |> List.map parse
      solver hyp que |> List.map Option.isSome