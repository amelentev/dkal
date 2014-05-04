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
open Utils

module Tests =
  let private solve = Utils.genericSolve PPILSolver.solveBPIL
  [<Tests>]
  let tests = 
      "testsuite" =>> [
          "stage1" =>
              fun _ ->
                  let (h,q,input) = translate ["p1 said (a() && q said p said b())"] []
                  Assert.Equal("ast serialization", "p1 said (a()&q said p said b())", input)
                  Assert.Equal("ast tostring", "p1:(a&q:p:b)", h.Head.ToString())

          "stage2" => 
              fun _ ->
                  let (h,q,s) = translate ["p1 said (p2 said a() && q3 said p4 said b())"] []
                  let trie = Trie([])
                  let lst = Stage2.constructTrie trie [] h.Head
                  Assert.Equal("trie", "(p1(p2(),q3(p4())))", trie.ToString())

          "stage3 1" =>
              fun _ ->
                  let (h,q,s) = translate ["a() && b() -> a() && b()"] []
                  let (N, V) = Stage2.constructNodesnVertices h
                  let (_,_,H) = Stage3.homonomySufArr s [] [] (N,V)
                  //debugHomonomy s H
                  Assert.Equal("", 4, checkHomonomy N H)
                  let (_,_,H) = Stage3.homonomyHash "" h q (N,V)
                  Assert.Equal("", 4, checkHomonomy N H)
          "stage3 2" =>
              fun _ ->
                  let (h,q,s) = translate ["p said a() && b() -> p said a() && b() -> a() && p said b()"] []
                  let (N, V) = Stage2.constructNodesnVertices h
                  let (_,_,H) = Stage3.homonomySufArr s [] [] (N,V)
                  //debugHomonomy s H
                  Assert.Equal("", 8, checkHomonomy N H)
                  let (_,_,H) = Stage3.homonomyHash "" h q (N,V)
                  Assert.Equal("", 8, checkHomonomy N H)
          "stage4" =>
              fun _ ->
                  let (h,q,s) = translate ["r(1) && r(2) -> r(3)"; "r(1)"; "r(2)"] []
                  let (N, V) = Stage2.constructNodesnVertices h
                  let (_,_,H) = Stage3.homonomySufArr s [] [] (N,V)
                  //debugHomonomy s H
                  let T = Stage4.preprocess H h
                  //printf "%A\n" T
                  // todo: check
                  ()
          "stage5 1" =>
              fun _ ->
                  Assert.Equal("->e", 
                    [true],
                    solve ["a()"; "a() -> b()"] 
                          ["b()"])
          "stage5 2" =>
              fun _ ->
                  Assert.Equal("saids",
                    [true; true; false],
                    solve ["r said ((p said x()) && (r said x()))"; "r said p said (x() -> y())"; "r said p said (y() -> z())"]
                          ["r said p said z()"; "r said r said x()"; "r said r said z()"])
          "||" =>
              fun _ ->
                  Assert.Equal("derivation", 
                    [true; true; true; true; true; true; false; false; true],
                    solve ["a() && b()"; "b()->c()"]
                          ["a() || b()"; "a()||c()"; "c()||d()"; "b()||a()"; "b()&&a()"; "d()||b()"; "d()"; "x() || y()"; "x() -> a()"])
          "said &&" =>
              fun _ ->
                  Assert.Equal("said and", 
                    [true],
                    solve ["p said r(1)"; "p said p said r(2)"; "p said r(1) -> r(1)"; "p said p said r(2) -> r(2)"]
                          ["r(1) && r(2)"])
          "->e bug" =>
              fun _ ->
                  Assert.Equal("",
                    [false; false],
                    solve ["a()"]
                          ["a() -> b()"; "b()"])
      ] @ SPIL.tests @ TPIL.tests @ TSPIL.tests @ TSPIL2.tests

  [<EntryPoint>]
  let main argv = 
      let r = tests
            //|> Test.filter (fun x -> x.Contains "TSPIL2")
            |> Test.filter (fun x -> not (x.EndsWith "todo"))
            |> run
      System.Console.ReadLine() |> ignore
      r