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

open Fuchu

open Microsoft.Research.Dkal.LogicEngine.PPIL
open Microsoft.Research.Dkal.Ast.Infon
open Utils

module SPIL =
  let private solveHash = Utils.genericSolve PPILSolver.solveSPILhash
  let private solveSufArr = Utils.genericSolve PPILSolver.solveSPILsufarr
  let private solveRnd = Utils.genericSolve PPILSolver.solveSPILrnd
  let private rel = RelationInfon []
  let private ra,rb,rc = rel "a", rel "b", rel "c"
  let check solve exp hy qu = Assert.Equal("hy = " + hy.ToString() + "\n" + "qu = " + qu.ToString(), exp, solve hy qu)

  let genericTests solve = 
    let check = check solve
    [
    "stage0" => fun _ ->
      let ast = Stage0.flatConjuncts (AndInfon [AndInfon [rc;ra]; rb; ra])
      Assert.Equal("flatConjuncts", "and(c(), a(), b(), a())", (ast.ToString()))
      let ast = Stage0.stage0 ast
      Assert.Equal("translation", "and(a(), b(), c())", (ast.ToString()))
      let ast = Stage0.stage0 (OrInfon [ast; ast; rb])
      Assert.Equal("orand", "or(and(a(), b(), c()), b())", (ast.ToString()))
    
    "set basic" => fun _ ->
      check [true]
        ["x() && y() -> z()"]
        ["y() && x() -> z()"]
      check [true]
        ["((x()&&y())&&z()) -> w()"]
        ["(x()&&(y()&&z())) -> w()"]
      check [true]
        ["r(1) && r(2) || r(3)"]
        ["r(2) && r(1) || r(3)"]

    "Carlos1" => fun _ ->
      check [true]
              ["(p said x() -> (
                          (q said x() -> (x() && asInfon(true))) &&
                          ((q said x() && asInfon(true)) -> ((x() && asInfon(true)) && asInfon(true))) &&
                          ((q said x() && q said x()) -> x())
                       )
              ) &&
              (((p said x() && asInfon(true)) && p said x()) -> ((q said x() -> (x() && asInfon(true))) &&
                          ((q said x() && asInfon(true)) -> ((x() && asInfon(true)) && asInfon(true))) &&
                          ((q said x() && q said x()) -> x())))"]
              ["p said x() -> (q said x() -> x())"]

    "Carlos2" => fun _ ->
      check [true]
              ["w() -> (x() && y() && z())";
               "(z() && y() && x()) -> w()"]
              ["w() -> w()"]
    ]

  let label s = List.map (fun t -> Test.TestLabel(s, t))

  let testNew solve = 
    let check = check solve
    [
    "quotation distributivity" => fun _ ->
      check [true]
              ["a() -> p said (p said b() && p said c())"]
              ["a() -> p said p said b() && p said p said c()"]

    "evidence" => fun _ ->
      check [true; true; true]
              ["p said (a() && (a() -> b()))"]
              ["p said a()"; "p said (a() || b())"; "p said (c() -> a() && b())"]

    "initial reformat" => fun _ ->
      let H = ["a() && p said (b() && c())" |> Utils.parse]
      let (H,_,_) = Stage1.stage1 H []
      let h = SPIL.initialReformat H.Head
      // TODO: Assert.Equal("reformat", "(a&&p:b&&p:c)", h.ToString());
      Assert.Equal("", 3, h.ChildrenCount)

    "or" => fun _ ->
      check [false; true]
              ["p said (a() || b() || q said c())"]
              ["p said (a() || b() || c())"; "p said (a() || b() || q said c() || d())"]

    "said-or distributivity. todo" => fun _ ->
      check [true]
              ["p said (a() || b() || q said c())"]
              ["p said a() || p said b() || p said q said c()"]
    ]

  let tests = label "SufArr" (genericTests solveSufArr) @ 
              label "Hash" (genericTests solveHash) @
              label "Rnd" (genericTests solveRnd @ testNew solveRnd)