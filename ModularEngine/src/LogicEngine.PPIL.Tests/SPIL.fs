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
  let private solve = Utils.genericSolve PPILSolver.solveSPILhash
  let private rel = RelationInfon []
  let private ra,rb,rc = rel "a", rel "b", rel "c"

  let tests = [
    "stage0" => fun _ ->
      let ast = Stage0.flatConjuncts (AndInfon [AndInfon [rc;ra]; rb; ra])
      Assert.Equal("flatConjuncts", "and(c(), a(), b(), a())", (ast.ToString()))
      let ast = Stage0.stage0 ast
      Assert.Equal("translation", "and(a(), b(), c())", (ast.ToString()))
      let ast = Stage0.stage0 (OrInfon [ast; ast; rb])
      Assert.Equal("orand", "or(and(a(), b(), c()), b())", (ast.ToString()))
    
    "set basic" => fun _ ->
      Assert.Equal("",
        [true],
        solve ["x() && y() -> z()"]
              ["y() && x() -> z()"])
      Assert.Equal("", 
        [true],
        solve ["((x()&&y())&&z()) -> w()"]
              ["(x()&&(y()&&z())) -> w()"])
      Assert.Equal("", 
        [true],
        solve ["r(1) && r(2) || r(3)"]
              ["r(2) && r(1) || r(3)"])

    "Carlos1" => fun _ ->
      Assert.Equal("",
        [true],
        solve ["(p said x() -> (
                          (q said x() -> (x() && asInfon(true))) &&
                          ((q said x() && asInfon(true)) -> ((x() && asInfon(true)) && asInfon(true))) &&
                          ((q said x() && q said x()) -> x())
                       )
              ) &&
              (((p said x() && asInfon(true)) && p said x()) -> ((q said x() -> (x() && asInfon(true))) &&
                          ((q said x() && asInfon(true)) -> ((x() && asInfon(true)) && asInfon(true))) &&
                          ((q said x() && q said x()) -> x())))"]
              ["p said x() -> (q said x() -> x())"])

    "set || intro. todo" => fun _ ->
      Assert.Equal("",
        [true],
        solve ["a()||b()"]
              ["a()||b()||c()"])
    ]