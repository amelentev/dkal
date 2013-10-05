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
open Utils

module TSPIL2 =
  let private solve = Utils.genericSolve PPILSolver.solveTSPIL2
  let private solveOld = Utils.genericSolve PPILSolver.solveTSPIL_DS

  let tests = 
    [
    "TSPIL2 1" => fun _ ->
        Assert.Equal("",
          [true; true; true; false; true],
          solve ["a() -> b()"; "a() -> c()"]
                ["a() -> c() && b()"; "a() && b() -> c()"; "a() && c() -> b()"; "b() -> c()"; "a() -> b() || c()"])
    "TSPIL2 2" => fun _ ->
        Assert.Equal("",
          [true; false; true; true; false],
          solve ["a() -> c()"; "b() -> d()"]
                ["a() && b() -> c() && d()"; "a() -> d() && c()"; "a() && b() -> c()"; "a() && c() -> c()"; "a() -> d()"])
    "TSPIL2 3" => fun _ ->
        Assert.Equal("",
          [true; true; true],
          solve ["a() -> b() && c()"; "a() -> d() && e()"; "c() && d() -> f()"; "b() && f() -> x()"]
                ["a() -> f()"; "a() -> b() && f()"; "a() -> x()"])
    ] @ (TPIL.genericTests solve)