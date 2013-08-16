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

module TSPIL =
  let private solve = Utils.genericSolve PPILSolver.solveTSPIL

  let tests = 
    [
    "TSPIL 1" => fun _ ->
        Assert.Equal("",
          [true; true],
          solve ["a() -> b() && c()"]
                ["a() -> b()"; "a() && d() -> c()"])

    "TSPIL 2" => fun _ ->
        Assert.Equal("",
          [true; false],
          solve ["a() -> b() && c()"]
                ["a() -> b() || c()"; "a()||b() -> c()"])

    "TSPIL 3" => fun _ ->
        Assert.Equal("",
          [true; true; true; true; false],
          solve ["a() || b() -> c() && d()"]
                ["a() || b() -> d()"; "a() -> c() && d()"; "b() -> c()"; "a()&&b() -> c()||d()"; "c() -> a()"])

    "TSPIL 4" => fun _ ->
        Assert.Equal("",
          [true; true; true; true; false],
          solve ["a() -> b() && c()"; "c() -> d() && e()"]
                ["a() -> e()"; "a() -> e()"; "a() && b() && c() -> d() && e()"; "a() -> d()||e()"; "b()||c() -> e()"])
    ] @ (TPIL.genericTests solve)