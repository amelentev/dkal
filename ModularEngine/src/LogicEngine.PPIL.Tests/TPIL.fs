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

module TPIL =
  let private solve = Utils.genericSolve PPILSolver.solveTPIL

  let genericTests solve = [
    "transitive 1" => fun _ ->
      Assert.Equal("",
        [true],
        solve ["a() -> b()"; "b() -> c()"]
              ["a() -> c()"])
    "transitive 2" => fun _ ->
      Assert.Equal("",
        [false; false],
        solve ["a() -> b()"; "b() -> c()"]
              ["c() -> b()"; "c() -> a()"])
    "transitive 3" => fun _ ->
      Assert.Equal("x2x",
        [true],
        solve ["a() -> b()"; "b() -> a()"]
              ["a() -> a()"])
    "transitive 4" => fun _ ->
      Assert.Equal("",
        [true; true; false],
        solve ["a() -> b()"; "b() -> c()"; "c() -> a()"; "b() -> d()"]
              ["c() -> b()"; "a() -> d()"; "d() -> a()"])
    "transitive x2x" => fun _ ->
      Assert.Equal("x2x axiom",
        [true],
        solve []
              ["a() -> a()"])
    
    "transitive -> &&. TSPIL. todo" => fun _ ->
        Assert.Equal("",
          [true],
          solve ["a() -> b() && c()"]
                ["a() -> b()"])
    ]
  let tests = genericTests solve