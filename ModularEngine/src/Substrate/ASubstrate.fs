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
namespace Microsoft.Research.Dkal.Substrate.Reflection

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open System.Collections.Generic

type ASubstrate(namespaces: string list, solvefun) =
  interface ISubstrate with
    member s.RequiredVars st =  st.Vars
    member s.Namespaces = new HashSet<_>(namespaces)
    member xs.Solve queries substs =
      let solve1Many substs query =
        substs |> Seq.collect (solvefun query)
      queries |> Seq.fold solve1Many substs
    member xs.Update terms = failwithf "not supported %O" terms
    member xs.AreConsistentUpdates _ = true
