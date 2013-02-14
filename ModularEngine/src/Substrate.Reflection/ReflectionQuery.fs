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
open Microsoft.Research.Dkal.Substrate
open System.Reflection

type ReflectionQuery(ns: string, meth: MethodInfo, input: ITerm list, output: ITerm list) =
  inherit ASubstrateQuery(ns)
  member x.meth = meth
  member x.input = input
  member x.output = output
  override x.ToString() = "{|"+ns+"| " + output.ToString() + " = " + meth.ToString() + "(" + input.ToString() + ") |}"
  interface ISubstrateQueryTerm with
    member x.Vars = input |> List.collect (fun x -> x.Vars)
    member x.BoundVars = output |> List.collect (fun x -> x.BoundVars)
    member x.Apply subst =
      let app = List.map (fun (x:ITerm) -> x.Apply subst)
      new ReflectionQuery(ns, meth, app input, app output) :> ITerm