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

open System.Text.RegularExpressions
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Substrate

type ReflectionParser() = 
  inherit ASubstrateParser(fun squery context ns rs ->
    let squery = squery.Trim([|' '|])
    let m = Regex.Match(squery, 
              "(?<out>\w+(\W+\w+)*\s*=\s*)?"+ // O1,..,On = 
              "(?<meth>\w+)\s*"+
              "\((?<in>\w+(\W+\w+)*)?\)", RegexOptions.ExplicitCapture) // (I1,..,Im)
    if not m.Success then failwithf "Invalid query: %s" squery
    let getvars (m:Group) (sep:string) = 
      if not m.Success then List.empty
      else m.Value.Split(sep.ToCharArray()) |> Seq.map (fun s -> s.Trim([|' '|])) |> Seq.filter (fun s -> s.Length>0) |> List.ofSeq
    let createvars (name:string) sep = 
      getvars m.Groups.[name] sep |> List.map (fun varName -> {Name=varName; Type=context.VariableType(varName)} :> ITerm)
    let meth = (rs :?> ReflectionSubstrate).clas.GetMethod(m.Groups.["meth"].Value)
    ReflectionQuery(ns, meth, createvars "in" ", ", createvars "out" ", =") :> ISubstrateTerm
  )
  interface ISubstrateParser