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
namespace Microsoft.Research.Dkal.Ast.Substrate.Rdf.Syntax

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate.Rdf
open Microsoft.Research.Dkal.Ast
open System.Text.RegularExpressions

type SparqlParser() = 
  let mutable Context: Option<IParsingContext> = None
  let mutable Namespace = ""
  interface ISubstrateParser with
    member sp.SetParsingContext (context: IParsingContext) = 
      Context <- Some context
    member sp.SetNamespace (ns: string) = 
      Namespace <- ns
    member sp.SetSubstrate (substrate: ISubstrate) =
      ()
    member sp.ParseTerm squery =
      let squery = squery.Trim([|' '; '"'|])
      let m = Regex.Match(squery, "select ([?]\w*(, [?]\w*)*)", RegexOptions.IgnoreCase)
      let createvar varName = {Name=varName; Type=Context.Value.VariableType(varName)} :> IVar
      let outputs = m.Groups.[1].Value.Split([|' '; ','|]) |> Seq.filter (fun s -> s.Length > 0) |> Seq.map (fun s -> s.Substring(1)) |> Seq.map createvar |> List.ofSeq
      let inputs = [for m in Regex.Matches(squery, "@\w+") do yield m] |> List.map(fun m -> m.Value.Substring(1)) |> List.map createvar
      SparqlQueryTerm(Namespace, squery, inputs, outputs) :> ISubstrateTerm
