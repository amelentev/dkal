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

namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Factories
open Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

/// The SimpleSqlPrettyPrinter prints substrate elements into the simple concrete syntax
type SimpleSqlPrettyPrinter() =
    
  interface ISubstratePrettyPrinter with
    member spp.PrintTerm t =
      match t with
      | :? DummySubstrateQueryTerm as t ->
        PrettyPrinter.PrettyPrint <| spp.TokenizeTerm t.Query
      | :? SqlSubstrateModifyTerm as t ->
        PrettyPrinter.PrettyPrint <| 
          [ ManyTokens <| spp.TokenizeTerm t.Query;
            TextToken <| " | update ";
            TextToken <| String.concat ", " 
              (Seq.map (fun (kv: KeyValuePair<_,_>) -> kv.Key + " := " + (PrettyPrinter.PrettyPrint <| spp.TokenizeTerm kv.Value)) t.ColsMapping) ] 
      | :? SqlSubstrateInsertTerm as t ->
        PrettyPrinter.PrettyPrint <| 
          [ TextToken <| String.concat ", " 
              (Seq.map (fun (kv: KeyValuePair<_,_>) -> kv.Key + " := " + (PrettyPrinter.PrettyPrint <| spp.TokenizeTerm kv.Value)) t.Values);
            TextToken <| " | insert " + t.Table ] 
      | :? SqlSubstrateDeleteTerm as t ->
        PrettyPrinter.PrettyPrint <| 
          [ ManyTokens <| spp.TokenizeTerm t.Query;
            TextToken <| " | delete " + t.Table ] 
      | _ -> failwithf "Unexpected term when printing simple SQL syntax: %O" t

  member private spp.PrintTerm mt = (spp :> ISubstratePrettyPrinter).PrintTerm mt

  static member FindFunctionSymbol f = 
    match f with
    | "and" -> "&&", true
    | "or" -> "||", true
    | "not" -> "!", false
    | "eq" -> "==", true
    | "neq" -> "!=", true
    | "lt" -> "<", true
    | "lte" -> "<=", true
    | "gt" -> ">", true
    | "gte" -> ">=", true
    | "plus" -> "+", true
    | "minus" -> "-", true
    | "times" -> "*", true
    | "div" -> "/", true
    | "uminus" -> "-", false
    | f -> f, false

  member private spp.TokenizeTerm mt =
    match mt with
    | App(f, []) -> [ TextToken f.Name ]
    | App(f, mts) -> 
      let fSymbol, infix = SimpleSqlPrettyPrinter.FindFunctionSymbol f.Name
      let args = List.map spp.TokenizeTerm mts
      if infix then
        [ TextToken "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken <| " " + fSymbol + " "] @ t2) args
        @ [ TextToken ")" ]
      else
        [ TextToken <| fSymbol + "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken ", "] @ t2) args
        @ [ TextToken ")"]
    | Var(v) -> [TextToken v.Name]
    | True -> [TextToken "true"]
    | False -> [TextToken "false"]
    | PrincipalConstant(p) -> [TextToken(p)]
    | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
    | SubstrateConstant(o) -> [TextToken(o.ToString())]
    | :? ISubstrateTerm as t ->
      let substrate = SubstrateMap.GetSubstrate t.Namespace
      let pp = SubstratePrettyPrinterFactory.SubstratePrettyPrinter substrate "simple"
      let printedSubstrateTerm = pp.PrintTerm t
      [ TextToken <| "{| \"" + t.Namespace + "\" | " + printedSubstrateTerm + " |}" ]      
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %O" mt
   
