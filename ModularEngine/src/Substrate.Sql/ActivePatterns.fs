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

/// Defines the public interface on how to pattern match AST elements defined
/// for the SqlSubstrate
[<AutoOpen>]
module Microsoft.Research.Dkal.Substrate.Sql.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  /// Matches a conjunction
  let (|AndBool|_|) mt =  match mt with
                          | App({Name=SqlPrimitives.And; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                          | _ -> None
  /// Matches a disjunction
  let (|OrBool|_|) mt = match mt with
                        | App({Name=SqlPrimitives.Or; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                        | _ -> None
  /// Matches an asBoolean construct (used to nest queries)
  let (|AsBoolean|_|) mt =  match mt with 
                            | App({Name=SqlPrimitives.AsBoolean}, [exp]) -> 
                              match exp with
                              | :? ISubstrateQueryTerm as exp -> Some exp
                              | _ -> failwith "Expecting ISubstrateQueryTerm in AsBoolean"
                            | _ -> None

  /// Matches a table.column term
  let (|Column|_|) mt = match mt with
                        | App({Name=fn}, []) when fn.Contains(".") ->
                          let i = fn.IndexOf('.')
                          Some (fn.Substring(0, i), fn.Substring(i+1))
                        | _ -> None