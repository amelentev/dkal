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
/// in the Ast module
[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces

  /// Active pattern to match the infon type
  let (|Infon|_|) t = if t = Type.Infon then Some () else None

  /// Active pattern to match the principal type
  let (|Principal|_|) t = if t = Type.Principal then Some () else None

  /// Active pattern to match the substrate update type
  let (|SubstrateUpdate|_|) t = if t = Type.SubstrateUpdate then Some () else None

  /// Active pattern to match the substrate query type
  let (|SubstrateQuery|_|) t = if t = Type.SubstrateQuery then Some () else None

  /// Active pattern to match the action type
  let (|Action|_|) t = if t = Type.Action then Some () else None

  /// Active pattern to match the condition type
  let (|Condition|_|) t = if t = Type.Condition then Some () else None

  /// Active pattern to match the rule type
  let (|Rule|_|) t = if t = Type.Rule then Some () else None

  /// Active pattern to match the evidence type
  let (|Evidence|_|) t = if t = Type.Evidence then Some () else None

  /// Active pattern to match the substrate (.NET elements) type
  let (|Substrate|_|) (t: IType) =  match t with
                                    | :? Type.Substrate as t -> Some t.Type
                                    | _ -> None

  /// Active pattern to match variables
  let (|Var|_|) (t: ITerm) =  match t with
                              | :? Variable as v -> Some v
                              | _ -> None

  /// Active pattern to match constants
  let (|Const|_|) (t: ITerm) =  match t with
                                | :? IConst as c -> Some c
                                | _ -> None

  /// Active pattern to match .NET type elements (integers, strings, etc.)
  let (|SubstrateConstant|_|) mt =  match mt with
                                    | Const(c) ->
                                      match c with
                                      | :? Constant as c -> 
                                        match (c :> ITerm).Type with 
                                        | Substrate(_) -> Some c.Value
                                        | _ -> None
                                      | _ -> None
                                    | _ -> None

  /// Active pattern to match principal constants
  let (|PrincipalConstant|_|) mt =  match mt with
                                    | Const(c) -> 
                                      match c with
                                      | :? PrincipalConstant as p -> Some p.Name
                                      | _ -> None
                                    | _ -> None

  /// Active pattern to match true boolean literal
  let (|True|_|) mt = match mt with
                      | Const(c) -> 
                        match c with
                        | :? Constant as bc -> 
                          match bc.Value with
                          | :? bool as b when b = true -> Some ()
                          | _ -> None
                        | _ -> None
                      | _ -> None

  /// Active pattern to match false boolean literal
  let (|False|_|) mt =  match mt with
                        | Const(c) -> 
                          match c with
                          | :? Constant as bc ->
                            match bc.Value with
                            | :? bool as b when b = false -> Some ()
                            | _ -> None
                          | _ -> None
                        | _ -> None

  /// Active pattern to match forall quantified terms
  let (|Forall|_|) (mt: ITerm) =  match mt with
                                  | :? ForallTerm as fit -> Some (fit.Var, fit.Term)
                                  | _ -> None

  let (|Collection|_|) (mt: ITerm) = match mt with
                                     | :? Collection as c -> Some c.elems
                                     | _ -> None

  let (|CollectionType|_|) (t: IType) = match t with
                                        | :? Type.CollectionType as ct -> Some(ct.elemType)
                                        | _ -> None