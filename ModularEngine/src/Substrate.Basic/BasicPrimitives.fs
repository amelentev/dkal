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

/// Defines the primitive functions used to construct Application elements
/// in the BasicSubstrate builders
module Microsoft.Research.Dkal.Substrate.Basic.BasicPrimitives

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Tree

  // Supported namespace
  [<Literal>] 
  let BasicNamespace = "basic"

  // Literals
  [<Literal>] 
  let And = "and"
  [<Literal>] 
  let Or = "or"
  [<Literal>] 
  let Not = "not"
  [<Literal>] 
  let Eq = "eq"
  [<Literal>] 
  let Neq = "neq"
  [<Literal>] 
  let Lt = "lt"
  [<Literal>] 
  let Lte = "lte"
  [<Literal>] 
  let Gt = "gt"
  [<Literal>] 
  let Gte = "gte"
  [<Literal>] 
  let Plus = "plus"
  [<Literal>] 
  let Minus = "minus"
  [<Literal>] 
  let Uminus = "uminus"
  [<Literal>] 
  let Times = "times"
  [<Literal>] 
  let Divide = "divide"
  [<Literal>] 
  let AsBoolean = "asBoolean"
  // collection functions
  [<Literal>]
  let Exists = "exists"
  [<Literal>]
  let IsEmpty = "isEmpty"
  [<Literal>]
  let Date = "date"
  [<Literal>]
  let Now = "now"

  /// non overloaded functions
  let functions =
    [
      {Name = Date; ArgsType = [Type.String]; RetType = Type.DateTime; Identity = None};
      {Name = Now; ArgsType = []; RetType = Type.DateTime; Identity = None}
    ] |> List.map (fun f -> f.Name, f) |> dict

  /// Returns true iff the given IType supports equality in Basic substrate
  let private HasEquality (t: IType) =
    match t with
    | Substrate t' -> 
      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IEquatable"))
    | Principal -> 
      true
    | _ -> false

  /// Returns true iff the given IType supports ordered comparisson in Basic substrate
  let private HasOrdering (t: IType) = 
    match t with
    | Substrate t' -> 
      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IComparable"))
    | _ -> false
  
  /// Returns true iff the given IType supports addition in Basic substrate
  let private HasSum (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports substraction in Basic substrate
  let private HasSubstraction (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports unary arithmetic negation in Basic substrate
  let private HasArithmeticNegation (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports multiplication in Basic substrate
  let private HasMultiplication (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports division in Basic substrate
  let private HasDivision (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Given an overloaded function name and the type of one of its parameters
  /// it looks the type information to see if there is a match.
  let SolveOverloadOperator (f: string) (t: IType) =
    match f with
    | AsBoolean when t = Type.SubstrateQuery ->
      Some {Name = f; ArgsType = [t]; RetType = Type.Boolean; Identity = None}
    | And when t = Type.Boolean -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t; 
            Identity = Some True}
    | Or when t = Type.Boolean -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some False}
    | Not when t = Type.Boolean -> 
      Some {Name = f; ArgsType = [t]; RetType = t; Identity = None}
    | Eq | Neq when HasEquality t -> 
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Boolean; Identity = None}
    | Lt | Lte | Gt | Gte when HasOrdering t ->
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Boolean; Identity = None}
    | Plus when HasSum t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(Constant(0))}
    | Minus when HasSubstraction t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(Constant(0))}
    | Uminus when HasArithmeticNegation t ->
      Some {Name = f; ArgsType = [t]; RetType = t; Identity = None}
    | Times when HasMultiplication t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(Constant(1))}
    | Divide when HasDivision t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(Constant(1))}
    | _ ->
      match t with
      | CollectionType(et) ->
        match f with
        | Exists -> Some {Name = f; ArgsType = [t; et]; RetType = Type.Boolean; Identity = None}
        | IsEmpty -> Some {Name = f; ArgsType = [t]; RetType = Type.Boolean; Identity = None}
        | _ -> None
      | _ -> None

  /// Given an function name and arguments it looks the type information to see if there is a match.
  let SolveFunction (f: string) (args: ITerm list) =
    match functions.TryGetValue(f) with
    | true, v -> Some(v)
    | _ -> SolveOverloadOperator f args.Head.Type