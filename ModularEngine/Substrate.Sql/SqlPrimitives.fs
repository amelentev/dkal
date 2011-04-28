namespace Microsoft.Research.Dkal.Substrate.Sql

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

type SqlPrimitives =
    
  /// Given an overloaded function name and the type of one of its parameters
  /// it looks the type information to see if there is a match.
  static member SolveOverloadOperator (f: string) (t: IType) =
    match f with
    | "and" when t = Type.Boolean -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t; 
            Identity = Some True}
    | "or" when t = Type.Boolean -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some False}
    | "not" when t = Type.Boolean -> 
      Some {Name = f; ArgsType = [t]; RetType = t; Identity = None}
    | "eq" | "neq" when SqlPrimitives.HasEquality t -> 
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Boolean; Identity = None}
    | "lt" | "lte" | "gt" | "gte" when SqlPrimitives.HasOrdering t ->
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Boolean; Identity = None}
    | "plus" when SqlPrimitives.HasSum t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(SubstrateConstant(0))}
    | "minus" when SqlPrimitives.HasSubstraction t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(SubstrateConstant(0))}
    | "uminus" when SqlPrimitives.HasArithmeticNegation t ->
      Some {Name = f; ArgsType = [t]; RetType = t; Identity = None}
    | "times" when SqlPrimitives.HasMultiplication t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(SubstrateConstant(1))}
    | "divide" when SqlPrimitives.HasDivision t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t;
            Identity = Some <| Const(SubstrateConstant(1))}
    | "ppalName" when t = Type.Principal ->
      Some {Name = f; ArgsType = [Type.Principal]; RetType = Type.String; Identity=None}
    | _ ->
      None

  static member private HasEquality (t: IType) =
    match t with
    | Substrate t' -> 
      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IEquatable"))
    | _ -> false

  static member private HasOrdering (t: IType) = 
    match t with
    | Substrate t' -> 
      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IComparable"))
    | _ -> false
  
  static member private HasSum (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  static member private HasSubstraction (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  static member private HasArithmeticNegation (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  static member private HasMultiplication (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  static member private HasDivision (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

