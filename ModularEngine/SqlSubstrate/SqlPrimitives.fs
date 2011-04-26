namespace Microsoft.Research.Dkal.SqlSubstrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

type SqlPrimitives =
    
  /// Given an overloaded function name and the type of one of its parameters
  /// it looks the type information to see if there is a match.
  static member SolveOverloadOperator (f: string) (t: IType) =
    match f with
    | "and" when t = Type.Bool -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "or" when t = Type.Bool -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "not" when t = Type.Bool -> 
      Some {Name = f; ArgsType = [t]; RetType = t}
    | "eq" | "neq" when SqlPrimitives.HasEquality t -> 
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Bool}
    | "le" | "leq" | "gt" | "gte" when SqlPrimitives.HasOrdering t ->
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Bool}
    | "plus" when SqlPrimitives.HasSum t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "minus" when SqlPrimitives.HasSubstraction t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "uminus" when SqlPrimitives.HasArithmeticNegation t ->
      Some {Name = f; ArgsType = [t]; RetType = t}
    | "times" when SqlPrimitives.HasMultiplication t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "divide" when SqlPrimitives.HasDivision t ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
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

