namespace Microsoft.Research.Dkal.Substrate.Sql

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

module SqlPrimitives =

  // Literals
  [<Literal>] 
  let And = "and"
  [<Literal>] 
  let Or = "Or"
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
  let PpalName = "ppalName"
  [<Literal>] 
  let AsBoolean = "asBoolean"

  /// Returns true iff the given IType supports equality in SQL
  let private HasEquality (t: IType) =
    match t with
    | Substrate t' -> 
      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IEquatable"))
    | _ -> false

  /// Returns true iff the given IType supports ordered comparisson in SQL
  let private HasOrdering (t: IType) = 
    match t with
    | Substrate t' -> 
      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IComparable"))
    | _ -> false
  
  /// Returns true iff the given IType supports addition in SQL
  let private HasSum (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports substraction in SQL
  let private HasSubstraction (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports unary arithmetic negation in SQL
  let private HasArithmeticNegation (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports multiplication in SQL
  let private HasMultiplication (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Returns true iff the given IType supports division in SQL
  let private HasDivision (t: IType) =
    match t with
    | Substrate t' when t' = typeof<int> || t' = typeof<float> -> true
    | _ -> false

  /// Given an overloaded function name and the type of one of its parameters
  /// it looks the type information to see if there is a match.
  let SolveOverloadOperator (f: string) (t: IType) =
    match f with
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
    | PpalName when t = Type.Principal ->
      Some {Name = f; ArgsType = [Type.Principal]; RetType = Type.String; Identity=None}
    | AsBoolean ->
      Some {Name = f; RetType = Type.Boolean; ArgsType = [Type.SubstrateQuery]; Identity = None}
    | _ ->
      None