namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

type Primitives =
    
  /// Returns true iff the function name correspond to a function that behaves
  /// associatively.
  static member IsAssociative (f: string) = 
    match f with
    | "and" | "or" | "plus" | "times" -> true
    | _ -> false

  /// Given a primitive function name it returns a Fuction, if anyone matches;
  /// None otherwise
  static member SolveFunction (f: string) =
    match f with
    | "rule" -> 
      Some {Name = "rule"; RetType = Rule; ArgsType = [Infon; Infon; Action]}
    | "seq" -> 
      Some {Name = "seq"; RetType = Action; ArgsType = [Action; Action]}
    | "send" -> 
      Some {Name = "send"; RetType = Action; ArgsType = [Type.Principal; Infon]}
    | "learn" -> 
      Some {Name = "learn"; RetType = Action; ArgsType = [Infon]}
    | "forget" -> 
      Some {Name = "forget"; RetType = Action; ArgsType = [Infon]}
    | "install" -> 
      Some {Name = "install"; RetType = Action; ArgsType = [Rule]}
    | "uninstall" -> 
      Some {Name = "uninstall"; RetType = Action; ArgsType = [Rule]}
    | "sql" ->
      Some {Name = "sql"; RetType = Substrate; ArgsType = [Type.String]}
    | "xml" ->
      Some {Name = "xml"; RetType = Substrate; ArgsType = [Type.String]}
    | "emptyInfon" ->
      Some {Name = "emptyInfon"; RetType = Infon; ArgsType = []}
    | "asInfon" ->
      Some {Name = "asInfon"; RetType = Infon; ArgsType = [Bool]}
    | "said" ->
      Some {Name = "said"; RetType = Infon; ArgsType = [Type.Principal; Infon]}
    | _ -> 
      None

  /// Given an overloaded function name and the type of one of its parameters
  /// it looks the type information to see if there is a match.
  static member SolveOverloadOperator (f: string) (t: IType) =
    let t = t :?> Type // TODO: move this information to IType interface
    match f with
    | "and" when t.hasConjunction()  -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "or" when t.hasDisjunction() -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "implies" when t.hasImplication() -> 
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "not" when t.hasLogicalNegation() -> 
      Some {Name = f; ArgsType = [t]; RetType = t}
    | "eq" | "neq" when t.hasEquality() -> 
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Bool}
    | "le" | "leq" | "gt" | "gte" when t.hasOrdering() ->
      Some {Name = f; ArgsType = [t;t]; RetType = Type.Bool}
    | "plus" when t.hasSum() ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "minus" when t.hasSubstraction() ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "uminus" when t.hasArithmeticNegation() ->
      Some {Name = f; ArgsType = [t]; RetType = t}
    | "times" when t.hasMultiplication() ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | "divide" when t.hasDivision() ->
      Some {Name = f; ArgsType = [t;t]; RetType = t}
    | _ -> 
      None
