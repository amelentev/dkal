namespace Microsoft.Research.Dkal.Ast

  open System.Collections.Generic
  open Microsoft.Research.Dkal.Ast

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
        Some {Name = "rule"; RetTyp = Rule; ArgsTyp = [Infon; Infon; Action]}
      | "seq" -> 
        Some {Name = "seq"; RetTyp = Action; ArgsTyp = [Action; Action]}
      | "send" -> 
        Some {Name = "send"; RetTyp = Action; ArgsTyp = [Type.Principal; Infon]}
      | "learn" -> 
        Some {Name = "learn"; RetTyp = Action; ArgsTyp = [Infon]}
      | "forget" -> 
        Some {Name = "forget"; RetTyp = Action; ArgsTyp = [Infon]}
      | "install" -> 
        Some {Name = "install"; RetTyp = Action; ArgsTyp = [Rule]}
      | "uninstall" -> 
        Some {Name = "uninstall"; RetTyp = Action; ArgsTyp = [Rule]}
      | "sql" ->
        Some {Name = "sql"; RetTyp = Substrate; ArgsTyp = [Type.String]}
      | "xml" ->
        Some {Name = "xml"; RetTyp = Substrate; ArgsTyp = [Type.String]}
      | "emptyInfon" ->
        Some {Name = "emptyInfon"; RetTyp = Infon; ArgsTyp = []}
      | "asInfon" ->
        Some {Name = "asInfon"; RetTyp = Infon; ArgsTyp = [Bool; Substrate]}
      | "said" ->
        Some {Name = "said"; RetTyp = Infon; ArgsTyp = [Type.Principal; Infon]}
      | _ -> 
        None

    /// Given an overloaded function name and the type of one of its parameters
    /// it looks the type information to see if there is a match.
    static member SolveOverloadOperator (f: string) (t: Type) =
      match f with
      | "and" when t.hasConjunction()  -> 
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | "or" when t.hasDisjunction() -> 
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | "implies" when t.hasImplication() -> 
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | "not" when t.hasLogicalNegation() -> 
        Some {Name = f; ArgsTyp = [t]; RetTyp = t}
      | "eq" | "neq" when t.hasEquality() -> 
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = Type.Bool}
      | "le" | "leq" | "gt" | "gte" when t.hasOrdering() ->
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = Type.Bool}
      | "plus" when t.hasSum() ->
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | "minus" when t.hasSubstraction() ->
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | "uminus" when t.hasArithmeticNegation() ->
        Some {Name = f; ArgsTyp = [t]; RetTyp = t}
      | "times" when t.hasMultiplication() ->
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | "divide" when t.hasDivision() ->
        Some {Name = f; ArgsTyp = [t;t]; RetTyp = t}
      | _ -> 
        None
