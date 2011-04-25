namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

type Primitives =
    
//  /// Returns true iff the function name correspond to a function that behaves
//  /// associatively.
//  static member IsAssociative (f: string) = 
//    match f with
//    | "and" | "or" | "plus" | "times" -> true
//    | _ -> false

  /// Given a primitive function name it returns a Fuction, if anyone matches;
  /// None otherwise
  static member SolveFunction (f: string) =
    match f with
    | "rule" -> 
      Some {Name = "rule"; RetType = Type.Rule; ArgsType = [Type.Infon; Type.Infon; Type.Action]}
    | "seq" -> 
      Some {Name = "seq"; RetType = Type.Action; ArgsType = [Type.Action; Type.Action]}
    | "send" -> 
      Some {Name = "send"; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]}
    | "learn" -> 
      Some {Name = "learn"; RetType = Type.Action; ArgsType = [Type.Infon]}
    | "forget" -> 
      Some {Name = "forget"; RetType = Type.Action; ArgsType = [Type.Infon]}
    | "install" -> 
      Some {Name = "install"; RetType = Type.Action; ArgsType = [Type.Rule]}
    | "uninstall" -> 
      Some {Name = "uninstall"; RetType = Type.Action; ArgsType = [Type.Rule]}
    | "emptyInfon" ->
      Some {Name = "emptyInfon"; RetType = Type.Infon; ArgsType = []}
    | "asInfon" ->
      Some {Name = "asInfon"; RetType = Type.Infon; ArgsType = [Type.Bool]}
    | "implies" ->
      Some {Name = "implies"; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]}
    | "said" ->
      Some {Name = "said"; RetType = Type.Infon; ArgsType = [Type.Principal; Type.Infon]}
    | "and" ->
      Some {Name = "and"; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]}
    | _ -> 
      None

//  /// Given an overloaded function name and the type of one of its parameters
//  /// it looks the type information to see if there is a match.
//  static member SolveOverloadOperator (f: string) (t: IType) =
//    let t = t :?> Type // TODO: move this information to IType interface
//    match f with
//    | "and" when t.hasConjunction()  -> 
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | "or" when t.hasDisjunction() -> 
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | "implies" when t.hasImplication() -> 
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | "not" when t.hasLogicalNegation() -> 
//      Some {Name = f; ArgsType = [t]; RetType = t}
//    | "eq" | "neq" when t.hasEquality() -> 
//      Some {Name = f; ArgsType = [t;t]; RetType = Type.Bool}
//    | "le" | "leq" | "gt" | "gte" when t.hasOrdering() ->
//      Some {Name = f; ArgsType = [t;t]; RetType = Type.Bool}
//    | "plus" when t.hasSum() ->
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | "minus" when t.hasSubstraction() ->
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | "uminus" when t.hasArithmeticNegation() ->
//      Some {Name = f; ArgsType = [t]; RetType = t}
//    | "times" when t.hasMultiplication() ->
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | "divide" when t.hasDivision() ->
//      Some {Name = f; ArgsType = [t;t]; RetType = t}
//    | _ -> 
//      None
