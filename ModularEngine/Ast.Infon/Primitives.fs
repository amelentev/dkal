namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

type Primitives =

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

