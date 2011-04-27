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
      Some {Name = "rule"; RetType = Type.Rule; ArgsType = [Type.Infon; Type.Infon; Type.Action]; Identity = None}
    | "seq" -> 
      Some {Name = "seq"; RetType = Type.Action; ArgsType = [Type.Action; Type.Action]; Identity = None}
    | "send" -> 
      Some {Name = "send"; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | "learn" -> 
      Some {Name = "learn"; RetType = Type.Action; ArgsType = [Type.Infon]; Identity = None}
    | "forget" -> 
      Some {Name = "forget"; RetType = Type.Action; ArgsType = [Type.Infon]; Identity = None}
    | "install" -> 
      Some {Name = "install"; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | "uninstall" -> 
      Some {Name = "uninstall"; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | "emptyInfon" ->
      Some {Name = "emptyInfon"; RetType = Type.Infon; ArgsType = []; Identity = None}
    | "asInfon" ->
      Some {Name = "asInfon"; RetType = Type.Infon; ArgsType = [Type.Boolean]; Identity = None}
    | "implies" ->
      Some {Name = "implies"; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]; Identity = None}
    | "said" ->
      Some {Name = "said"; RetType = Type.Infon; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | "and" ->
      Some {Name = "and"; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]; 
            Identity = Some <| ({Function=(Primitives.SolveFunction "emptyInfon").Value; Args=[]} :> ITerm) }
    | _ -> 
      None

