namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

type Primitives =

  /// Given a primitive function name it returns a Fuction, if anyone matches;
  /// None otherwise
  static member SolveFunction (f: string) =
    match f with
    // Rules
    | "seqRule" -> 
      Some {Name = "seqRule"; RetType = Type.Rule; ArgsType = [Type.Rule; Type.Rule]; 
            Identity = Some <| ({Function=(Primitives.SolveFunction "emptyRule").Value; Args=[]} :> ITerm) }
    | "emptyRule" -> 
      Some {Name = "emptyRule"; RetType = Type.Rule; ArgsType = []; Identity = None}
    | "rule" -> 
      Some {Name = "rule"; RetType = Type.Rule; ArgsType = [Type.Condition; Type.Action]; Identity = None}

    // Conditions
    | "seqCondition" -> 
      Some {Name = "seqCondition"; RetType = Type.Condition; ArgsType = [Type.Condition; Type.Condition]; 
            Identity = Some <| ({Function=(Primitives.SolveFunction "emptyCondition").Value; Args=[]} :> ITerm) }
    | "emptyCondition" -> 
      Some {Name = "emptyCondition"; RetType = Type.Condition; ArgsType = []; Identity = None}
    | "wireCondition" -> 
      Some {Name = "wireCondition"; RetType = Type.Condition; ArgsType = [Type.Infon]; Identity = None}
    | "knownCondition" -> 
      Some {Name = "knownCondition"; RetType = Type.Condition; ArgsType = [Type.Infon]; Identity = None}

    // Actions
    | "seqAction" -> 
      Some {Name = "seqAction"; RetType = Type.Action; ArgsType = [Type.Action; Type.Action]; 
            Identity = Some <| ({Function=(Primitives.SolveFunction "emptyAction").Value; Args=[]} :> ITerm) }
    | "emptyAction" -> 
      Some {Name = "emptyAction"; RetType = Type.Action; ArgsType = []; Identity = None}
    | "send" -> 
      Some {Name = "send"; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | "say" -> 
      Some {Name = "say"; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | "learn" -> 
      Some {Name = "learn"; RetType = Type.Action; ArgsType = [Type.Infon]; Identity = None}
    | "forget" -> 
      Some {Name = "forget"; RetType = Type.Action; ArgsType = [Type.Infon]; Identity = None}
    | "install" -> 
      Some {Name = "install"; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | "uninstall" -> 
      Some {Name = "uninstall"; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    
    // Infons
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

