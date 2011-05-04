namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

module Primitives =

  // Rules
  [<Literal>] 
  let SeqRule = "seqRule"
  [<Literal>] 
  let EmptyRule = "emptyRule"
  [<Literal>] 
  let Rule = "rule"

  // Conditions
  [<Literal>] 
  let SeqCondition = "seqCondition"
  [<Literal>] 
  let EmptyCondition = "emptyCondition"
  [<Literal>] 
  let WireCondition = "wireCondition"
  [<Literal>] 
  let KnownCondition = "knownCondition"

  // Actions
  [<Literal>] 
  let SeqAction = "seqAction"
  [<Literal>] 
  let EmptyAction = "emptyAction"
  [<Literal>] 
  let Send = "send"
  [<Literal>] 
  let Say = "say"
  [<Literal>] 
  let Learn = "learn"
  [<Literal>] 
  let Forget = "forget"
  [<Literal>] 
  let Install = "install"
  [<Literal>] 
  let Uninstall = "uninstall"
  [<Literal>] 
  let Apply = "apply"

  // Infons
  [<Literal>] 
  let EmptyInfon = "emptyInfon"
  [<Literal>] 
  let AsInfon = "asInfon"
  [<Literal>] 
  let Implies = "implies"
  [<Literal>] 
  let Said = "said"
  [<Literal>] 
  let And = "and"

  /// Given a primitive function name it returns a Fuction, if anyone matches;
  /// None otherwise
  let rec SolveFunction (f: string) =
    match f with
    // Rules
    | SeqRule -> 
      Some {Name = f; RetType = Type.Rule; ArgsType = [Type.Rule; Type.Rule]; 
            Identity = Some <| ({Function=(SolveFunction EmptyRule).Value; Args=[]} :> ITerm) }
    | EmptyRule -> 
      Some {Name = f; RetType = Type.Rule; ArgsType = []; Identity = None}
    | Rule -> 
      Some {Name = f; RetType = Type.Rule; ArgsType = [Type.Condition; Type.Action]; Identity = None}

    // Conditions
    | SeqCondition -> 
      Some {Name = f; RetType = Type.Condition; ArgsType = [Type.Condition; Type.Condition]; 
            Identity = Some <| ({Function=(SolveFunction EmptyCondition).Value; Args=[]} :> ITerm) }
    | EmptyCondition -> 
      Some {Name = f; RetType = Type.Condition; ArgsType = []; Identity = None}
    | WireCondition -> 
      Some {Name = f; RetType = Type.Condition; ArgsType = [Type.Infon]; Identity = None}
    | KnownCondition -> 
      Some {Name = f; RetType = Type.Condition; ArgsType = [Type.Infon]; Identity = None}

    // Actions
    | SeqAction -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Action; Type.Action]; 
            Identity = Some <| ({Function=(SolveFunction EmptyAction).Value; Args=[]} :> ITerm) }
    | EmptyAction -> 
      Some {Name = f; RetType = Type.Action; ArgsType = []; Identity = None}
    | Send -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | Say -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | Learn -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Infon]; Identity = None}
    | Forget -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Infon]; Identity = None}
    | Install -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | Uninstall -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | Apply -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.SubstrateUpdate]; Identity = None}
    
    // Infons
    | EmptyInfon ->
      Some {Name = f; RetType = Type.Infon; ArgsType = []; Identity = None}
    | AsInfon ->
      Some {Name = f; RetType = Type.Infon; ArgsType = [Type.SubstrateQuery]; Identity = None}
    | Implies ->
      Some {Name = f; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]; Identity = None}
    | Said ->
      Some {Name = f; RetType = Type.Infon; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | And ->
      Some {Name = f; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]; 
            Identity = Some <| ({Function=(SolveFunction EmptyInfon).Value; Args=[]} :> ITerm) }
    | _ -> 
      None

