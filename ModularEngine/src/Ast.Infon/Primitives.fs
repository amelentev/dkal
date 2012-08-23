// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

/// Defines the primitive functions used to construct Application elements
/// in the Ast module builders
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
  let JustifiedSay = "justifiedSay"
  [<Literal>] 
  let JustifiedSend = "justifiedSend"
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
  [<Literal>] 
  let Fresh = "fresh"

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
  [<Literal>]
  let Or = "or"
  [<Literal>] 
  let Justified = "justified"

  // Evidence
  [<Literal>] 
  let EvEmpty = "evEmpty"
  [<Literal>] 
  let EvSignature = "evSignature"
  [<Literal>] 
  let EvModusPonens = "evModusPonens"
  [<Literal>] 
  let EvImplicationIntroduction = "->i"  
  [<Literal>] 
  let EvAnd = "evAnd"
  [<Literal>] 
  let EvAndElimitation = "&e"
  [<Literal>] 
  let EvOrIntroduction = "|i"
  [<Literal>] 
  let EvAsInfon = "evAsInfon"

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
      Some {Name = f; RetType = Type.Condition; ArgsType = [Type.AbstractInfon; Type.Principal]; Identity = None}
    | KnownCondition -> 
      Some {Name = f; RetType = Type.Condition; ArgsType = [Type.AbstractInfon]; Identity = None}

    // Actions
    | SeqAction -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Action; Type.Action]; 
            Identity = Some <| ({Function=(SolveFunction EmptyAction).Value; Args=[]} :> ITerm) }
    | EmptyAction -> 
      Some {Name = f; RetType = Type.Action; ArgsType = []; Identity = None}
    | Send -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Principal; Type.AbstractInfon]; Identity = None}
    | JustifiedSend -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | JustifiedSay -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Principal; Type.Infon]; Identity = None}
    | Learn -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.AbstractInfon]; Identity = None}
    | Forget -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.AbstractInfon]; Identity = None}
    | Install -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | Uninstall -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Rule]; Identity = None}
    | Apply -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.SubstrateUpdate]; Identity = None}
    | Fresh -> 
      Some {Name = f; RetType = Type.Action; ArgsType = [Type.Int32]; Identity = None}
    
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
    | Or ->
      Some {Name = f; RetType = Type.Infon; ArgsType = [Type.Infon; Type.Infon]; Identity = None }
    | Justified ->
      Some {Name = f; RetType = Type.JustifiedInfon; ArgsType = [Type.Infon; Type.Evidence]; Identity = None}

    // Evidence
    | EvEmpty ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = []; Identity = None}
    | EvSignature ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.Principal; Type.Infon; Type.Int32]; Identity = None}
    | EvModusPonens ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.Evidence; Type.Evidence]; Identity = None}
    | EvImplicationIntroduction ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.Evidence; Type.Infon]; Identity = None}
    | EvAnd ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.Evidence; Type.Evidence]; 
            Identity = Some <| ({Function=(SolveFunction EvEmpty).Value; Args=[]} :> ITerm) }
    | EvAndElimitation ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.Evidence; Type.Infon]; Identity = None}
    | EvOrIntroduction ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.Evidence; Type.Infon]; Identity = None}
    | EvAsInfon ->
      Some {Name = f; RetType = Type.Evidence; ArgsType = [Type.SubstrateQuery]; Identity = None}

    // Otherwise, not found
    | _ ->
      None