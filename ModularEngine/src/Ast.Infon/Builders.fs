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

/// Defines the public interface on how to construct AST elements defined
/// in the Ast.Infon module
[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  /// Build a sequencing of rules
  let SeqRule (rs: ITerm list) = 
    App({ Name = Primitives.SeqRule; 
          RetType = Type.Rule; 
          ArgsType = List.replicate rs.Length Type.Rule;
          Identity = (Primitives.SolveFunction Primitives.SeqRule).Value.Identity }, rs)
  /// Build an empty rule
  let EmptyRule = 
    App(Primitives.SolveFunction Primitives.EmptyRule |> Option.get, [])
  /// Build a simple rule
  let RuleRule (c: ITerm, a: ITerm) = 
    App(Primitives.SolveFunction Primitives.Rule |> Option.get, [c; a])
  /// Build a "do once" rule
  let RuleOnceRule (c: ITerm, a: ITerm) = 
    App(Primitives.SolveFunction Primitives.RuleOnce |> Option.get, [c; a])

  /// Build a sequencing of conditions
  let SeqCondition (conds: ITerm list) = 
    App({ Name = Primitives.SeqCondition; 
          RetType = Type.Condition; 
          ArgsType = List.replicate conds.Length Type.Condition;
          Identity = (Primitives.SolveFunction Primitives.SeqCondition).Value.Identity }, conds)
  /// Build an empty condition
  let EmptyCondition = 
    App(Primitives.SolveFunction Primitives.EmptyCondition |> Option.get, [])
  /// Build an upon condition
  let WireCondition (i: ITerm, p: ITerm) = 
    App(Primitives.SolveFunction Primitives.WireCondition |> Option.get, [i; p])
  /// Build an if condition
  let KnownCondition (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.KnownCondition |> Option.get, [i])

  /// Build a sequencing of actions
  let SeqAction (actions: ITerm list) = 
    App({ Name = Primitives.SeqAction; 
          RetType = Type.Action; 
          ArgsType = List.replicate actions.Length Type.Action;
          Identity = (Primitives.SolveFunction Primitives.SeqAction).Value.Identity }, actions)
  /// Build an empty action
  let EmptyAction = 
    App(Primitives.SolveFunction Primitives.EmptyAction |> Option.get, [])
  /// Build a send action
  let SendAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Send |> Option.get, [ppal; i])
  /// Build a send with justification action
  let JustifiedSendAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.JustifiedSend |> Option.get, [ppal; i])
  /// Build a say with justification action
  let JustifiedSayAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.JustifiedSay |> Option.get, [ppal; i])
  /// Build a learn infon action
  let LearnAction (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Learn |> Option.get, [i])
  /// Build a forget infon action
  let ForgetAction (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Forget |> Option.get, [i])
  /// Build an install rule action
  let InstallAction (r: ITerm) = 
    App(Primitives.SolveFunction Primitives.Install |> Option.get, [r])
  /// Build an uninstall rule action
  let UninstallAction (r: ITerm) = 
    App(Primitives.SolveFunction Primitives.Uninstall |> Option.get, [r])
  /// Build an apply substrate update action
  let ApplyAction (su: ITerm) = 
    App(Primitives.SolveFunction Primitives.Apply |> Option.get, [su])
  /// Build a fresh id action
  let FreshAction (v: ITerm) = 
    App(Primitives.SolveFunction Primitives.Fresh |> Option.get, [v])

  /// Build an empty infon
  let EmptyInfon = 
    App(Primitives.SolveFunction Primitives.EmptyInfon |> Option.get, [])
  /// Build an asInfon
  let AsInfon (query: ISubstrateQueryTerm) = 
    App(Primitives.SolveFunction Primitives.AsInfon |> Option.get, [query])
  /// Build an infon conjunction
  let AndInfon (infons: ITerm list) = 
    App({ Name = Primitives.And; 
          RetType = Type.Infon; 
          ArgsType = List.replicate infons.Length Type.Infon;
          Identity = (Primitives.SolveFunction Primitives.And).Value.Identity }, infons)
  /// Build an infon implication
  let ImpliesInfon (i1: ITerm, i2: ITerm) = 
    App(Primitives.SolveFunction Primitives.Implies |> Option.get, [i1; i2])
  /// Build a said quotation infon
  let SaidInfon (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Said |> Option.get, [ppal; i])
  /// Build a justified infon (infon with evidence)
  let JustifiedInfon (i: ITerm, e: ITerm) =
    App(Primitives.SolveFunction Primitives.Justified |> Option.get, [i; e])
  /// Build a quotation infon with several principals (e.g., p1 said p2 said 
  /// p3 said [...] i)
  let PrefixedInfon (ppals: ITerm list, i: ITerm) =
    List.foldBack (fun ppal i -> SaidInfon(ppal, i)) ppals i

  /// Build empty evidence
  let EmptyEvidence =
    App(Primitives.SolveFunction Primitives.EvEmpty |> Option.get, [])
  /// Build a signature evidence and wrap it with a ExplicitSubstitutionTerm 
  /// to prevent it from being substituted
  let SignatureEvidence (p: ITerm, i: ITerm, s: ITerm) =
    ExplicitSubstitutionTerm(App(Primitives.SolveFunction Primitives.EvSignature |> Option.get, [p; i; s])) :> ITerm
  /// Build a modus ponens evidence
  let ModusPonensEvidence (e1: ITerm, e2: ITerm) =
    App(Primitives.SolveFunction Primitives.EvModusPonens |> Option.get, [e1; e2])
  /// Build conjunction of evidence
  let AndEvidence (evidences: ITerm list) =
    App({ Name = Primitives.EvAnd; 
          RetType = Type.Evidence; 
          ArgsType = List.replicate evidences.Length Type.Evidence;
          Identity = (Primitives.SolveFunction Primitives.EvAnd).Value.Identity }, evidences)
  /// Build asInfon evidence (for basic theorems)
  let AsInfonEvidence (sq: ISubstrateQueryTerm) =
    App(Primitives.SolveFunction Primitives.EvAsInfon |> Option.get, [sq])
    
