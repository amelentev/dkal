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

/// Defines the public interface on how to pattern match AST elements defined
/// in the Ast.Infon module
[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  /// Active pattern for the sequencing of rules
  let (|SeqRule|_|) mt =  match mt with
                          | App({Name=Primitives.SeqRule}, rs) -> Some rs
                          | _ -> None
  /// Active pattern for the empty rule
  let (|EmptyRule|_|) mt =  match mt with
                            | App({Name=Primitives.EmptyRule}, []) -> Some ()
                            | _ -> None
  /// Active pattern for the simple rule
  let (|Rule|_|) mt = match mt with
                      | App({Name=Primitives.Rule}, [c; a]) -> Some (c, a)
                      | _ -> None
  
  /// Active pattern for the sequencing of conditions
  let (|SeqCondition|_|) mt = match mt with
                              | App({Name=Primitives.SeqCondition}, cs) -> Some cs
                              | _ -> None
  /// Active pattern for the empty condition
  let (|EmptyCondition|_|) mt = match mt with
                                | App({Name=Primitives.EmptyCondition}, []) -> Some ()
                                | _ -> None
  /// Active pattern for the upon condition
  let (|WireCondition|_|) mt =  match mt with
                                | App({Name=Primitives.WireCondition}, [i; p]) -> Some (i, p)
                                | _ -> None
  /// Active pattern for the if condition
  let (|KnownCondition|_|) mt = match mt with
                                | App({Name=Primitives.KnownCondition}, [i]) -> Some i
                                | _ -> None

  /// Active pattern for the sequencing of actions
  let (|SeqAction|_|) mt =  match mt with
                            | App({Name=Primitives.SeqAction}, actions) -> Some actions
                            | _ -> None
  /// Active pattern for the empty action
  let (|EmptyAction|_|) mt =  match mt with
                              | App({Name=Primitives.EmptyAction}, []) -> Some ()
                              | _ -> None
  /// Active pattern for the send action
  let (|Send|_|) mt = match mt with
                      | App({Name=Primitives.Send}, [ppal; i]) -> Some (ppal, i)
                      | _ -> None
  /// Active pattern for the send with justification action
  let (|JustifiedSend|_|) mt =  match mt with
                                | App({Name=Primitives.JustifiedSend}, [ppal; i]) -> Some (ppal, i)
                                | _ -> None
  /// Active pattern for the say with justification action
  let (|JustifiedSay|_|) mt = match mt with
                              | App({Name=Primitives.JustifiedSay}, [ppal; i]) -> Some (ppal, i)
                              | _ -> None
  /// Active pattern for the learn infon action
  let (|Learn|_|) mt =  match mt with
                        | App({Name=Primitives.Learn}, [i]) -> Some i
                        | _ -> None
  /// Active pattern for the forget infon action
  let (|Forget|_|) mt = match mt with
                        | App({Name=Primitives.Forget}, [i]) -> Some i
                        | _ -> None
  /// Active pattern for the install rule action
  let (|Install|_|) mt =  match mt with
                          | App({Name=Primitives.Install}, [r]) -> Some r
                          | _ -> None
  /// Active pattern for the uninstall rule action
  let (|Uninstall|_|) mt =  match mt with
                            | App({Name=Primitives.Uninstall}, [r]) -> Some r
                            | _ -> None
  /// Active pattern for the apply substrate update action
  let (|Apply|_|) mt =  match mt with
                        | App({Name=Primitives.Apply}, [t]) -> 
                          match t with
                            | :? ISubstrateUpdateTerm as su -> Some su
                            | _ -> failwith "Expecting ISubstrateUpdateTerm in AsInfon"
                        | _ -> None
  /// Active pattern for the fresh id action
  let (|Fresh|_|) mt = match mt with
                       | App({Name=Primitives.Fresh}, [v]) -> Some v
                       | _ -> None

  /// Active pattern for the empty infon
  let (|EmptyInfon|_|) mt = match mt with 
                            | App({Name=Primitives.EmptyInfon}, []) -> Some ()
                            | _ -> None
  /// Active pattern for asInfon 
  let (|AsInfon|_|) mt =  match mt with 
                          | App({Name=Primitives.AsInfon}, [exp]) -> 
                            match exp with
                            | :? ISubstrateQueryTerm as exp -> Some exp
                            | _ -> failwith "Expecting ISubstrateQueryTerm in AsInfon"
                          | _ -> None
  /// Active pattern for infon conjunction
  let (|AndInfon|_|) mt = match mt with
                          | App({Name=Primitives.And; RetType=Infon}, mts) -> Some mts
                          | _ -> None
  let (|OrInfon|_|) mt = match mt with
                          | App({Name=Primitives.Or; RetType=Infon}, mts) -> Some mts
                          | _ -> None
  /// Active pattern for infon implication
  let (|ImpliesInfon|_|) mt = match mt with
                              | App({Name=Primitives.Implies; RetType=Infon}, [mt1; mt2]) -> Some (mt1, mt2)
                              | _ -> None
  /// Active pattern for said quotation
  let (|SaidInfon|_|) mt = match mt with
                            | App({Name=Primitives.Said}, [ppal; mt']) -> Some (ppal, mt')
                            | _ -> None
  /// Active pattern for justified infons (infons with evidence)
  let (|JustifiedInfon|_|) mt = match mt with
                                | App({Name=Primitives.Justified}, [i; e]) -> Some (i, e)
                                | _ -> None

  /// Active pattern for empty evidence
  let (|EmptyEvidence|_|) mt =  match mt with
                                | App({Name=Primitives.EvEmpty}, []) -> Some ()
                                | _ -> None

  /// Active pattern for concretization of evidence (explicit substitutions)
  let (|ConcretizationEvidence|_|) (mt: ITerm) =  match mt with
                                                  | :? ExplicitSubstitutionTerm as t when (t :> ITerm).Type = Type.Evidence -> 
                                                    Some (t.Term, t.Substitution)
                                                  | _ -> 
                                                    None
  /// Active pattern for signed evidence
  let (|SignatureEvidence|_|) mt =  match mt with
                                    | App({Name=Primitives.EvSignature}, [p; i; s]) -> Some (p, i, s)
                                    | _ -> None
  /// Active pattern for modus ponens evidence
  let (|ModusPonensEvidence|_|) mt =  match mt with
                                      | App({Name=Primitives.EvModusPonens}, [e1; e2]) -> Some (e1, e2)
                                      | _ -> None
  /// Active pattern for implication introduction evidence
  let (|ImplicationIntroductionEvidence|_|) mt =  match mt with
                                                  | App({Name=Primitives.EvImplicationIntroduction}, [e1; e2]) -> Some (e1, e2)
                                                  | _ -> None
  /// Active pattern for conjunction of evidence
  let (|AndEvidence|_|) mt =  match mt with
                              | App({Name=Primitives.EvAnd}, evidences) -> Some evidences
                              | _ -> None
  /// Active pattern for conjunction elimination evidence
  let (|AndEliminationEvidence|_|) mt =  match mt with
                                          | App({Name=Primitives.EvAndElimitation}, [conj; result]) -> Some (conj, result)
                                          | _ -> None
  /// Active pattern for disjunction introduction evidence
  let (|OrIntroductionEvidence|_|) mt =  match mt with
                                          | App({Name=Primitives.EvOrIntroduction}, [disj; result]) -> Some (disj, result)
                                          | _ -> None
  /// Active pattern for asInfon evidence (basic theorems)
  let (|AsInfonEvidence|_|) mt =  match mt with
                                  | App({Name=Primitives.EvAsInfon}, [sq]) -> 
                                    match sq with
                                    | :? ISubstrateQueryTerm as sq -> Some sq
                                    | _ -> None
                                  | _ -> None
