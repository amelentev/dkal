(*
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
*)

namespace Microsoft.Research.Dkal.LogicEngine.ML

open System.Collections.Generic
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

module TranslationfromML =
  let ITypeOfMLtyp (t: MLType.typ) : IType =
    match t with
    | MLType.Infon -> Type.Infon
    | MLType.Principal -> Type.Principal
    | MLType.SubstrateUpdate -> Type.SubstrateUpdate
    | MLType.SubstrateQuery -> Type.SubstrateQuery
    | MLType.Action -> Type.Action
    | MLType.Condition -> Type.Condition
    | MLType.RuleT -> Type.Rule
    | MLType.Evidence -> Type.Evidence
    | MLType.Boolean -> Type.Boolean
    | MLType.Int32 -> Type.Int32
    | MLType.Double -> Type.Double
    | MLType.String -> Type.String

  let IVarOfMLvar (v: MLType.var) : Variable =
    { Name = v.name ; Type = ITypeOfMLtyp v.typ }

  let rec FunctionOfMLfunc (f: MLType.func) : Function = 
    match f with
    | MLType.SeqRule -> 
      Primitives.SeqRule |> Primitives.SolveFunction |> Option.get
    | MLType.EmptyRule -> 
      Primitives.EmptyRule |> Primitives.SolveFunction |> Option.get
    | MLType.Rule ->  
      Primitives.Rule |> Primitives.SolveFunction |> Option.get
    | MLType.RuleOnce ->  
      Primitives.RuleOnce |> Primitives.SolveFunction |> Option.get
    | MLType.SeqCondition ->  
      Primitives.SeqCondition |> Primitives.SolveFunction |> Option.get
    | MLType.EmptyCondition ->  
      Primitives.EmptyCondition |> Primitives.SolveFunction |> Option.get
    | MLType.WireCondition ->  
      Primitives.WireCondition |> Primitives.SolveFunction |> Option.get
    | MLType.KnownCondition ->  
      Primitives.KnownCondition |> Primitives.SolveFunction |> Option.get
    | MLType.SeqAction ->  
      Primitives.SeqAction |> Primitives.SolveFunction |> Option.get
    | MLType.EmptyAction ->  
      Primitives.EmptyAction |> Primitives.SolveFunction |> Option.get
    | MLType.Send ->  
      Primitives.Send |> Primitives.SolveFunction |> Option.get
    | MLType.JustifiedSend ->  
      Primitives.JustifiedSend |> Primitives.SolveFunction |> Option.get
    | MLType.JustifiedSay ->  
      Primitives.JustifiedSay |> Primitives.SolveFunction |> Option.get
    | MLType.Learn -> 
      Primitives.Learn |> Primitives.SolveFunction |> Option.get
    | MLType.Forget -> 
      Primitives.Forget |> Primitives.SolveFunction |> Option.get
    | MLType.Install -> 
      Primitives.Install |> Primitives.SolveFunction |> Option.get
    | MLType.Uninstall -> 
      Primitives.Uninstall |> Primitives.SolveFunction |> Option.get
    | MLType.Apply -> 
      Primitives.Apply |> Primitives.SolveFunction |> Option.get
    | MLType.Drop -> 
      Primitives.Drop |> Primitives.SolveFunction |> Option.get
    | MLType.EmptyInfon -> 
      Primitives.EmptyInfon |> Primitives.SolveFunction |> Option.get
    | MLType.AsInfon -> 
      Primitives.AsInfon |> Primitives.SolveFunction |> Option.get
    | MLType.AndInfon -> 
      Primitives.And |> Primitives.SolveFunction |> Option.get
    | MLType.ImpliesInfon -> 
      Primitives.Implies |> Primitives.SolveFunction |> Option.get
    | MLType.SaidInfon -> 
      Primitives.Said |> Primitives.SolveFunction |> Option.get
    | MLType.JustifiedInfon -> 
      Primitives.Justified |> Primitives.SolveFunction |> Option.get
    | MLType.EmptyEvidence -> 
      Primitives.EvEmpty |> Primitives.SolveFunction |> Option.get
    | MLType.SignatureEvidence -> 
      Primitives.EvSignature |> Primitives.SolveFunction |> Option.get
    | MLType.ModusPonensEvidence -> 
      Primitives.EvModusPonens |> Primitives.SolveFunction |> Option.get
    | MLType.AndEvidence -> 
      Primitives.EvAnd |> Primitives.SolveFunction |> Option.get
    | MLType.AsInfonEvidence -> 
      Primitives.EvAsInfon |> Primitives.SolveFunction |> Option.get
    | MLType.RelationInfon f ->
      { Name = f.name ; RetType = ITypeOfMLtyp f.retType ;
        ArgsType = List.map ITypeOfMLtyp f.argsType ;
        Identity = Option.map ITermOfMLterm f.identity}

  and ITermOfMLterm (t: MLType.term) : ITerm =
    match t with
    | MLType.Var(v) -> Var(IVarOfMLvar v)
    | MLType.Const(MLType.SubstrateConstant(o)) -> Constant(o) :> ITerm
    | MLType.Const(MLType.PrincipalConstant(n)) -> PrincipalConstant(n) :> ITerm
    | MLType.Const(MLType.True) -> Constant(true) :> ITerm
    | MLType.Const(MLType.False) -> Constant(false) :> ITerm
    | MLType.Forall(v, t) -> {Var = IVarOfMLvar v; 
                          Term = ITermOfMLterm t} :> ITerm
    // 5 special cases for the functions where the number of arguments
    // can vary. Special cases here because they are special cases in Builders.fs
    | MLType.App(MLType.SeqRule, tl) ->
      Builders.SeqRule(List.map ITermOfMLterm tl)
    | MLType.App(MLType.SeqCondition, tl) ->
      Builders.SeqCondition(List.map ITermOfMLterm tl)
    | MLType.App(MLType.SeqAction, tl) ->
      Builders.SeqAction(List.map ITermOfMLterm tl)
    | MLType.App(MLType.AndInfon, tl) ->
      Builders.AndInfon(List.map ITermOfMLterm tl)
    | MLType.App(MLType.AndEvidence, tl) ->
      Builders.AndEvidence(List.map ITermOfMLterm tl)
    // general case for App: all the other cases are the same in Builders.fs
    | MLType.App(f, tl) -> 
      { Function = FunctionOfMLfunc f ;
        Args = List.map ITermOfMLterm tl } :> ITerm
    | MLType.SubstrateQueryTerm(t0) -> t0 :> ITerm
    | MLType.SubstrateUpdateTerm(t0) -> t0 :> ITerm
    | MLType.ConcretizationEvidence(t0, s) -> 
       ExplicitSubstitutionTerm(ITermOfMLterm t0, ISubstitutionOfMLsubstitution s) :> ITerm

  and ISubstitutionOfMLsubstitution (subst: MLType.substitution) : ISubstitution =
    List.fold (fun res k ->
               res.Extend 
                 (IVarOfMLvar k, ITermOfMLterm (Subst.subst_apply subst k)))
              Substitution.Id (Subst.domain subst) 