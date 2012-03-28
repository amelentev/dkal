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

module TranslationtoML =
  let MLtypOfIType (t: IType) : MLType.typ =
    match t with
    | Infon -> MLType.Infon
    | Principal -> MLType.Principal
    | SubstrateUpdate -> MLType.SubstrateUpdate
    | SubstrateQuery -> MLType.SubstrateQuery
    | Action -> MLType.Action
    | Condition -> MLType.Condition
    | Rule -> MLType.RuleT
    | Evidence -> MLType.Evidence
    | Substrate(t1) when t1=typeof<bool> -> MLType.Boolean
    | Substrate(t1) when t1=typeof<int32> -> MLType.Int32
    | Substrate(t1) when t1=typeof<double> -> MLType.Double
    | Substrate(t1) when t1=typeof<string> -> MLType.String
    | Substrate(_) -> failwith "Error in MLtypOfIType: Substrate type not supported" 
    | _ -> failwith "Error in MLtypOfIType: IType not supported"

  let MLvarOfIVar (v: IVar) : MLType.var =
    let t = MLtypOfIType v.Type
    { typ = t ; name = v.Name }
  
  let rec MLfuncOfFunction (f: Function) : MLType.func =
    match f.Name with
    | Primitives.SeqRule -> MLType.SeqRule
    | Primitives.EmptyRule -> MLType.EmptyRule
    | Primitives.Rule -> MLType.Rule
    | Primitives.RuleOnce -> MLType.RuleOnce
    | Primitives.SeqCondition -> MLType.SeqCondition
    | Primitives.EmptyCondition -> MLType.EmptyCondition
    | Primitives.WireCondition -> MLType.WireCondition
    | Primitives.KnownCondition -> MLType.KnownCondition
    | Primitives.SeqAction -> MLType.SeqAction
    | Primitives.EmptyAction -> MLType.EmptyAction
    | Primitives.Send -> MLType.Send
    | Primitives.JustifiedSend -> MLType.JustifiedSend
    | Primitives.JustifiedSay -> MLType.JustifiedSay
    | Primitives.Learn -> MLType.Learn
    | Primitives.Forget -> MLType.Forget
    | Primitives.Install -> MLType.Install
    | Primitives.Uninstall -> MLType.Uninstall
    | Primitives.Apply -> MLType.Apply
    | Primitives.EmptyInfon -> MLType.EmptyInfon
    | Primitives.AsInfon -> MLType.AsInfon
    | Primitives.And when f.RetType = Type.Infon -> MLType.AndInfon
    | Primitives.Implies when f.RetType = Type.Infon -> MLType.ImpliesInfon
    | Primitives.Said -> MLType.SaidInfon
    | Primitives.Justified -> MLType.JustifiedInfon
    | Primitives.EvEmpty -> MLType.EmptyEvidence
    | Primitives.EvSignature -> MLType.SignatureEvidence
    | Primitives.EvModusPonens -> MLType.ModusPonensEvidence
    | Primitives.EvAnd -> MLType.AndEvidence
    | Primitives.EvAsInfon -> MLType.AsInfonEvidence
    | _ -> MLType.RelationInfon { name = f.Name ; retType = MLtypOfIType f.RetType ;
                                  argsType = List.map MLtypOfIType f.ArgsType ;
                                  identity = Option.map MLtermOfITerm f.Identity }

  and MLtermOfITerm (term: ITerm) : MLType.term =
    match term with
    | Var(v) -> MLType.Var(MLvarOfIVar v)
    | SubstrateConstant(o) -> MLType.Const(MLType.SubstrateConstant(o))
    | PrincipalConstant(n) -> MLType.Const(MLType.PrincipalConstant(n))
    | True -> MLType.Const(MLType.True)
    | False -> MLType.Const(MLType.False)
    | Forall(v, t) -> MLType.Forall(MLvarOfIVar v, MLtermOfITerm t)
    | App(f, tl) -> MLType.App(MLfuncOfFunction f, List.map MLtermOfITerm tl)
    | :? ISubstrateQueryTerm as sq -> MLType.SubstrateQueryTerm(sq)
    | :? ISubstrateUpdateTerm as su -> MLType.SubstrateUpdateTerm(su) 
    | ConcretizationEvidence(t, s) -> 
        MLType.ConcretizationEvidence(MLtermOfITerm t, MLsubstitutionOfISubstitution s)
    | _ -> failwith "MLtermOfITerm, case not supported"

  and MLsubstitutionOfISubstitution (subst: ISubstitution) : MLType.substitution =
    List.fold (fun res k -> 
               Subst.extend res
                 (MLvarOfIVar k, MLtermOfITerm (subst.Apply k)))
              Subst.id subst.Domain

