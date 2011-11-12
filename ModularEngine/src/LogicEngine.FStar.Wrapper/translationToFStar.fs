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

namespace Microsoft.Research.Dkal.LogicEngine.FStar.Wrapper

open System.Collections.Generic
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.LogicEngine.FStar.Deps.Builders

open Microsoft.Research.Dkal.Interfaces

module TranslationToFStar =

  type substitution = Dictionary<Types.var, Types.term>
  

  let FStarTypOfIType (t: IType) : Types.typ =
    match t with
    | Infon -> new Types.Infon () :> Types.typ 
    | Principal -> new Types.Principal () :> Types.typ 
    | SubstrateUpdate -> new Types.SubstrateUpdate () :> Types.typ 
    | SubstrateQuery -> new Types.SubstrateQuery () :> Types.typ 
    | Action -> new Types.Action () :> Types.typ 
    | Condition -> new Types.Condition () :> Types.typ 
    | Rule -> new Types.RuleT () :> Types.typ 
    | Evidence -> new Types.Evidence () :> Types.typ 
    | Substrate(t1) when t1=typeof<bool> -> new Types.Boolean () :> Types.typ 
    | Substrate(t1) when t1=typeof<int32> -> new Types.Int32 () :> Types.typ 
    | Substrate(t1) when t1=typeof<double> -> new Types.Double () :> Types.typ 
    | Substrate(t1) when t1=typeof<string> -> new Types.String () :> Types.typ 
    | Substrate(_) -> failwith "Error in FStarTypOfIType: Substrate type not supported" 
    | _ -> failwith "Error in FStarTypOfIType: IType not supported"

  let FStarVarOfIVar (v: IVar) : Types.var =
    let t = FStarTypOfIType v.Type in
      new Types.var(v.Name, t)
  
  let rec FStarFuncOfFunction (f: Function) : Types.func =
    match f.Name with
    | Primitives.SeqRule -> new Types.SeqRule () :> Types.func 
    | Primitives.EmptyRule -> new Types.EmptyRule () :> Types.func 
    | Primitives.Rule -> new Types.Rule () :> Types.func 
    | Primitives.RuleOnce -> new Types.RuleOnce () :> Types.func 
    | Primitives.SeqCondition -> new Types.SeqCondition () :> Types.func 
    | Primitives.EmptyCondition -> new Types.EmptyCondition () :> Types.func 
    | Primitives.WireCondition -> new Types.WireCondition () :> Types.func 
    | Primitives.KnownCondition -> new Types.KnownCondition () :> Types.func 
    | Primitives.SeqAction -> new Types.SeqAction () :> Types.func 
    | Primitives.EmptyAction -> new Types.EmptyAction () :> Types.func 
    | Primitives.Send -> new Types.Send () :> Types.func 
    | Primitives.JustifiedSend -> new Types.JustifiedSend () :> Types.func 
    | Primitives.JustifiedSay -> new Types.JustifiedSay () :> Types.func 
    | Primitives.Learn -> new Types.Learn () :> Types.func 
    | Primitives.Forget -> new Types.Forget () :> Types.func 
    | Primitives.Install -> new Types.Install () :> Types.func 
    | Primitives.Uninstall -> new Types.Uninstall () :> Types.func 
    | Primitives.Apply -> new Types.Apply () :> Types.func 
    | Primitives.Drop -> new Types.Drop () :> Types.func 
    | Primitives.EmptyInfon -> new Types.EmptyInfon () :> Types.func 
    | Primitives.AsInfon -> new Types.AsInfon () :> Types.func 
    | Primitives.And when f.RetType = Type.Infon -> new Types.AndInfon () :> Types.func 
    | Primitives.Implies when f.RetType = Type.Infon -> new Types.ImpliesInfon () :> Types.func 
    | Primitives.Said -> new Types.SaidInfon () :> Types.func 
    | Primitives.Justified -> new Types.JustifiedInfon () :> Types.func 
    | Primitives.EvEmpty -> new Types.EmptyEvidence () :> Types.func 
    | Primitives.EvSignature -> new Types.SignatureEvidence () :> Types.func 
    | Primitives.EvModusPonens -> new Types.ModusPonensEvidence () :> Types.func 
    | Primitives.EvAnd -> new Types.AndEvidence () :> Types.func 
    | Primitives.EvAsInfon -> new Types.AsInfonEvidence () :> Types.func 
    | _ -> Types.RelationInfon(
             new Types.relationInfon(
               PrimsListOfList(List.map FStarTypOfIType f.ArgsType),
               PrimsOptionOfOption(Option.map FStarTermOfITerm f.Identity),
               f.Name, FStarTypOfIType f.RetType)) :> Types.func 

  and FStarPolyTermOfITerm (term : ITerm) : Types.polyterm =
    let rec aux (t : ITerm) =
      match t with
      | Forall(v, t') -> let tl, body = aux t' in (v::tl, body)
      | _ -> ([], t)
    in let vars, body = aux term in
    match vars with
    | [] -> new Types.MonoTerm(FStarTermOfITerm body) :> Types.polyterm
    | _ -> new Types.ForallT(
             PrimsListOfList(List.map FStarVarOfIVar vars),
             FStarTermOfITerm body) :> Types.polyterm

  and FStarTermOfITerm (term: ITerm) : Types.term =
    match term with
    | Var(v) -> new Types.Var(FStarVarOfIVar v)  :> Types.term 
    | SubstrateConstant(o) -> new Types.Const(Types.SubstrateConstant(o))  :> Types.term 
    | PrincipalConstant(n) -> new Types.Const(Types.PrincipalConstant(n))  :> Types.term 
    | True -> new Types.Const(new Types.TrueT() :> Types.constant)  :> Types.term 
    | False -> new Types.Const(new Types.FalseT() :> Types.constant)  :> Types.term 
    | Forall(v, t) -> failwith "Forall inside expression"
    | App(f, tl) -> new Types.App(FStarFuncOfFunction f, PrimsListOfList (List.map FStarTermOfITerm tl)) :> Types.term 
    | :? ISubstrateQueryTerm as sq -> new Types.SubstrateQueryTerm(sq)  :> Types.term 
    | :? ISubstrateUpdateTerm as su -> new Types.SubstrateUpdateTerm(su)  :> Types.term 
    (*| ConcretizationEvidence(t, s) -> (* not supported any more *) 
        new Types.ConcretizationEvidence(FStarTermOfITerm t, FStarSubstitutionOfISubstitution s) :> Types.term *)
    | _ -> failwith "FStarTermOfITerm, case not supported"

  and FStarSubstitutionOfISubstitution (subst: ISubstitution) : substitution =
    (List.fold (fun res k -> 
               extend res
                 (FStarVarOfIVar k) (FStarTermOfITerm (subst.Apply k)))
               (new Dictionary<_, _>()) subst.Domain
    )


