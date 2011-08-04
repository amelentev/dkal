#light
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

module TranslationFromFStar
open TypeHeaders
open Types

  let ITypeOfFStarTyp (t: Types.typ) : IType =
    match t with
    | Types.Infon -> TypeHeaders.Infon false
    | Types.Principal -> TypeHeaders.Principal false
    | Types.SubstrateUpdate -> TypeHeaders.SubstrateUpdate false
    | Types.SubstrateQuery -> TypeHeaders.SubstrateQuery false
    | Types.Action -> TypeHeaders.Action false
    | Types.Condition -> TypeHeaders.Condition false
    | Types.RuleT -> TypeHeaders.Rule false
    | Types.Evidence -> TypeHeaders.Evidence false
    | Types.Boolean ->TypeHeaders.Boolean false
    | Types.Int32 -> TypeHeaders.Int32 false
    | Types.Double -> TypeHeaders.Double false
    | Types.String -> TypeHeaders.String false

  val IVarOfFStarVar : Types.var -> IVar
  let IVarOfFStarVar (v: Types.var) : IVar =
    mkIVar (v.name : string) (ITypeOfFStarTyp (v.typ : Types.typ)) (* Rk: type annotations needed *)

  val FunctionOfFStarFunc : Types.func -> Function
  val ITermOfFStarTerm : Types.term -> ITerm
  val ISubstitutionOfFStarSubstitution : Types.substitution -> ISubstitution
  let rec FunctionOfFStarFunc (f: Types.func) : Function = 
    match f with
    | Types.SeqRule -> option_get(SolveFunctionW (TypeHeaders.SeqRuleStr false))
    | Types.EmptyRule -> option_get(SolveFunctionW (TypeHeaders.EmptyRule false))
    | Types.Rule -> option_get(SolveFunctionW(TypeHeaders.RuleF false))
    | Types.RuleOnce -> option_get(SolveFunctionW(TypeHeaders.RuleOnce false))
    | Types.SeqCondition -> option_get(SolveFunctionW(TypeHeaders.SeqConditionStr false))
    | Types.EmptyCondition -> option_get(SolveFunctionW(TypeHeaders.EmptyCondition false))
    | Types.WireCondition -> option_get(SolveFunctionW(TypeHeaders.WireCondition false))
    | Types.KnownCondition -> option_get(SolveFunctionW(TypeHeaders.KnownCondition false))
    | Types.SeqAction -> option_get(SolveFunctionW(TypeHeaders.SeqActionStr false))
    | Types.EmptyAction -> option_get(SolveFunctionW(TypeHeaders.EmptyAction false))
    | Types.Send -> option_get(SolveFunctionW(TypeHeaders.Send false))
    | Types.JustifiedSend -> option_get(SolveFunctionW(TypeHeaders.JustifiedSend false))
    | Types.JustifiedSay -> option_get(SolveFunctionW(TypeHeaders.JustifiedSay false))
    | Types.Learn -> option_get(SolveFunctionW(TypeHeaders.Learn false))
    | Types.Forget -> option_get(SolveFunctionW(TypeHeaders.Forget false))
    | Types.Install -> option_get(SolveFunctionW(TypeHeaders.Install false))
    | Types.Uninstall -> option_get(SolveFunctionW(TypeHeaders.Uninstall false))
    | Types.Apply -> option_get(SolveFunctionW(TypeHeaders.Apply false))
    | Types.Drop -> option_get(SolveFunctionW(TypeHeaders.Drop false))
    | Types.EmptyInfon -> option_get(SolveFunctionW(TypeHeaders.EmptyInfon false))
    | Types.AsInfon -> option_get(SolveFunctionW(TypeHeaders.AsInfon false))
    | Types.AndInfon -> option_get(SolveFunctionW(TypeHeaders.And false))
    | Types.ImpliesInfon -> option_get(SolveFunctionW(TypeHeaders.Implies false))
    | Types.SaidInfon -> option_get(SolveFunctionW(TypeHeaders.Said false))
    | Types.JustifiedInfon -> option_get(SolveFunctionW(TypeHeaders.Justified false))
    | Types.EmptyEvidence -> option_get(SolveFunctionW(TypeHeaders.EvEmpty false))
    | Types.SignatureEvidence -> option_get(SolveFunctionW(TypeHeaders.EvSignature false))
    | Types.ModusPonensEvidence -> option_get(SolveFunctionW(TypeHeaders.EvModusPonens false))
    | Types.AndEvidence -> option_get(SolveFunctionW(TypeHeaders.EvAnd false))
    | Types.AsInfonEvidence -> option_get(SolveFunctionW(TypeHeaders.EvAsInfon false))
    | Types.RelationInfon f -> mkFunction f.name (ITypeOfFStarTyp f.retType) 
		                             (map ITypeOfFStarTyp f.argsType)
																 (option_map ITermOfFStarTerm f.identity)

  and ITermOfFStarTerm (t: Types.term) : ITerm =
    match t with
    | Types.Var(v) -> ITermOfIVar(IVarOfFStarVar v) 
    | Types.Const(Types.SubstrateConstant(o)) -> TypeHeaders.Constant_ITerm(o) 
    | Types.Const(Types.PrincipalConstant(n)) -> TypeHeaders.PrincipalConstant_ITerm(n) 
    | Types.Const(Types.TrueT) -> Constant_ITerm_bool(true) 
    | Types.Const(Types.FalseT) -> Constant_ITerm_bool(false) 
    | Types.ForallT v t -> TypeHeaders.ForallT(IVarOfFStarVar v, ITermOfFStarTerm t) (* Rk: double parenthesis needed: error msg "Too many pattern variables" confusing *)
    (* 5 special cases for the the functions where the number of arguments
       can vary. Special cases here because they are special cases in Builders.fs *)
    | Types.App Types.SeqRule tl ->
      TypeHeaders.SeqRuleW(map ITermOfFStarTerm tl)
    | Types.App Types.SeqCondition tl ->
      TypeHeaders.SeqConditionW(map ITermOfFStarTerm tl)
    | Types.App Types.SeqAction tl ->
      TypeHeaders.SeqActionW(map ITermOfFStarTerm tl)
    | Types.App Types.AndInfon tl ->
      TypeHeaders.AndInfonW(map ITermOfFStarTerm tl)
    | Types.App Types.AndEvidence tl ->
      TypeHeaders.AndEvidenceW(map ITermOfFStarTerm tl)
    (* general case for App: all the other cases are the same in Builders.fs *)
    | Types.App f tl -> 
      Application_ITerm
        (mkApplication (FunctionOfFStarFunc f) (map ITermOfFStarTerm tl))  
    | Types.SubstrateQueryTerm(t0) -> (substrateQueryTerm_ITerm (t0:ISubstrateQueryTerm) : ITerm) 
    | Types.SubstrateUpdateTerm(t1) -> (substrateUpdateTerm_ITerm (t1:ISubstrateUpdateTerm) : ITerm) 
    | Types.ConcretizationEvidence t0 s -> explicitSubstitutionTerm_ITerm
       (ITermOfFStarTerm t0) (ISubstitutionOfFStarSubstitution s)
       
  and ISubstitutionOfFStarSubstitution (subst: Types.substitution) : ISubstitution =
    fold_left (fun res k ->SubstExtend res (IVarOfFStarVar k, 
		                                        ITermOfFStarTerm (subst_apply subst k)))
              Id (domain subst) 
