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

module Types
open TypeHeaders
  type principal = string

  type typ =  (* IType *)
  (* from Types.fs, see also Ast/ActivePatterns.fs *)
  (* BasicType types *)
    | Infon : typ
    | Principal : typ
    | SubstrateUpdate : typ (* see ISubstrateUpdate.fs *)
    | SubstrateQuery : typ (* see ISubstrateQuery.fs *)
    | Action : typ
    | Condition : typ
    | RuleT : typ
    | Evidence : typ
  (* Substrate types *)
    | Boolean : typ
    | Int32 : typ
    | Double : typ
    | String : typ

  type var = (* IVar *)
    { name : string; typ : typ }

  type constant =
    | TrueT : constant
    | FalseT : constant
    | SubstrateConstant : object -> constant
    | PrincipalConstant : principal -> constant

  type relationInfon = (* form TreeTerm.fs, def of type Function *)
    { name : string; 
      retType: typ; 
      argsType : list typ; 
      identity : option term  }

  and func =
  (* from Ast.Infon/ActivePatterns.fs *)
  (* types from Ast.Infon/Primitives.fs, function SolveFunction *)
  (* and its use in Ast.Infon/Builders.fs *)
    (* Rule: <condition> do <action> *)
    | SeqRule : func (* [RuleT;...; RuleT] -> RuleT *)
    | EmptyRule : func (* [] -> RuleT *)
    | Rule : func (* [Condition; Action] -> RuleT *)
    | RuleOnce : func (* [Condition; Action] -> RuleT *)
    (* Condition *)
    | SeqCondition : func (* [Condition;...; Condition] -> Condition *)
    | EmptyCondition : func (* [] -> Condition *)
    | WireCondition : func (* [Infon; Principal] -> Condition *)
      (* upon in concrete syntax *)
    | KnownCondition : func (* [Infon] -> Condition *)
      (* if in concrete syntax *)
    (* Action *)
    | SeqAction : func (* [Action;...; Action] -> Action *)
    | EmptyAction : func (* [] -> Action *)
    | Send : func (* [Principal; Infon] -> Action *)
      (* ppal (destination), msg *)
    | JustifiedSend : func (* [Principal; Infon] -> Action *)
      (* ppal (destination), msg *)
    | JustifiedSay : func (* [Principal; Infon] -> Action *)
      (* ppal (destination), msg *)
    | Learn : func (* [Infon] -> Action *)
    | Forget : func (* [Infon] -> Action *)
    | Install : func (* [RuleT] -> Action *)
      (* add a rule to set of rules *)
    | Uninstall : func (* [RuleT] -> Action *)
    | Apply : func (* [SubstrateUpdate] -> Action *)
      (* of substrateUpdateTerm // apply this update to the substrate *)
    | Drop : func (* [Infon] -> Action *)
      (* regarding an infon that came in as a message *)
    (* Infon *)
    | EmptyInfon : func (* [] -> Infon *)
    | AsInfon : func (* [SubstrateQuery] -> Infon *)(* of substrateQueryTerm *)
    | AndInfon : func (* [Infon; ...; Infon] -> Infon *)
      (* may be easier to consider just binary case *)
    | ImpliesInfon : func (* [Infon; Infon] -> Infon *)
    | SaidInfon : func (* [Principal; Infon] -> Infon *)
      (* ppal (sender), msg *)
    | JustifiedInfon : func (* [Infon; Evidence] -> Infon *)
    (* Evidence *)
    | EmptyEvidence : func (* [] -> Evidence *)
    | SignatureEvidence : func (* [Principal; Infon; Int32] -> Evidence *)
      (* ppal, term, int *)
      (* might want to change the third type to dsig, signature for .Net *)
    | ModusPonensEvidence : func (* [Evidence; Evidence] -> Evidence *)
    | AndEvidence : func (* [Evidence;... ; Evidence] -> Evidence *)
    | AsInfonEvidence : func (* [SubstrateQuery] -> Evidence *)
      (* of substrateQueryTerm *)
    (* Relations defined by the writer of the policy *)
    | RelationInfon (*of relationInfon*) : relationInfon -> func
      (* no active pattern for it, base case for infons *)

  and substitution = Dictionary var term 
  (* wrap the functions of dictionary used inside
     other functions and only use them there.
     use Guido's functional wrapper from Substitution *)

  and term = (* ITerm *)
  (* from Ast.Tree/ActivePatterns.fs *)
  (* from Ast/ActivePatterns.fs *)
    | Var : var -> term
    | Const : constant -> term
    | ForallT : var -> term -> term (* Rk: need parenthesis around var*term *)
    | App : func -> (list term) -> term
    | SubstrateQueryTerm : ISubstrateQueryTerm -> term
    | SubstrateUpdateTerm : ISubstrateUpdateTerm -> term
    | ConcretizationEvidence : term -> substitution -> term

  (* Rk: does not work if I put substitution after term and not before *)

  val subst_apply : substitution -> var -> term
  let subst_apply s v = subst_apply_def s v (Var v)

  val id : substitution 
  let id = emptySubst false
