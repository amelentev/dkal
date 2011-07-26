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
    | SubstrateUpdate : typ
    | SubstrateQuery : typ
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
    { typ : typ; name : string }

  type constant =
    | True : constant
    | False : constant
    | SubstrateConstant : object -> constant
    | PrincipalConstant : principal -> constant

  type relationInfon = (* form TreeTerm.fs, def of type Function *)
    { name : string; retType: typ; 
      argsType : list typ; 
      identity : option term  }

  and func =
  (* from Ast.Infon/ActivePatterns.fs *)
    (* Rule: <condition> do <action> *)
    | SeqRule : func
    | EmptyRule : func
    | Rule : func
    | RuleOnce : func
    (* Condition *)
    | SeqCondition : func
    | EmptyCondition : func
    | WireCondition : func (* upon in concrete syntax *)
    | KnownCondition : func (* if in concrete syntax *)
    (* Action *)
    | SeqAction : func
    | EmptyAction : func
    | Send : func (* ppal (destination), msg *)
    | JustifiedSend : func (* ppal (destination), msg *)
    | JustifiedSay : func (* ppal (destination), msg *)
    | Learn : func
    | Forget : func
    | Install : func (* add a rule to set of rules *)
    | Uninstall : func (* regarding a rule *)
    | Apply : func (* of substrateUpdateTerm // apply this update to the substrate *)
    | Drop : func (* regarding an infon that came in as a message *)
    (* Infon *)
    | EmptyInfon : func
    | AsInfon : func (* of substrateQueryTerm *)
    | AndInfon : func (* may be easier to consider just binary case *)
    | ImpliesInfon : func (* infon, infon *)
    | SaidInfon : func (* ppal (sender), msg *)
    | JustifiedInfon : func (* infon, evidence *)
    (* Evidence *)
    | EmptyEvidence : func
    | SignatureEvidence : func (* ppal, term, int *)
      (* might want to change the third type to dsig, signature for .Net *)
    | ModusPonensEvidence : func
    | AndEvidence : func
    | AsInfonEvidence : func (* of substrateQueryTerm *)
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
    | Forall : (var * term) -> term (* Rk: need parenthesis around var*term *)
    | App : (func * (list term)) -> term
    | ConcretizationEvidence : (term * substitution) -> term
    | SubstrateQueryTerm : ISubstrateQueryTerm -> term
    | SubstrateUpdateTerm : ISubstrateUpdateTerm -> term

  (* Rk: does not work if I put substitution after term and not before *)

  val subst_apply : substitution -> var -> term
  let subst_apply s v = subst_apply_def s v (Var v)

  val id : substitution 
  let id = emptySubst false
