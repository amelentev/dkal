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
  type vars = list var

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

  and term = (* ITerm *)
  (* from Ast.Tree/ActivePatterns.fs *)
  (* from Ast/ActivePatterns.fs *)
    | Var : var -> term
    | Const : constant -> term
    | SubstrateQueryTerm : ISubstrateQueryTerm -> term
    | SubstrateUpdateTerm : ISubstrateUpdateTerm -> term
    | App : func -> list term -> term

  and polyterm = 
    | MonoTerm : term -> polyterm
    | ForallT : vars -> term -> polyterm

  type infostrate = list polyterm
  type prefix = list term

  type substitution = Dictionary var term 
  val subst_apply : substitution -> var -> term
  let subst_apply s v = subst_apply_def s v (Var v)

  logic function AsTerms : vars -> list term
  assume (AsTerms [] = [])         
  assume (forall (x:var) (xs:vars). (AsTerms (x::xs)) = ((Var x)::(AsTerms xs)))
  val asTerms: xs:vars -> ts:list term{(AsTerms xs)=ts}
  let rec asTerms = function
    | [] -> []
    | hd::tl -> (Var hd)::asTerms tl

  extern reference TranslationToFStar {language="F#";
                                       dll="TranslationToFStar";
                                       namespace="";
                                       classname="TranslationToFStar"}
  extern TranslationToFStar val FStarVarOfIVar : IVar -> (*Types.*)var
  extern TranslationToFStar val FStarTermOfITerm : ITerm -> (*Types.*)term
  extern TranslationToFStar val FStarSubstitutionOfISubstitution: ISubstitution -> (*Types.*)substitution
end
