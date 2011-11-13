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
  open Crypto
  type principal = string
  type substrate = unit
  type ISubstrateUpdateTerm

  extern reference Generics {language="F#";
                             dll="mscorlib";
                             namespace="System.Collections";
                             classname="Generic"}
  extern Generics type Dictionary :: * => * => *
  extern Generics type HashSet :: * => *

  type typ =  (* IType *)  (* from Types.fs, see also Ast/ActivePatterns.fs *)
    | Infon           : typ
    | Principal       : typ
    | SubstrateUpdate : typ (* see ISubstrateUpdate.fs *)
    | SubstrateQuery  : typ (* see ISubstrateQuery.fs *)
    | Action          : typ
    | Condition       : typ
    | RuleT           : typ
    | Evidence        : typ
    | Boolean         : typ
    | Int32           : typ
    | BytesT          : typ
    | Double          : typ
    | String          : typ

  type var = (* IVar *)
      { name : string; typ : typ }
  type vars = list var

  type constant =
    | TrueT             : constant
    | FalseT            : constant
    | Int               : int -> constant
    | Bytes             : bytes -> constant
    | SubstrateConstant : constant -> constant
    | PrincipalConstant : principal -> constant

  type relationInfon = (* from TreeTerm.fs, def of type Function *)
      { fname    : string; 
        retType  : typ; 
        argsType : list typ; 
        identity : option term }
        
  and func =
    | EmptyInfon    : func (* [] -> Infon *)
    | AsInfon       : func (* [SubstrateQuery] -> Infon *)
    | AndInfon      : func (* [Infon; ...; Infon] -> Infon *)
    | ImpliesInfon  : func (* [Infon; Infon] -> Infon *)
    | SaidInfon     : func (* [Principal; Infon] -> Infon *)
    | JustifiedInon : func (* [Principal; Infon; Bytes] -> Infon *)
    | RelationInfon : relationInfon -> func
(* --- We should get rid of these remaining ones --- *)
    | SeqRule : func 
    | SeqCondition : func 
    | SeqAction : func
    | AndInfon : func 
    | AndEvidence : func
    | EmptyRule : func 
    | Rule : func 
    | RuleOnce : func
    | EmptyCondition : func 
    | WireCondition : func 
    | KnownCondition : func
    | EmptyAction : func 
    | Send : func 
    | JustifiedSend : func
    | JustifiedSay : func 
    | Learn : func 
    | Forget : func
    | Install : func 
    | Uninstall : func
    | Apply : func 
    | Drop : func 
    | EmptyInfon : func
    | AsInfon : func 
    | ImpliesInfon : func
    | SaidInfon : func 
    | JustifiedInfon : func
    | EmptyEvidence : func 
    | SignatureEvidence : func
    | ModusPonensEvidence : func
    | AsInfonEvidence : func 

  and ISubstrateQueryTerm::* = {n:term; low: term; hi: term}

  and term = (* ITerm *)
    | Var                 : var -> term
    | Const               : constant -> term
    | App                 : func -> list term -> term
    | SubstrateQueryTerm  : ISubstrateQueryTerm -> term
    | SubstrateUpdateTerm : ISubstrateUpdateTerm -> term

  and polyterm = 
    | MonoTerm      : term -> polyterm
    | ForallT       : vars -> term -> polyterm
    | JustifiedPoly : term -> polyterm -> term -> polyterm

  type SubstrateSays :: substrate => ISubstrateQueryTerm => E
  val mkSubstrateQuery: term -> term -> term -> ISubstrateQueryTerm
  let mkSubstrateQuery i l h = {n=i; low=l; hi=h}

  val check_substrate: s:substrate
                    -> q:ISubstrateQueryTerm 
                    -> b:bool{b=true => SubstrateSays s q}
  let check_substrate s q =
    let getInt t =  (* get a const int from a term *)
      (match t with
         | Const (Int i) -> i
         | _ -> raise "unexpected term in check_substrate") in
    if intCheckRange (getInt q.n) (getInt q.low) (getInt q.hi) 
    then (assume (SubstrateSays s q); true)
    else false

  type Knows :: polyterm => E
  type kpolyterm = i:polyterm{Knows i}
  type infostrate = list polyterm
  type prefix = list term

  type substitution = list (var * term)

  logic function AsTerms : vars -> list term
  assume (AsTerms [] = [])         
  assume (forall (x:var) (xs:vars). (AsTerms (x::xs)) = ((Var x)::(AsTerms xs)))
  val asTerms: xs:vars -> ts:list term{(AsTerms xs)=ts}
  let rec asTerms = function
    | [] -> []
    | hd::tl -> (Var hd)::asTerms tl

end
