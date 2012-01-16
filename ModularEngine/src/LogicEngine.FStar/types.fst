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
  open Util
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
    | AsInfon       : func (* [SubstrateQuery] -> Infon *)
    | EmptyInfon    : func (* [] -> Infon *)
    | AndInfon      : func (* [Infon; Infon] -> Infon *)
    | ImpliesInfon  : func (* [Infon; Infon] -> Infon *)
    | SaidInfon     : func (* [Principal; Infon] -> Infon *)
    | JustifiedInfon : func (* [Principal; Infon; Bytes] -> Infon *)
    | RelationInfon : relationInfon -> func
(* --- We should get rid of these remaining ones --- *)
    | SeqRule : func 
    | SeqCondition : func 
    | SeqAction : func
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

  type Says :: principal => polyterm => E
  type CheckedInfon :: polyterm => E
  type CheckedInfonMono :: term => E
  assume I_Mono: forall (i:term). 
                 CheckedInfonMono i 
              => CheckedInfon (MonoTerm i)
  assume I_All : forall (xs:vars) (i:term).
                 CheckedInfonMono i 
              => CheckedInfon (ForallT xs i)
  assume I_Just: forall (p:principal) (i:polyterm) (e:term).
                 Says p i
              && CheckedInfon i 
              => CheckedInfon (JustifiedPoly (Const (PrincipalConstant p)) i e)
  assume IM_Var : forall (x:var). CheckedInfonMono (Var x)
  assume IM_Con : forall (c:constant). CheckedInfonMono (Const c)
  assume IM_Just: forall (p:principal) (i:term) (e:term). 
                  Says p (MonoTerm i)
               && CheckedInfonMono i
               => CheckedInfonMono (App JustifiedInfon [(Const (PrincipalConstant p));i;e])
  assume IM_App : forall (f:func) (tms:list term).
                  (f<>JustifiedInfon)
               && (forall (tm:term). In tm tms => CheckedInfonMono tm)
               => CheckedInfonMono (App f tms)
  assume IM_SQ  : forall (q:ISubstrateQueryTerm).
                  CheckedInfonMono (SubstrateQueryTerm q)

  type monoinfon = i:term{CheckedInfonMono i}
  type infon = i:polyterm{CheckedInfon i}
  type Knows :: polyterm => E
  type kinfon = i:infon{Knows i}
  type infostrate = list infon
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
