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

(************************************************)
(* build:                                       *)
(* fstar typeHeaders.fst types.fst coredkal.fst *)
(* or:                                          *)
(* make coredkal                                *)
(************************************************)

module CoreDKAL

(********************)
(* Type definitions *)
(********************)

open Types

(** only for Core DKAL **)
type infostrate = list term
type substrate
type varDecl = list var
type prefix = list term

(*************)
(* Utilities *)
(*************)

type Mem :: 'a::* => 'a => list 'a => P =
(* = Inductive definition of list membership in a constructive style *)
(* look at fine/src/certify/coretyping.fst (module FniteSet for Mem and decideMem) *)
  | Mem_hd : a:'a -> t:list 'a -> Mem 'a a (a::t)
  | Mem_tl : a:'a -> h:'a -> t:list 'a -> Mem 'a a t -> Mem 'a a (h::t)

val mem : a:'a -> l:list 'a -> option (Mem 'a a l)
let rec mem (a : 'a) (l: list 'a) : option (Mem 'a a l) =
  match l with
  | [] -> None
  | h::t when h=a -> Some(Mem_hd a t)
  | h::t -> (match mem a t with
               | None -> None
               | Some(m) -> Some(Mem_tl a h t m))
			   
val contains : a:'a -> l:list 'a -> b:bool{((b=true) <=> (In 'a a l))}
(* TODO *) 

val memInfostrate : i:term -> is:infostrate -> option (Mem i is)
let memInfostrate i _I = mem i _I


(* Intuition:                                                 *)
(* MkPrefix [p1;...;pn] i (p1 said (p2 said ... (pn said i))) *)
type MkPrefix :: list term => term => term => P =
  | MkPrefix_Nil : i:term -> MkPrefix [] i i
  | MkPrefix_Cons : p:term -> ps:list term -> i:term -> i':term
                   -> MkPrefix ps i i'
                   -> MkPrefix (p::ps) i (App SaidInfon [p; i'])
                   (* Rk: cannot use said here *)

(* Intuition:                                                    *)
(* mkPrefix [p1;...;pn] i = ( (p1 said (... pn said i)), proof ) *)
val mkPrefix: pref:prefix -> i:term -> (i':term * dummy:(MkPrefix pref i i'){pref=[] => i=i'})
let rec mkPrefix pref i =
  (match pref with
     | [] -> (i, MkPrefix_Nil i)
     | h::t -> let (j, m) = mkPrefix t i in
                 (App SaidInfon [h; j], MkPrefix_Cons h t i j m))
                 (* Rk: if using the function said here, can't prove it *)


(* judgment: is l a list of just constants c *)
type ConstList :: 'a::* => 'a => list 'a => P =
  | ConstList_Nil : 'a::* -> c:'a -> ConstList 'a c []
  | ConstList_Cons : 'a::* -> c:'a -> t:list 'a
                   -> ConstList 'a c t
                   -> ConstList 'a c (c::t)
val constList: c:'a -> l:list 'a -> option(ConstList c l)
let rec constList c = function
  | [] -> Some(ConstList_Nil c)
  | h::t when h = c -> 
      (match constList c t with
         | None -> None
         | Some p -> Some(ConstList_Cons c t p))
  | _ -> None


val fold_left_dep : res:'a -> l:list 'b -> ('a -> x:'b -> Mem x l -> 'a) -> 'a
let rec fold_left_dep res l f = match l with
  | [] -> res
  | h::t -> fold_left_dep (f res h (Mem_hd h t)) t
                          (fun res x m -> f res x (Mem_tl x h t m))

val collect_dep : l:list 'a -> (x:'a -> Mem x l -> list 'b) -> list 'b
let rec collect_dep l f =
  fold_left_dep [] l (fun res x m -> append res (f x m))

val option_map : f:('a -> 'b) -> oa:option 'a 
              -> (*ob:*)option 'b(*{forall a. oa = (Some a) => ob = (Some (f a))}*)
let option_map f = function
  | None -> None
  | Some a -> Some (f a)


(********** Zip functions taken from coretyping.fst *************)
type MapL :: 'a::* => ('a => P) => list 'a => P =
   | MapL_Nil : 'a::* -> 'Q::('a => P)
             -> MapL 'a 'Q []
   | MapL_Cons : 'a::* -> 'Q::('a => P)
              -> t:list 'a
			  -> h:'a
			  -> MapL 'a 'Q t
			  -> 'Q h
			  -> MapL 'a 'Q (h::t)
			  
val mapL_p: 'a::* -> 'Q::('a => P)
         -> f:(x:'a -> option ('Q x))
		 -> l:list 'a
		 -> option (MapL 'a 'Q l)
let rec mapL_p f l = match l with
  | [] -> Some (MapL_Nil<'a,'Q>)
  | h::t -> 
     match mapL_p<'a,'Q> f t, f h with
	   | Some pf_tl, Some pf_hd ->
	       Some (MapL_Cons<'a,'Q> t h pf_tl pf_hd)
	   | _ -> None

type Zip :: 'a::* => 'b::* => ('a => 'b => P) => list 'a => list 'b => P = 
   | Zip_Nil : 'a::* -> 'b::* -> 'Q::('a => 'b => P) 
             -> Zip 'a 'b 'Q [] []
   | Zip_Cons: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
             -> l1:list 'a -> l2:list 'b 
             -> x:'a -> y:'b 
             -> Zip 'a 'b 'Q l1 l2
             -> 'Q x y
             -> Zip 'a 'b 'Q (x::l1) (y::l2)

val zip_p: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
      -> f:(x:'a -> y:'b -> option ('Q x y))
      -> l1:list 'a
      -> l2:list 'b
      -> option (Zip 'a 'b 'Q l1 l2)
let rec zip_p f l1 l2 = match l1, l2 with
  | [],[] -> Some (Zip_Nil<'a,'b,'Q>)
  | (x1::tl1), (x2::tl2) ->
      match zip_p<'a,'b,'Q> f tl1 tl2, f x1 x2 with
        | (Some pf_tl), Some pf_hd -> Some (Zip_Cons<'a,'b,'Q> tl1 tl2 x1 x2 pf_tl pf_hd)
        | _ -> None

val map_p: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
      -> f:(x:'a -> (y:'b * 'Q x y))
      -> l1:list 'a
      -> (l2:list 'b * Zip 'a 'b 'Q l1 l2)
let rec map_p f l1 = match l1 with
  | [] -> [], Zip_Nil<'a,'b,'Q>
  | x::tlx ->
      let y, pfHd = f x in
      let tly, pfTl = map_p<'a,'b,'Q> f tlx in
        (y::tly), Zip_Cons<'a,'b,'Q> tlx tly x y pfTl pfHd
        
val map_p_opt: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
      -> f:(x:'a -> option (y:'b * 'Q x y))
      -> l1:list 'a
      -> option (l2:list 'b * Zip 'a 'b 'Q l1 l2)
let rec map_p_opt f l1 = match l1 with
  | [] -> Some(([], Zip_Nil<'a,'b,'Q>))
  | x::tlx ->
      match f x, map_p_opt<'a,'b,'Q> f tlx with
        | Some((y, pfHd)), Some((tly, pfTl)) ->
            Some(((y::tly), Zip_Cons<'a,'b,'Q> tlx tly x y pfTl pfHd))
        | _ -> None

val map_mapL_p: 'a::* -> 'b::* 
            -> 'Q::('a => 'b => P)
			-> 'R::('b => P)
			-> f:(x:'a -> option(x':'b * 'Q x x' * 'R x'))
			-> l:list 'a
			-> option (l':list 'b * Zip 'a 'b 'Q l l' * MapL 'b 'R l')
let rec map_mapL_p f l = match l with
  | [] -> Some(([], Zip_Nil, MapL_Nil))
  | h::t -> 
     (match f h, map_mapL_p f t with
	    | Some((h', qh, rh)), Some((t', qt, rt)) -> 
            Some(((h'::t'), (* need parenthesis around the list; precedence of :: and , *)
			      Zip_Cons<'a, 'b, 'Q> t t' h h' qt qh,
                  MapL_Cons<'b, 'R> t' h' rt rh))
	    | _ -> None)

(*
type Nth :: 'a::* => l:list 'a => i:int => x:'a => P =
  | Nth_b: 'a::* -> h:'a -> t:list 'a
           -> Nth 'a (h::t) 0 h
  | Nth_r: 'a::* -> h:'a -> t:list 'a -> i:int -> x:'a
           (*-> Nth 'a t (i-1) x*) (*??*)
		   -> Nth 'a (h::t) i x
val nth_p: 'a::* -> l:list 'a -> i:int -> 
         option (x:'a * Nth 'a l i x)
let rec nth_p l i = match l with
  | [] -> None
  | h::t -> 
     match nth_p t (i-1) with
	   | None -> None
	   | Some((x, pr)) -> Some((x, Nth_r<'a> h t i x (*pr*)))
*) 
  
(******************************)
(* Trusted external functions *)
(******************************)

(* 1. Dynamic type tests from .NET *)
type TypeOf :: object => typ => E
(* extern Runtime  FIXME *) val check_object_typ : x:object -> t:typ -> b:bool{b=true => TypeOf x t}

(* 2. Queries to external databases i.e., Substrate *)
type SubstrateSays :: substrate => ISubstrateQueryTerm => E
(* extern Runtime  FIXME *) val check_substrate : s:substrate -> q:ISubstrateQueryTerm -> b:bool{b=true => SubstrateSays s q}

(***********************)
(* Inductive judgments *)
(***********************)

type FuncTyping :: f:func => typArgs:list typ => typRes:typ => P =
  (* 5 with variable number of arguments *)
  | FuncTyping_SeqRule : l:list typ -> ConstList RuleT l 
                      -> FuncTyping SeqRule l RuleT
  | FuncTyping_SeqCondition : l:list typ -> ConstList Condition l 
                           -> FuncTyping SeqCondition l Condition
  | FuncTyping_SeqAction : l:list typ -> ConstList Action l
                        -> FuncTyping SeqAction l Action
  | FuncTyping_AndInfon : l:list typ -> ConstList Infon l
                       -> FuncTyping AndInfon l Infon
  | FuncTyping_AndEvidence : l:list typ -> ConstList Evidence l
                       -> FuncTyping AndEvidence l Evidence
  (* all others *)
  | FuncTyping_EmptyRule : FuncTyping EmptyRule [] RuleT
  | FuncTyping_Rule : FuncTyping Rule [Condition; Action] RuleT
  | FuncTyping_RuleOnce : FuncTyping RuleOnce [Condition; Action] RuleT
  | FuncTyping_EmptyCondition : FuncTyping EmptyCondition [] Condition
  | FuncTyping_WireCondition : FuncTyping WireCondition [Infon; Principal] Condition
  | FuncTyping_KnownCondition : FuncTyping KnownCondition [Infon] Condition
  | FuncTyping_EmptyAction : FuncTyping EmptyAction [] Action
  | FuncTyping_Send : FuncTyping Send [Principal; Infon] Action
  | FuncTyping_JustifiedSend : FuncTyping JustifiedSend [Principal; Infon] Action
  | FuncTyping_JustifiedSay : FuncTyping JustifiedSay [Principal; Infon] Action
  | FuncTyping_Learn : FuncTyping Learn [Infon] Action
  | FuncTyping_Forget : FuncTyping Forget [Infon] Action
  | FuncTyping_Install : FuncTyping Install [RuleT] Action
  | FuncTyping_Uninstall : FuncTyping Uninstall [RuleT] Action
  | FuncTyping_Apply : FuncTyping Apply [SubstrateUpdate] Action
  | FuncTyping_Drop : FuncTyping Drop [Infon] Action
  | FuncTyping_EmptyInfon : FuncTyping EmptyInfon [] Infon
  | FuncTyping_AsInfon : FuncTyping AsInfon [SubstrateQuery] Infon
  | FuncTyping_ImpliesInfon : FuncTyping ImpliesInfon [Infon; Infon] Infon
  | FuncTyping_SaidInfon : FuncTyping SaidInfon [Principal; Infon] Infon
  | FuncTyping_JustifiedInfon : FuncTyping JustifiedInfon [Infon; Evidence] Infon
  | FuncTyping_EmptyEvidence : FuncTyping EmptyEvidence [] Evidence
  | FuncTyping_SignatureEvidence : FuncTyping SignatureEvidence 
      [Principal; Infon; Int32] Evidence
  | FuncTyping_ModusPonensEvidence : FuncTyping ModusPonensEvidence 
      [Evidence; Evidence] Evidence
  | FuncTyping_AsInfonEvidence : FuncTyping AsInfonEvidence [SubstrateQuery] Evidence
  | FuncTyping_RelationInfon : r:relationInfon 
                  -> FuncTyping (RelationInfon r) r.argsType r.retType

val funcTyping : f:func -> typArgs:list typ 
              -> option (typRes:typ * FuncTyping f typArgs typRes)
let funcTyping f typArgs = 
  let getTyp = function (* it's also the return type *)
    | SeqRule -> RuleT, RuleT
    | SeqCondition -> Condition, Condition
    | SeqAction -> Action, Action
    | AndInfon -> Infon, Infon
    | AndEvidence -> Evidence, Evidence in
  match f, typArgs with
  (* 5 with variable number of arguments *)
  (*| (SeqRule, l) | (SeqCondition, l) | (SeqAction, l) (* Rk: no multiple match? *)
  | (AndInfon, l) | (AndEvidence, l) -> *)
  (* Rk: in conditions, = seems to bind tighter than || *)
  | SeqRule, _ -> 
      (match constList RuleT typArgs with
       | None -> None
       | Some c -> Some((RuleT, FuncTyping_SeqRule typArgs c)))
  | SeqCondition, _ -> 
      (match constList Condition typArgs with
       | None -> None
       | Some c -> Some((Condition, FuncTyping_SeqCondition typArgs c)))
  | SeqAction, _ -> 
      (match constList Action typArgs with
       | None -> None
       | Some c -> Some((Action, FuncTyping_SeqAction typArgs c)))
  | AndInfon, _ -> 
      (match constList Infon typArgs with
       | None -> None
       | Some c -> Some((Infon, FuncTyping_AndInfon typArgs c)))
  | AndEvidence, _ -> 
      (match constList Evidence typArgs with
       | None -> None
       | Some c -> Some((Evidence, FuncTyping_AndEvidence typArgs c)))
  (* all others *)
  | EmptyRule, [] -> Some((RuleT, FuncTyping_EmptyRule))
  | Rule, [Condition; Action] -> Some((RuleT, FuncTyping_Rule))
  | RuleOnce, [Condition; Action]-> Some((RuleT, FuncTyping_RuleOnce))
  | EmptyCondition, []-> Some((Condition, FuncTyping_EmptyCondition))
  | WireCondition, [Infon; Principal]-> Some((Condition, FuncTyping_WireCondition))
  | KnownCondition, [Infon]-> Some((Condition, FuncTyping_KnownCondition))
  | EmptyAction, []-> Some((Action, FuncTyping_EmptyAction))
  | Send, [Principal; Infon]-> Some((Action, FuncTyping_Send))
  | JustifiedSend, [Principal; Infon]-> Some((Action, FuncTyping_JustifiedSend))
  | JustifiedSay, [Principal; Infon]-> Some((Action, FuncTyping_JustifiedSay))
  | Learn, [Infon] -> Some((Action, FuncTyping_Learn))
  | Forget, [Infon]-> Some((Action, FuncTyping_Forget))
  | Install, [RuleT]-> Some((Action, FuncTyping_Install))
  | Uninstall, [RuleT]-> Some((Action, FuncTyping_Uninstall))
  | Apply, [SubstrateUpdate]-> Some((Action, FuncTyping_Apply))
  | Drop, [Infon]-> Some((Action, FuncTyping_Drop))
  | EmptyInfon, [] -> Some((Infon, FuncTyping_EmptyInfon))
  | AsInfon, [SubstrateQuery]-> Some((Infon, FuncTyping_AsInfon))
  | ImpliesInfon, [Infon; Infon]-> Some((Infon, FuncTyping_ImpliesInfon))
  | SaidInfon, [Principal; Infon]-> Some((Infon, FuncTyping_SaidInfon))
  | JustifiedInfon, [Infon; Evidence]-> Some((Infon, FuncTyping_JustifiedInfon))
  | EmptyEvidence, []-> Some((Evidence, FuncTyping_EmptyEvidence))
  | SignatureEvidence, [Principal; Infon; Int32]-> 
      Some((Evidence, FuncTyping_SignatureEvidence))
  | ModusPonensEvidence, [Evidence; Evidence]-> 
      Some((Evidence, FuncTyping_ModusPonensEvidence))
  | AsInfonEvidence, [SubstrateQuery]-> 
      Some((Evidence, FuncTyping_AsInfonEvidence))
  | RelationInfon r, a when a = r.argsType -> 
      Some((r.retType, FuncTyping_RelationInfon r))

type types :: varDecl => term => typ => P =
  | Types_Var : G:varDecl
               -> v:var
               -> Mem v G
               -> types G (Var v) v.typ
               (* TODO: change rules for name clashes/scope *)
  | Types_ConstTrueT : G:varDecl -> types G (Const TrueT) Boolean
  | Types_ConstFalseT : G:varDecl -> types G (Const FalseT) Boolean
  | Types_ConstPrincipalConstant : G:varDecl -> p:principal
                             -> types G (Const (PrincipalConstant p)) Principal
  | Types_ConstSubstrateConstant : G:varDecl -> t:typ -> o:object{TypeOf o t}
                                 -> types G (Const (SubstrateConstant o)) t
  | Types_ForallT : G:varDecl -> x:var -> i:term
                   -> types (x::G) i Infon
                   -> types G (ForallT x i) Infon
  (*??: is this rule really true? *)
  | Types_SubstrateQueryTerm : G:varDecl -> q:ISubstrateQueryTerm 
                               (* TODO: check this rule *)
                               -> types G (SubstrateQueryTerm q) SubstrateQuery
  (* generic App rule that replaces all the others *)
  | Types_App : g:varDecl 
             -> f:func
             -> args:list term
             -> typArgs: list typ
             -> typRes: typ
             -> Zip term typ (types g) args typArgs
             (*-> Zip (fun i t -> types g i t) ilist tylist*) (* ask Nik difference *)
             -> FuncTyping f typArgs typRes
             -> types g (App f args) typRes


			 

			 
(* val subst_special: names:binders -> i:term -> x:var -> Subst names i x (Var x) i *)

(* Axiomatization of sets of variables *)
type varset = list var
logic function Empty : varset
logic function Singleton : var -> varset
logic function Union : varset -> varset -> varset
logic function SetMem : var -> varset -> bool
logic function BoolOr : bool -> bool -> bool
assume forall (b:bool). (BoolOr b true) = true
assume forall (b:bool). (BoolOr true b) = true
assume forall (v:var). (SetMem v (Singleton v)) = true
assume forall (l1:varset) (l2:varset) (v:var). 
  SetMem v (Union l1 l2) = (BoolOr (SetMem v l1) (SetMem v l2))
assume forall (v:var). (SetMem v Empty) = false
(* extern Runtime *) val union : s1:varset -> s2:varset -> s3:varset{(s3 = (Union s1 s2))}

(* Axiomatization of substitutions *)
logic function EmptySubst : substitution
logic function Domain : substitution -> varset
logic function Select : substitution -> var -> term
logic function Update : substitution -> var -> term -> substitution
logic function FreeVars : term -> varset
logic function FreeVarsSubst : substitution -> varset 
  (* set of variable in both the domain and the range of the map *)
assume forall (s:substitution) (x:var) (t:term). 
  (Select (Update s x t) x) = t
assume forall (s:substitution) (x:var) (y:var) (t:term).
  (x <> y) => ((Select (Update s x t) y) = (Select s y))
assume forall (s:substitution) (x:var) (t:term). 
  Domain (Update s x t) = (Union (Domain s) (Singleton x))
assume (Domain EmptySubst) = Empty

logic function Subst : term -> substitution -> term 
logic function SubstList: list term -> substitution -> list term
  (* map of subst over the list *)
assume Subst_VarRepl: (forall (x:var) (s:substitution).
        ((SetMem x (Domain s)) = true) => ((Subst (Var x) s) = (Select s x)))
assume Subst_VarIgnore: (forall (x:var) (s:substitution).
        ((SetMem x (Domain s)) = false) => ((Subst (Var x) s) = (Var x)))
assume Subst_Const: 
        (forall (c:constant) (s:substitution). (Subst (Const c) s) = (Const c))
assume Subst_app: (forall (f:func) (args:list term) (s:substitution).
        (Subst (App f args) s) = (App f (SubstList args s)))
assume Subst_forallT_1: 
        (forall (x:var) (t:term) (s:substitution). 
           (not(In x (FreeVarsSubst s))) => 
		       ((Subst (ForallT x t) s) = (ForallT x (Subst t s))))
assume Subst_forallT_2: 
        (forall (x:var) (t:term) (s:substitution) (y:var). 
           ((not(In y (Union (FreeVarsSubst s) (FreeVars t))))) => 
            (Subst (ForallT x t) s) =
              (ForallT y (Subst (Subst t (Update EmptySubst x (Var y))) s)))
assume SubstList_nil:
        (forall (s:substitution). (SubstList [] s) =  [])
assume SubstList_cons:
        (forall (t:term) (tl:list term) (s:substitution).
           (SubstList (t::tl) s) = ((Subst t s)::(SubstList tl s)))

(* extern Runtime *) val emptySubst: unit -> s:substitution{s=EmptySubst}
(* extern Runtime *) val lookupVar : s:substitution -> x:var 
        -> o:option term{((o=None <=> (false=(SetMem x (Domain s)))) &&
                          (forall (t:term). (o=(Some t)) => 
						     ((SetMem x (Domain s) = true) && (t=(Select s x)))))}
(* extern Runtime *) val extendSubst : s:substitution -> x:var -> t:term 
        -> (s':substitution{s'=(Update s x t)})
(* extern Runtime *) val inDomain : s:substitution -> x:var 
        -> b:bool{b=(SetMem x (Domain s))}
(* extern Runtime *) val inFreevars : s:substitution -> x:var 
        -> b:bool{b=(SetMem x (FreeVarsSubst s))}
(* added *)
(* extern Runtime *) val forgetSubst : s:substitution -> x:var 
        -> (substitution) (* spec: TODO *)
(* extern Runtime *) val freeVarsSubst : s:substitution 
                                        -> f:varset{(f = (FreeVarsSubst s))}
(* extern Runtime *) val freeVars : t:term -> f:varset{(f = (FreeVars t))}
(* (* extern Runtime *) val substrateQuerySubst : q1:queryTerm 
        -> s:substitution -> q2:term{q2=(Subst (SubstrateQueryTerm q1) s)} *)

(* functions from builders.fs *)


(*
val mem_assoc : x:string -> s:varset 
              -> b:bool{ ((b=false) => (not(In x s)) }
let rec mem_assoc x s = match s with
  | [] -> false
  | y :: tl -> 
     if y = x then true else mem_assoc x tl
*)

let _freshVarId = ref 0 (* can't go inside because no inside rec function *)

val freshVar : ty:typ -> s:varset 
             -> x:var{ not ( In x s ) }
let rec freshVar ty s =
  let n = { typ = ty; name = (Concat "fresh" (string_of_any (!_freshVarId))) } in
   (_freshVarId := !_freshVarId + 1); (* Rk: parenthesis needed *)
   if not (contains n s) 
   then n 
   else freshVar ty s
     
(* from term.fst *)	 


extern reference TranslationToFStar {language="F#";
                                     dll="TranslationToFStar";
                                     namespace="";
                                     classname="TranslationToFStar"}
extern TranslationToFStar val FStarVarOfIVar : IVar -> Types.var
extern TranslationToFStar val FStarTermOfITerm : ITerm -> Types.term
extern TranslationToFStar val FStarSubstitutionOfISubstitution: ISubstitution -> Types.substitution	 
	 
(* see term_apply in term.fst *)
val subst: i:term -> s:substitution -> i':term{i'=(Subst i s)}
val substList : tl:list term -> s:substitution -> tl':list term{tl'=(SubstList tl s)}
let rec subst i s = 
  match i with
    | Var y -> 
        (match lookupVar s y with 
           | None -> Var y
           | Some t -> t)
          
    | Const c -> Const c
        
    | App f tl -> App f (substList tl s)
    
	| ForallT x t -> (* code taken partially from term.fst *)
      (* the substitution is not applied to the quantified variable *)
        let y : (yy:var{not(In yy (Union (FreeVars t) (FreeVarsSubst s)))}) =
          freshVar x.typ (union (freeVars t) (freeVarsSubst s)) in
		let ySubst : (ys:substitution{(ys = (Update EmptySubst x (Var y)))}) =
          extendSubst (emptySubst ()) x (Var y) in
		let t' : (tt:term{(tt = (Subst t (Update EmptySubst x (Var y))))}) =
          subst t ySubst in
		let t'' : (tt:term{(tt = (Subst (Subst t (Update EmptySubst x (Var y))) s))}) =
          (subst t' s) in
        let res = Types.ForallT y t'' in 
		let _ = Assume <(res = (Subst i s))> () in
		res
		
	| SubstrateQueryTerm t0 ->
	    let res = 
          FStarTermOfITerm
            (TypeHeaders.substrateQueryTerm_apply t0  
		       (* not sure why I need the TypeHeaders. , TypeHeaders has been opened *)
		       (TranslationFromFStar.ISubstitutionOfFStarSubstitution s) )
		in let _ = Assume <(res = (Subst i s))> () in
		res
	
	| SubstrateUpdateTerm t0 ->
	    let res =
          FStarTermOfITerm 
            (TypeHeaders.substrateUpdateTerm_apply t0 
		       (TranslationFromFStar.ISubstitutionOfFStarSubstitution s) )
		in let _ = Assume <(res = (Subst i s))> () in
		res
	
	| ConcretizationEvidence t1 s1 -> raise "Evidences not handled"
	  (* ConcretizationEvidence (ConcretizationEvidence t1 s1) s *) (* correct?? *)
      (* Types.ConcretizationEvidence t1 (composeWith s s1) *)
	
    
	
	

(*     | ForallT y j when x=y -> *)
(*         ForallT y j, Subst_ForallTIgnore x j u *)
(* 	| ForallT y j -> *)
(* 	    let (j', pr) = subst j x u in *)
(*           ForallT y j', Subst_ForallTRepl y j x u j' pr *)
(* 	| SubstrateQueryTerm q -> *)
(* 	    SubstrateQueryTerm q, Subst_SubstrateQueryTerm q x u *)

and substList ilist s = (* why not map (fun i -> subst i s) ilist ? *)
  (*map (fun i -> subst i s) ilist*)
  match ilist with
  | [] -> []
  | hd::tl -> 
      let tl' = substList tl s in 
      let hd' = subst hd s in
        Cons<term>  hd' tl'
		(* type inference is too eager here; need to provide explicit annotation on Cons *)
			 
			 
			 
(* type names = list string *)
(* type Subst :: (\* binders crossed *\) names => (\* subst into this term *\) term => (\* for this *\) var => (\* with this *\) term => (\* result *\) term => P =  *)
(*   | Subst_ForallTRepl : binders:names -> y:name -> t:typ -> i:term -> x:var{x<>y} -> j:term -> result:term *)
(*                       -> Subst (y::binders) i x j result *)
(*                       -> Subst (ForallT (y,t) i) x j (ForallT (y,t) result) *)

(*   | Subst_VarRepl_NoCapture : binders:list string -> x:var -> j:term -> freenames_j:names  *)
(*                             -> FreeVariables j freenames_j *)
(*                             -> Disjoint binders freenames_j *)
(*                             -> Subst binders (Var x) x j j *)

(* val my_lemma: names:binders -> i:term -> x:var -> j:term -> Subst names i x (Var x) j -> option (Eq i j) *)
(* let my_lemma names i x j pf =  *)
(*   if i=j then Some (Refl_eq i) *)
(*   else raise "Impos" *)

(* val subst_special: names:binders -> i:term -> x:var -> Subst names i x (Var x) i *)


(* Subst i x u i'  iff  i[u/x] = i' *)
(** OLD *)
(*
type Subst :: term => var => term => term => P =
  | Subst_VarRepl : x:var -> u:term -> Subst (Var x) x u u
  | Subst_VarIgnore : y:var -> u:term -> x:var{not(x = y)}
                    -> Subst (Var y) x u (Var y)
  | Subst_Const : x:var -> u:term -> c:constant
                -> Subst (Const c) x u (Const c)
  | Subst_ForallTIgnore : x:var -> i:term -> u:term
                        -> Subst (ForallT x i) x u (ForallT x i)
  | Subst_ForallTRepl : y:var -> i:term
                      -> x:var{not(x = y)}
                      -> u:term
                      -> i' : term
                      -> Subst i x u i'
                      -> Subst (ForallT y i) x u (ForallT y i')
  | Subst_App : f:func -> x:var -> u:term
              -> ilist:list term -> ilist':list term
			  -> Zip term term (fun i i' => Subst i x u i') ilist ilist'
			  -> Subst (App f ilist) x u (App f ilist')

  (* Is this really true? Any variable ever in the ISubstrateQueryTerm? *)
  | Subst_SubstrateQueryTerm : q:ISubstrateQueryTerm -> x:var -> u:term
                     -> Subst (SubstrateQueryTerm q) x u (SubstrateQueryTerm q)
*)
type entails :: substrate => infostrate => varDecl => term => P =
  | Entails_Emp : S:substrate -> I:infostrate -> G:varDecl
               -> pref: prefix
               -> result: term
               -> MkPrefix pref (App EmptyInfon []) result
                  (* Rk: cannot just use empty in place of App EmptyInfon [] *)
               -> entails S I G result

  | Entails_Hyp_Knowledge : S:substrate -> I:infostrate -> G:varDecl
                          -> i:term
                          -> Mem i I
                          -> entails S I G i

  | Entails_Hyp_Substrate : S:substrate -> I:infostrate -> G:varDecl
                          -> q:ISubstrateQueryTerm{SubstrateSays S q}
                          (* Eventually, we get a signature from the DB about this fact *)
                          -> entails S I G (App AsInfon [(SubstrateQueryTerm q)])
                          (* Rk: need parenthesis inside the list *)


  (* Q: here order of G matters; make sure that's all right; I think so *)
  | Entails_Q_Intro : S:substrate -> I:infostrate -> G:varDecl
                    -> x:var -> i:term -> i':term
                    -> result:term
                    -> pref:prefix
                    -> MkPrefix pref (ForallT x i) result
                    -> MkPrefix pref i i'
                    -> entails S I (x::G) i'
                    -> entails S I G result

  | Entails_Q_Inst : S:substrate -> I:infostrate -> G:varDecl
                   -> v:var
                   -> i:term
                   -> i':term
                   -> u:term
                   -> pref:prefix
                   -> hyp:term
                   -> result:term
                   -> MkPrefix pref (ForallT v i) hyp
                   (* -> Subst i v u i' *) (* TODO *)
                   -> MkPrefix pref i' result
                   -> entails S I G hyp
                   -> types G u v.typ
                   -> entails S I G result

  | Entails_And_Intro : S:substrate -> I:infostrate -> G:varDecl
                     -> ilist:list term
					 -> ilist':list term
					 -> result:term
					 -> pref:prefix
					 -> MkPrefix pref (App AndInfon ilist) result
					 -> Zip term term (MkPrefix pref) ilist ilist'
					 -> MapL term (entails S I G) ilist'
					 -> entails S I G result

  | Entails_And_Elim : S:substrate -> I:infostrate -> G:varDecl
                    -> ilist:list term
					-> i:term
					-> i':term
					-> pref: list term
					-> hyp:term
					-> Mem i ilist
					-> MkPrefix pref (App AndInfon ilist) hyp
					-> MkPrefix pref i i'
					-> entails S I G hyp
					-> entails S I G i'

  | Entails_W_Imp_Intro : S:substrate -> I:infostrate -> G:varDecl
                      -> i:term
                      -> j:term -> j':term
                      -> result:term
                      -> pref: list term
                      -> MkPrefix pref j j'
                      -> MkPrefix pref (App ImpliesInfon [i; j]) result
                      -> entails S I G j'
                      -> entails S I G result

  | Entails_Imp_Elim : S:substrate -> I:infostrate -> G:varDecl
                     -> i : term -> i':term (* = pref i *)
                     -> j : term -> j':term (* = pref j *)
                     -> pref : list term
                     -> hyp : term (* = pref ( i => j ) *)
                     -> MkPrefix pref i i'
                     -> MkPrefix pref (App ImpliesInfon [i; j]) hyp
                     -> MkPrefix pref j j'
                     -> entails S I G i'
                     -> entails S I G hyp
                     -> entails S I G j'

(*************************)
(* Derivation algorithms *)
(*************************)

val doType: g:varDecl -> t:term -> option(ty:typ * (types g t ty))
let rec doType g = function
  | Var v -> (match mem v g with (* TODO: change with new rules *)
                | None -> None
                | Some m -> Some((v.typ, Types_Var g v m)))
  | Const TrueT -> Some((Boolean, Types_ConstTrueT g))
  | Const FalseT -> Some((Boolean, Types_ConstFalseT g))
  | Const(PrincipalConstant p) ->
      Some((Principal,
            Types_ConstPrincipalConstant g p))
  | ForallT x i -> (match doType (x::g) i with
                     | None -> None
                     | Some((Infon, pr)) ->
                         Some((Infon, Types_ForallT g x i pr))
                     | Some _ -> None)
  | SubstrateQueryTerm q -> Some(SubstrateQuery, Types_SubstrateQueryTerm g q)
  | App f args -> 
     (match map_p_opt (doType g) args with
        | None -> None
        | Some((typArgs, przip)) ->
           (match funcTyping f typArgs with
              | None -> None
              | Some((typRes, fpr)) ->
                   Some((typRes, 
				         Types_App g f args typArgs typRes przip fpr))))
  | _ -> None

(*
val subst: i:term -> x:var -> u:term -> (i':term * Subst i x u i')
let rec subst i x u =
  match i with
    | Var y when x=y -> u, Subst_VarRepl x u
    | Var y -> Var y, Subst_VarIgnore y u x (* Rk: no need for a second when. Nice! *)
    | Const c -> Const c, Subst_Const x u c
    | ForallT y j when x=y ->
        ForallT y j, Subst_ForallTIgnore x j u
    | ForallT y j ->
        let (j', pr) = subst j x u in
          ForallT y j', Subst_ForallTRepl y j x u j' pr
	| App f ilist ->
	    let (ilist', przip) = 
          map_p<term, term, (fun i i' => Subst i x u i')>
		  (* Rk: type annotation necessary *)
               (fun i -> subst i x u) ilist in
		  (App f ilist', Subst_App f x u ilist ilist' przip)
    | SubstrateQueryTerm q ->
        SubstrateQueryTerm q, Subst_SubstrateQueryTerm q x u
*)
		
		
(*
(* TODO: syntax?? *)
(* type entails_subst (*:: substrate => infostrate => varDecl => term => P =*)
  (ss:substrate) (ii:infostrate) (gg:varDecl) (t:term) (s:substitution) =
     ( entails ss ii gg (Subst t s) ) *)

val doDerive: S:substrate -> I:infostrate -> G:varDecl
             -> pref:prefix -> target:term
			 -> s:substitution
             -> option (target':term *
                        MkPrefix pref target target' *
						s':substitution * (* s' more constrained than s *)
                        entails_subst S I G target' s')
						
val tryDerive: S:substrate -> I:infostrate -> G:varDecl
             -> pref:prefix -> target:term
			 -> s:substitution
             -> infon:term 
             -> entails S I G infon (* or entails_subst S I G infon s ? *)
             -> option (target':term *
                        MkPrefix pref target target' *
						s':substitution *
                        entails S I G target')
*)

val doDerive: S:substrate -> I:infostrate -> G:varDecl
             -> pref:prefix -> target:term
             -> option (target':term *
                        MkPrefix pref target target' *
                        entails S I G target')
                        
val tryDerive: S:substrate -> I:infostrate -> G:varDecl
             -> pref:prefix -> target:term
             -> infon:term 
             -> entails S I G infon
             -> option (target':term *
                        MkPrefix pref target target' *
                        entails S I G target')

(* tries to derive (pref target) using (infon); pr is a proof on infon *)
let rec tryDerive _S _I _G pref target infon pr =
  match infon with
  (* NO! need to use substitutions *)
   (*
  | App SaidInfon [p; infon2] ->
      (match pref with
       | p2 :: tl when p = p2 ->
          (match tryDerive _S _I _G tl target infon2 with
             | Some((target', m, e)) ->
                  (*let (target'', m2) = mkPrefix tl target in*)
                  Some((App SaidInfon [p; target'],
                        MkPrefix_Cons p tl target target' m,
                        e))
             | _ -> None)
       | _ -> None) *)
  | App ImpliesInfon [i; j] -> (* to use i => j you need to *)
      (match doDerive _S _I _G [] i with (* first prove i *)
       | None -> None
       | Some((i', mi, ei)) ->
          let (j', mj) = mkPrefix [] j in
          let (hyp, mhyp) = mkPrefix [] infon in 
          let prj = Entails_Imp_Elim _S _I _G i i' j j' [] hyp mi mhyp mj ei pr in
            tryDerive _S _I _G pref target j prj) (* then prove (pref target) using j *)

  | App AndInfon ilist -> (* to And(ilist), we can use anyone in the ilist*)
                          (* eventually the list should contain both solutions *)
      let (hyp, mhyp) = mkPrefix [] infon in
	  fold_left_dep None ilist
	    (fun res i memi -> match res with
		   | Some _ -> res (* we don't go further, eventually concatenate sols *)
		   | None ->
		       let (i', mi) = mkPrefix [] i in
		       tryDerive _S _I _G pref target i
                 (Entails_And_Elim _S _I _G ilist i i' [] hyp memi mhyp mi pr))
       
  (*| App ForallT x i ->*)

  | _ -> None

and doDerive _S _I _G pref target =
  let memInfostrate_target = memInfostrate target _I in
  match target with
  | App EmptyInfon [] -> let (j, m) = mkPrefix pref target in
                      (* if unable to make F* prove sth, put in a runtime check *)
                      (* if j = Empty then *)
                     Some((j, m,
                           Entails_Emp _S _I _G pref j m))
                      (* else raise "stupid F*" *)

  | i when ((pref = []) && (not (memInfostrate_target = None))) ->
      let (j, m) = mkPrefix pref target in
      (match memInfostrate_target with
         | None -> raise "assert false"
         | Some mis -> Some((j, m, Entails_Hyp_Knowledge _S _I _G i mis)))

  | App AsInfon [SubstrateQueryTerm q] ->
      (match pref with
         | [] -> let (j, m) = mkPrefix pref target in
             (match check_substrate _S q with
                | false -> None
                | true -> Some((j, m, Entails_Hyp_Substrate _S _I _G q)))
         | _ -> None)
         
  | ForallT x i ->
      (match doDerive _S _I (x::_G) pref i with
         | None -> None
         | Some((i', mi, ei)) ->
             let (j, m) = mkPrefix pref target in
                Some((j, m, Entails_Q_Intro _S _I _G x i i' j pref m mi ei)))
  (*              
  | ForallT x i ->  (* doesn't type ?? *)
     let s__ = _S in let i__ = _I in let g__ = _G in
     (option_map
       (fun (i', mi, ei) ->
           let (target', m) = mkPrefix pref target in
              ((target', m, Entails_Q_Intro _S _I _G x i i' target' pref m mi ei)
			   : (target':term *
                  MkPrefix pref target target' *
                  entails s__ i__ g__ target')))
       (doDerive _S _I (x::_G) pref i)) *)
  
  | App AndInfon ilist ->
	   match map_mapL_p (doDerive _S _I _G pref) ilist with
	     | Some((ilist', m, e)) -> 
		     let (target', mt) = mkPrefix pref target in
			 Some((target', mt,
			       Entails_And_Intro _S _I _G ilist ilist' target' pref mt m e))
         | _ -> None
		 
  | App ImpliesInfon [i; j] ->
      (match doDerive _S _I _G pref j with
         | None -> None
         | Some((j', mj, ej)) ->
             let (a, m) = mkPrefix pref target in
             Some((a, m,
                   Entails_W_Imp_Intro _S _I _G i j j' a pref mj m ej)))

  | _ -> (* eventually, a collect; for now only pick the first one *)
    fold_left_dep  None _I
                   (fun res infon pr_mem -> match res with
                      | None -> tryDerive _S _I _G pref target infon
                                (Entails_Hyp_Knowledge _S _I _G infon pr_mem)
                      | _ -> res) 