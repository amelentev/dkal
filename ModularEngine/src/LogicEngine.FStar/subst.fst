module Subst
open Types
open Util


val subst_apply : substitution -> var -> term
let subst_apply s v = match assoc v s with 
  | None -> Var v
  | Some t -> t

(* Axiomatization of sets of variables *)
logic function Empty : vars
logic function Singleton : var -> vars
logic function Union : vars -> vars -> vars
assume forall (v:var). (In v (Singleton v))
assume forall (l1:vars) (l2:vars) (v:var). 
        ((In v l1) || (In v l2)) <=> (In v (Union l1 l2))
assume forall (v:var). not(In v Empty)

(* Axiomatization of substitutions *)
logic function Select : substitution -> var -> term
logic function Update : substitution -> var -> term -> substitution
assume forall (s:substitution) (x:var) (t:term). (Select (Update s x t) x) = t
assume forall (s:substitution) (x:var) (y:var) (t:term). (x <> y) => ((Select (Update s x t) y) = (Select s y))
assume forall (s:substitution) (x:var) (t:term). (Update s x t) = ((x,t)::s)

(* Domain of a substitution will be restricted to be a subset of the unification variables *)
logic function Domain : substitution -> vars
logic function EmptySubst : substitution
assume forall (s:substitution) (x:var) (t:term). Domain (Update s x t) = (Union (Domain s) (Singleton x))
assume (Domain EmptySubst) = Empty
assume (EmptySubst = []) 

(* freevars of both the domain and the range *)
(* the set of variables is represented as a list with possible double occurrences
   but logically it is still a set, not a multiset *)
logic function FreeVarsPoly : polyterm -> vars
logic function FreeVars : term -> vars
logic function FreeVarsSubst : substitution -> vars
logic function FreeVarsSubst_aux : substitution -> vars -> vars 
assume FreeVars_Var: forall (x:var). (FreeVars(Var x) = [x])
assume FreeVars_Const: forall (c:constant). (FreeVars(Const c) = [])
assume FreeVars_App_nil: forall (f:func). (FreeVars(App f []) = [])
assume FreeVars_App_cons: forall (f:func) (t:term) (tl:list term).
        ((FreeVars(App f (t::tl))) = (Append (FreeVars t) (FreeVars(App f tl))))
assume FreeVarsSubst_aux_nil: forall (s:substitution) (v:vars).
        (v = []) => (FreeVarsSubst_aux s v = [])
assume FreeVarsSubst_aux_cons: forall (s:substitution) (x:var) (v:vars).
        ((FreeVarsSubst_aux s (x::v)) = (Append (FreeVars (Select s x)) (FreeVarsSubst_aux s v)))
assume FreeVarSubst_all: forall (s:substitution).
        ((FreeVarsSubst s) = (FreeVarsSubst_aux s (Domain s)))

(* Axiomatization of substitution itself *)
logic function SubstQuery : ISubstrateQueryTerm -> substitution -> ISubstrateQueryTerm (* substitution of all variables in a SubstrateQueryTerm *)
logic function Subst : term -> substitution -> term (* substitution of all variables in a term *)
logic function SubstList : list term -> substitution -> list term (* substitution of all variables in a list of terms *)
assume Subst_VarRepl: forall (x:var) (s:substitution).
        (In x (Domain s)) => ((Subst (Var x) s)=(Select s x))
assume Subst_VarIgnore: forall (x:var) (s:substitution).
        (not (In x (Domain s))) => ((Subst (Var x) s)=(Var x))
assume Subst_Const: forall (c:constant) (s:substitution). 
        (Subst (Const c) s)=(Const c)
assume Subst_App: forall (f:func) (args:list term) (s:substitution).
          ((Subst (App f args) s)=(App f (SubstList args s)))
assume Subst_QueryTerm: forall (q:ISubstrateQueryTerm) (s:substitution). 
          ((Subst (SubstrateQueryTerm q) s)=(SubstrateQueryTerm (SubstQuery q s)))
assume SubstList_nil: forall (s:substitution). ((SubstList [] s)=[])
assume SubstList_cons: forall (t:term) (tl:list term) (s:substitution). 
           (SubstList (t::tl) s) = ((Subst t s)::(SubstList tl s))

logic function PolySubst : polyterm -> substitution -> polyterm
(* substitution of all variables in a polyterm *)
assume (forall (s:substitution) (xs:vars) (body:term). Disjoint (FreeVarsSubst s) xs =>
    ((PolySubst (ForallT xs body) s)=(ForallT xs (Subst body s))))
assume (forall (s:substitution) (body:term). 
    ((PolySubst (MonoTerm body) s)=(MonoTerm (Subst body s))))

val polysubst: polyterm -> substitution -> polyterm

(* Building substitutions *)
logic function MkSubst: vars -> list term -> substitution
assume (MkSubst [] [] = EmptySubst)
assume (forall (x:var) (t:term) (xs:vars) (ts:list term). (MkSubst (x::xs) (t::ts)) = (Update (MkSubst xs ts) x t))
(* solution 1 (preferred but the F# file does not compile, see utilities.fs) *)

val mkSubst: xs:vars -> ts:list term -> s:substitution{s=(MkSubst xs ts)}
let mkSubst xs ts = 
  let s = zip xs ts in 
    assume (s=(MkSubst xs ts));
    s

type Extends :: substitution => substitution => E
assume forall (s:substitution). Extends s s
assume forall (s1:substitution) (s2:substitution) (x:var) (t:term). 
          ((Extends s2 s1) && ((Subst t s2)=t) && (not(In x (Domain s2))))
        => Extends (Update s2 x t) s1
(* what if x appears free in some elements of the image of s2?? *)

val substQuery: q:ISubstrateQueryTerm -> s:substitution -> q':ISubstrateQueryTerm{(SubstQuery q s)=q'}

val emptySubst: unit -> s:substitution{s=EmptySubst}
let emptySubst b = []
 

(* TODO: dummy, need to get rid of it eventually *)
val extends : s2:substitution
           -> s1:substitution 
           -> b:bool{((b=true) <=> (Extends s2 s1))}
let extends s2 s1 = raise "NYI"

type PostLookup :: _ = (fun (s:substitution) (x:var) (o:option term) =>
    ((o=None <=> (not (In x (Domain s)))) &&
                      (forall (t:term). (o=(Some t)) =>
   			  ((In x (Domain s)) && (t=(Select s x))))))
val lookupVar :
     s:substitution
  -> x:var
  -> o:option term{PostLookup s x o}
let lookupVar s x =
  let o = assoc x s in
    assume (PostLookup s x o);
    o

val extendSubst :
     s:substitution
  -> x:var
  -> t:term(*{(Subst t s)=t && not(In x (Domain s))}*) (* TODO *)
  -> s':substitution{s'=(Update s x t)}
let extendSubst s x t = (x,t)::s

val domain : s:substitution -> v:vars{(v = (Domain s))}
let domain s =
  let v = map (fun (x,_) -> x)  s in
    assume (v=(Domain s));
    v

val inDomain :
     s:substitution
  -> x:var
  -> b:bool{b=true <=> In x (Domain s)}
let inDomain s x =
  contains x (domain s)

val substrateQueryTerm_Vars :
  s:ISubstrateQueryTerm -> f:vars{(f = (FreeVars (SubstrateQueryTerm s)))}

val substrateUpdateTerm_Vars :
  s:ISubstrateUpdateTerm -> f:vars{(f = (FreeVars (SubstrateUpdateTerm s)))}

val freeVars :
     t:term
  -> f:vars{(f = (FreeVars t))}
let rec freeVars t = match t with
  | Var x -> [x]
  | Const c -> []
  | App f [] -> []
  | App f (h::t) -> append (freeVars h) (freeVars (App f t))
  | SubstrateQueryTerm s -> substrateQueryTerm_Vars s
  | SubstrateUpdateTerm s -> substrateUpdateTerm_Vars s

val freeVarsSubst_aux :
     s:substitution -> v:vars -> f:vars{(f = (FreeVarsSubst_aux s v))}
let rec freeVarsSubst_aux s v =
  match v with
  | [] -> []
  | x::tl -> (match lookupVar s x with
             | None -> failwith "impos"
			 | Some(t) -> append (freeVars t) (freeVarsSubst_aux s tl))

val freeVarsSubst : (* maybe a way to not make it extern? *)
     s:substitution
  -> f:vars{(f = (FreeVarsSubst s))}
let freeVarsSubst s = freeVarsSubst_aux s (domain s)
    
val inFreevars :
     s:substitution
  -> x:var
  -> b:bool{b=true <=> In x (FreeVarsSubst s)}
let inFreevars s x =
  contains x (freeVarsSubst s)

let genId =
  let ctr = newref 0 in
    fun (x:unit) ->
      (ctr := !ctr + 1);
      !ctr

val freshVar : ty:typ -> x:var{x.typ=ty}
let freshVar ty = { typ = ty; name = (Concat "__fresh" (string_of_any (genId())))}

type varTyEq :: _ = (fun (x:var) (y:var) => (x.typ=y.typ))
type varsTyEq :: _ = (fun (xs:vars) (ys:vars) => (ZipE var var varTyEq xs ys))
val freshVars: xs:vars -> (ys:vars * varsTyEq xs ys)
let rec freshVars xs = match xs with
  | [] -> ([], ZipE_Nil<var, var, varTyEq>)
  | x::xtl ->
      let y = freshVar x.typ in
      let ytl, ztl = freshVars xtl in
        ((y::ytl), ZipE_Cons<var,var,varTyEq> xtl ytl x y ztl)

  
(* see term_apply in term.fst *)
(* NS: Would be nice to have this be the only definition of subst
       and generate the logic function axioms from this code. *)
val subst: i:term -> s:substitution -> i':term{(Subst i s)=i'}
val substList : tl:list term -> s:substitution -> tl':list term{(SubstList tl s)=tl'}
let rec subst i s = match i with
  | Var y -> (match lookupVar s y with
                | None -> Var y
                | Some t -> t)
  | Const c -> Const c
  | App f tl -> App f (substList tl s)
  | SubstrateQueryTerm q ->
     SubstrateQueryTerm(substQuery q s)
	
and substList ilist s = match ilist with
  | [] -> []
  | hd::tl ->
      let tl' = substList tl s in
      let hd' = subst hd s in
        Cons<term> hd' tl' (* type inference is too eager here and infers a refined instantiation;
                              need to provide explicit annotation on Cons *)
