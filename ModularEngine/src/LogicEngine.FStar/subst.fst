module Subst
open Types
open Util

(*********************************************************************************)
(* Specification *)
(*********************************************************************************)

(* Abstract representation of substitutions, useful for when we use external dictionaries *)
logic function Select     : substitution -> var -> term
assume Subst_sel1:forall (s:substitution) (x:var) (t:term).
                      (Select ((x,t)::s) x) = t
assume Subst_sel2:forall (s:substitution) (x:var) (y:var) (t:term).
                      (x <> y) => ((Select ((x,t)::s) y) = (Select s y))

(* Domain of a substitution *)
logic function Domain : substitution -> vars
assume Subst_dom_emp:(Domain []) = []
assume Subst_dom_upd:forall (s:substitution) (x:var) (t:term). 
                     Domain ((x,t)::s) = (x::(Domain s))

(* Axiomatization of substitution on (mono)terms *)
logic function Subst      : term -> substitution -> term               
logic function SubstList  : list term -> substitution -> list term 
assume SubstList0:forall (s:substitution). ((SubstList [] s)=[])
assume SubstList1:forall (t:term) (tl:list term) (s:substitution). 
                  (SubstList (t::tl) s) = ((Subst t s)::(SubstList tl s))
assume Subst_Var1:forall (x:var) (s:substitution). 
                  (In x (Domain s)) => ((Subst (Var x) s)=(Select s x))
assume Subst_Var2:forall (x:var) (s:substitution). 
                  (not (In x (Domain s))) => ((Subst (Var x) s)=(Var x))
assume Subst_Cnst:forall (c:constant) (s:substitution). 
                  (Subst (Const c) s)=(Const c)
assume Subst_Appl:forall (f:func) (args:list term) (s:substitution). 
                  ((Subst (App f args) s)=(App f (SubstList args s)))
assume Subst_QyTm:forall (q:ISubstrateQueryTerm) (s:substitution). 
                  ((Subst (SubstrateQueryTerm q) s)=(SubstrateQueryTerm ({n=(Subst q.n s); 
                                                                          low=(Subst q.low s); 
                                                                          hi=(Subst q.hi s)})))

(* Substitution on polyterms, as a parial function to avoid capture *)
logic function FreeVars : term -> vars
assume FreeVars_Var  : forall (x:var). (FreeVars(Var x) = [x])
assume FreeVars_Const: forall (c:constant). (FreeVars(Const c) = [])
assume FreeVars_App1 : forall (f:func). (FreeVars(App f []) = [])
assume FreeVars_App2 : forall (f:func) (t:term) (tl:list term).
                       ((FreeVars(App f (t::tl))) = (Append (FreeVars t) (FreeVars(App f tl))))
assume FreeVars_SubQ : forall (s:ISubstrateQueryTerm). 
                       ((FreeVars (SubstrateQueryTerm s))) = (Append (FreeVars (s.n)) (Append (FreeVars (s.low)) (FreeVars (s.hi))))
logic function FreeVarsSubst : substitution -> vars 
assume FreeVarsSubst_Emp: FreeVarsSubst [] = []
assume FreeVarsSubst_Upd: forall (s:substitution) (x:var) (v:term).
                          ((FreeVarsSubst ((x,v)::s)) = (Union [x] (Union (FreeVars v) (FreeVarsSubst s))))

logic function PolySubst : polyterm -> substitution -> polyterm
assume PolySubst_Mono: forall (s:substitution) (body:term). 
                       ((PolySubst (MonoTerm body) s)=(MonoTerm (Subst body s)))
assume PolySubst_All : forall (s:substitution) (xs:vars) (body:term). 
                       Disjoint (FreeVarsSubst s) xs =>
                          ((PolySubst (ForallT xs body) s)=(ForallT xs (Subst body s)))
assume PolySubst_Just: forall (s:substitution) (p:term) (i:polyterm) (d:term). 
                          ((PolySubst (JustifiedPoly p i d) s)=(JustifiedPoly 
                                                                  (Subst p s) 
                                                                  (PolySubst i s)
                                                                  (Subst d s)))
(* Building substitutions *)
logic function MkSubst: vars -> list term -> substitution
assume MkSubst_nil :(MkSubst [] []) = []
assume MkSubst_cons:forall (x:var) (t:term) (xs:vars) (ts:list term). 
                    (MkSubst (x::xs) (t::ts)) = ((x,t)::(MkSubst xs ts))

(* Abstract predicate: TODO, remove this, or else refine it. *)
type Extends :: substitution => substitution => E

(*********************************************************************************)
(* Implementation *)
(*********************************************************************************)
val emptySubst: unit -> s:substitution{s=[]}
let emptySubst b = []

val subst_apply : substitution -> var -> term
let subst_apply s v = match assoc v s with 
  | None -> Var v
  | Some t -> t

val mkSubst: xs:vars -> ts:list term -> s:substitution{s=(MkSubst xs ts)}
let rec mkSubst xs ts = match xs, ts with 
  | [], [] -> []
  | (x::xtl), (t::ttl) -> (x,t)::(mkSubst xtl ttl)

val lookupVar: s:substitution -> x:var -> r:option term{(((r=None) => not (In x (Domain s))) &&
                                                         ((r<>None) => (In x (Domain s)) && (r=(Some (Select s x)))))}
let rec lookupVar s x = match s with 
  | [] -> None
  | (x',t)::s' -> 
      if x=x' 
      then 
        (assert ((Domain s) = (x::(Domain s')));
         assert (In x (x::(Domain s')));
         assert (In x (Domain s));
         assert ((Select s x) = t);
         Some t) 
      else 
        (assert ((Domain s) = (x'::Domain s'));
         assert ((not(In x (Domain s'))) => (not(In x (Domain s))));
         assert (In x (Domain s') => In x (Domain s));
         assert ((Select s x) = (Select s' x));
         lookupVar s' x)

val intFunc: string -> string -> (int -> int -> int) -> (substitution -> term)
let intFunc x y f =
  let makeIntVar (n:string) : var = {name = n; typ = Int32} in
  let getInt (t:term) : int =
    match t with
	  | Const c -> (match c with Int i -> i | _ -> raise "plusFunc: not an int")
	  | _ -> raise "plusFunc: not a const" in
  fun (s:substitution) ->
    match (lookupVar s (makeIntVar x), lookupVar s (makeIntVar y)) with
	  | Some t1, Some t2 -> Const (Int (f (getInt t1) (getInt t2)))
	  | _ -> raise "cannot find var"

val extendSubst : s:substitution -> x:var
               -> t:term(*{(Subst t s)=t && not(In x (Domain s))}*) (* TODO *)
               -> s':substitution{s'=((x,t)::s)}
let extendSubst s x t = (x,t)::s

val domain : s:substitution -> v:vars{(v = (Domain s))}
let rec domain s = match s with 
  | [] -> []
  | (x,t)::tl -> 
      let dtl = domain tl in 
        (* assert (Domain tl = dtl); *)
        (* assert (Domain s = (Union [x] dtl)); *)
        (* assert ((Union [x] dtl) = (Append [x] dtl)); *)
        (* assert ((Append [x] dtl) = (x::dtl)); *)
        x::dtl

val inDomain : s:substitution -> x:var
            -> b:bool{b=true <=> In x (Domain s)}
let inDomain s x = contains x (domain s)

val freeVars : t:term -> f:vars{(f = (FreeVars t))}
let rec freeVars t = match t with
  | Var x -> [x]
  | Const c -> []
  | App f [] -> []
  | App f (h::t) -> append (freeVars h) (freeVars (App f t))
  | SubstrateQueryTerm s -> append (freeVars s.n) (append (freeVars s.low) (freeVars s.hi))
  | SubstrateUpdateTerm s -> raise "NYI: substrateUpdateTerm_Vars s"
  | EvalTerm _ _ -> []

val freeVarsSubst: s:substitution -> f:vars{(f = (FreeVarsSubst s))}
let rec freeVarsSubst s = match s with
  | [] -> []
  | (x,v)::tl -> 
      let fvtl = append (freeVars v) (freeVarsSubst tl) in 
        assert (Union [x] fvtl) = (Append [x] fvtl);
        assert (Append [x] fvtl) = (x::fvtl);
        x::fvtl
    
val inFreevars : s:substitution -> x:var
              -> b:bool{b=true <=> In x (FreeVarsSubst s)}
let inFreevars s x = contains x (freeVarsSubst s)

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
val substQuery :   q:ISubstrateQueryTerm 
                -> s:substitution
                -> r:ISubstrateQueryTerm{r=({n=(Subst q.n s); low=(Subst q.low s); hi=(Subst q.hi s)})}
let rec subst i s = match i with
  | Var y -> (match lookupVar s y with
                | None -> Var y
                | Some t -> t)
  | Const c -> Const c
  | App f tl -> App f (substList tl s)
  | SubstrateQueryTerm q -> SubstrateQueryTerm({n=(subst q.n s); low=(subst q.low s); hi=(subst q.hi s)})
  | SubstrateUpdateTerm _ -> raise "NYI: Substrate updates"
  | EvalTerm _ evalFunc -> 
    let t = evalFunc s in
	(assume ((Subst i s)=t); t)

and substList ilist s = match ilist with
  | [] -> []
  | hd::tl ->
      let tl' = substList tl s in
      let hd' = subst hd s in
        Cons<term> hd' tl' (* type inference is too eager here and infers a refined instantiation;
                              need to provide explicit annotation on Cons *)

and substQuery q s = {n=(subst q.n s); low=(subst q.low s); hi=(subst q.hi s)}

val polysubst: p1:polyterm -> s:substitution -> p2:polyterm{(PolySubst p1 s)=p2}
let rec polysubst p1 s = 
  match p1 with 
    | MonoTerm i -> MonoTerm (subst i s)
    | ForallT xs i -> 
        if check_disjoint (freeVarsSubst s) xs
        then ForallT xs (subst i s)
        else raise "polysubst failed: name capture"
    | JustifiedPoly p i d -> 
        JustifiedPoly (subst p s) (polysubst i s) (subst d s)
