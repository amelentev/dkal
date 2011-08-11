module Subst
open Types
open Util

(* Axiomatization of sets of variables *)
logic function Empty : varset
logic function Singleton : var -> varset
logic function Union : varset -> varset -> varset
assume forall (v:var). (In v (Singleton v))
assume forall (l1:varset) (l2:varset) (v:var). 
        ((In v l1) || (In v l2)) => (In v (Union l1 l2))
assume forall (v:var). not(In v Empty)
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

type Subst :: term => substitution => term => E
type SubstList :: list term => substitution => list term => E

assume Subst_VarRepl: forall (x:var) (s:substitution).
        (In x (Domain s)) => ((Subst (Var x) s (Select s x)))
assume Subst_VarIgnore: forall (x:var) (s:substitution).
        (not (In x (Domain s))) => (Subst (Var x) s (Var x))
assume Subst_Const:  (forall (c:constant) (s:substitution). 
        (Subst (Const c) s (Const c)))
assume Subst_App: forall (f:func) (args:list term) (args': list term) (s:substitution).
        (SubstList args s args') =>  (Subst (App f args) s (App f args'))
assume Subst_forallT: forall (x:var) (t:term) (s:substitution) (t':term). 
        (not(In x (FreeVarsSubst s)) && Subst t s t') => 
           (Subst (ForallT x t) s (ForallT x t'))
assume Subst_forallT_Alpha: forall (x:var) (t:term) (s:substitution) (y:var) (t_yx:term) (t':term). 
        (not(In y (Union (FreeVarsSubst s) (FreeVars t))) &&
            (Subst t (Update EmptySubst x (Var y)) t_yx) &&
            (Subst t_yx s t'))
        => (Subst (ForallT x t) s (ForallT y t'))

assume SubstList_nil: forall (s:substitution). SubstList [] s []
assume SubstList_cons: forall (t:term) (t':term) (tl:list term) (tl':list term) (s:substitution). 
        (Subst t s t' && SubstList tl s tl')
    <=> (SubstList (t::tl) s (t'::tl'))


type Extends :: substitution => substitution => E
assume forall (s:substitution). Extends s s
assume forall (s1:substitution) (s2:substitution) (x:var) (t:term). 
          ((Extends s2 s1) && (Subst t s2 t) && (not(In x (Domain s2))))
        => Extends (Update s2 x t) s1


val emptySubst: unit -> s:substitution{s=EmptySubst}
  
val lookupVar : 
     s:substitution 
  -> x:var 
  -> o:option term{((o=None <=> (not (In x (Domain s)))) &&
                      (forall (t:term). (o=(Some t)) => 
   			  ((In x (Domain s)) && (t=(Select s x)))))}

val extendSubst : 
     s:substitution 
  -> x:var 
  -> t:term{Subst t s t && not(In x (Domain s))}
  -> s':substitution{s'=(Update s x t)}

val inDomain : 
     s:substitution 
  -> x:var 
  -> b:bool{b=true <=> In x (Domain s)}
   
val inFreevars : 
     s:substitution 
  -> x:var 
  -> b:bool{b=true <=> In x (FreeVarsSubst s)}

(* added *)
val freeVarsSubst : 
     s:substitution 
  -> f:varset{(f = (FreeVarsSubst s))}

val freeVars : 
     t:term 
  -> f:varset{(f = (FreeVars t))}


(* functions from builders.fs *)
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
val subst: i:term -> s:substitution -> i':term{Subst i s i'}
val substList : tl:list term -> s:substitution -> tl':list term{SubstList tl s tl'}
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
		let t_yx : (t_yx:term{((Subst t (Update EmptySubst x (Var y)) t_yx))}) =
          subst t ySubst in
		let t' : (t':term{(Subst t_yx s t')}) =
          (subst t_yx s) in
(*         let res : (res:term{Subst i s res}) = Types.ForallT y t' in  *)
        let res = Types.ForallT y t' in 
	let _ = Assume <(Subst i s res)> () in
		res
		
(*     | SubstrateQueryTerm t0 -> *)
(* 	    let res =  *)
(*           FStarTermOfITerm *)
(*             (TypeHeaders.substrateQueryTerm_apply t0   *)
(* 		       (\* not sure why I need the TypeHeaders. , TypeHeaders has been opened *\) *)
(* 		       (TranslationFromFStar.ISubstitutionOfFStarSubstitution s) ) *)
(* 		in let _ = Assume <(res = (Subst i s))> () in *)
(* 		res *)
	
(*     | SubstrateUpdateTerm t0 -> *)
(* 	    let res = *)
(*           FStarTermOfITerm  *)
(*             (TypeHeaders.substrateUpdateTerm_apply t0  *)
(* 		       (TranslationFromFStar.ISubstitutionOfFStarSubstitution s) ) *)
(* 		in let _ = Assume <(res = (Subst i s))> () in *)
(* 		res *)
	
(*     | ConcretizationEvidence t1 s1 -> raise "Evidences not handled" *)
(* 	  (\* ConcretizationEvidence (ConcretizationEvidence t1 s1) s *\) (\* correct?? *\) *)
(*       (\* Types.ConcretizationEvidence t1 (composeWith s s1) *\) *)

and substList ilist s = match ilist with
  | [] -> []
  | hd::tl -> 
      let tl' = substList tl s in 
      let hd' = subst hd s in
        Cons<term>  hd' tl' (* type inference is too eager here; need to provide explicit annotation on Cons *)
