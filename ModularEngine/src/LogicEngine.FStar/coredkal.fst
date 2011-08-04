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

val func_typing : func -> option ((list typ) * typ)
let func_typing = function
 (* could also take code from Ast.Infon/Primitives.fs *)
  | SeqRule -> None (*[RuleT; RuleT], RuleT*) 
                    (* any number of args possible *)
  | EmptyRule -> Some([], RuleT)
  | Rule -> Some([Condition; Action], RuleT)
  | RuleOnce -> Some([Condition; Action], RuleT)
  | SeqCondition -> None (*[Condition; Condition], Condition*) 
                         (* any number of args possible *)
  | EmptyCondition -> Some([], Condition)
  | WireCondition -> Some([Infon; Principal], Condition)
  | KnownCondition -> Some([Infon], Condition)
  | SeqAction -> None (*[Action; Action], Action*)
                      (* any number of args possible *)
  | EmptyAction -> Some([], Action)
  | Send -> Some([Principal; Infon], Action)
  | JustifiedSend -> Some([Principal; Infon], Action)
  | JustifiedSay -> Some([Principal; Infon], Action)
  | Learn -> Some([Infon], Action)
  | Forget -> Some([Infon], Action)
  | Install -> Some([RuleT], Action)
  | Uninstall -> Some([RuleT], Action)
  | Apply -> Some([SubstrateUpdate], Action)
  | Drop -> Some([Infon], Action)
  | EmptyInfon -> Some([], Infon)
  | AsInfon -> Some([SubstrateQuery], Infon)
  | AndInfon -> None (* Some([Infon; Infon], Infon *)
                     (* any number of args possible *)
  | ImpliesInfon -> Some([Infon; Infon], Infon)
  | SaidInfon -> Some([Principal; Infon], Infon)
  | JustifiedInfon -> Some([Infon; Evidence], Infon)
  | EmptyEvidence -> Some([], Evidence)
  | SignatureEvidence -> Some([Principal; Infon; Int32], Evidence)
  | ModusPonensEvidence -> Some([Evidence; Evidence], Evidence)
  | AndEvidence -> None (*[Evidence; Evidence], Evidence*)
                        (* any number of args possible *)
  | AsInfonEvidence -> Some([SubstrateQuery], Evidence)
  | RelationInfon r -> Some(r.argsType, r.retType)
				 
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

(* could do a cast in an external file *)
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
  | Types_AppEmptyInfon : G:varDecl -> types G (App EmptyInfon []) Infon
  | Types_AppSaidInfon : G:varDecl -> p:term -> i:term
                         -> types G p Principal
						 -> types G i Infon
						 -> types G (App SaidInfon [p; i]) Infon
  | Types_AppAsInfon : G:varDecl -> q:ISubstrateQueryTerm
                       -> types G (App AsInfon [(SubstrateQueryTerm q)]) Infon
  | Types_AppAndInfon : G:varDecl -> i:term -> j:term (* TODO: extend to n *)
                     -> types G i Infon
					 -> types G j Infon 
					 -> types G (App AndInfon [i; j]) Infon
  | Types_AppImpliesInfon : G:varDecl -> i:term -> j:term
                     -> types G i Infon
					 -> types G j Infon 
					 -> types G (App ImpliesInfon [i; j]) Infon
  | Types_AppJustifiedInfon : G:varDecl -> i:term -> e:term 
                            -> types G i Infon
							-> types G e Evidence
							-> types G (App JustifiedInfon [i; e]) Infon
  | Types_AppRelationInfon : G:varDecl -> r:relationInfon (* no type check here *)
                           -> l:list term
						 (* TODO: need to typecheck all the subterms! *)
						   -> types G (App (RelationInfon r) l) Infon

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
  (* TODO: do something more general for App? *)
  | Subst_App0 : f:func -> x:var -> u:term 
               -> Subst (App f []) x u (App f [])
  | Subst_App1 : f:func -> x:var -> u:term 
               -> i:term -> i':term
			   -> Subst i x u i'
               -> Subst (App f [i]) x u (App f [i'])
  | Subst_App2 : f:func -> x:var -> u:term 
               -> i:term -> i':term
               -> j:term -> j':term
			   -> Subst i x u i'
			   -> Subst j x u j'
               -> Subst (App f [i; j]) x u (App f [i'; j'])
  (* try with n *)
  (*| Subst_Appn : f:func -> x:var -> u:term
               -> ilist : term list 
			   -> ilist': term list{}
			   -> Forall i in ilist, Subst i x u i'
			   -> ... *)
  (* Is this really true? Any variable ever in the ISubstrateQueryTerm? *)
  | Subst_SubstrateQueryTerm : q:ISubstrateQueryTerm -> x:var -> u:term
                     -> Subst (SubstrateQueryTerm q) x u (SubstrateQueryTerm q)
						   
						   
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
                   -> Subst i v u i'
				   -> MkPrefix pref i' result
				   -> entails S I G hyp
				   -> types G u v.typ
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
				   -> MkPrefix pref i' result
                   -> entails S I G hyp
				   -> types G u v.typ
                   -> Subst i v u i'
				   -> entails S I G result
				 
  | Entails_And_Intro : S:substrate -> I:infostrate -> G:varDecl
                     -> i:term -> i':term
                     -> j:term -> j':term 
                     -> result:term
                     -> pref: prefix
                     -> MkPrefix pref (App AndInfon [i; j]) result
                     -> MkPrefix pref i i'
                     -> MkPrefix pref j j'
                     -> entails S I G i' 
                     -> entails S I G j' 
                     -> entails S I G result

  | Entails_And_Elim1 : S:substrate -> I:infostrate -> G:varDecl
                     -> i : term -> i': term
                     -> j : term
                     -> pref : list term 
                     -> hyp : term
                     -> MkPrefix pref (App AndInfon [i; j]) hyp
                     -> MkPrefix pref i i'
                     -> entails S I G hyp
                     -> entails S I G i'
	 
  | Entails_And_Elim2 : S:substrate -> I:infostrate -> G:varDecl
                     -> i : term
                     -> j : term -> j': term
                     -> pref : list term 
                     -> hyp : term
                     -> MkPrefix pref (App AndInfon [i; j]) hyp
                     -> MkPrefix pref j j'
                     -> entails S I G hyp
                     -> entails S I G j'
	 
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
                     -> i : term -> i':term
                     -> j : term -> j':term
                     -> pref : list term 
                     -> hyp : term
                     -> MkPrefix pref i i'
                     -> MkPrefix pref (App ImpliesInfon [i; j]) hyp
                     -> MkPrefix pref i j'
					 -> entails S I G i'
                     -> entails S I G hyp
                     -> entails S I G j'

(*************************)
(* Derivation algorithms *)
(*************************) 

val doType: g:varDecl -> t:term -> option(ty:typ * (types g t ty))
(* val doTypeList : g:varDecl -> terms:list term -> typs:list typ -> option (Zip term typ (types G)) *)
(* val farg_typing: f:func -> list typ *)
(* let farg_typing = function  *)
(*   | EmptyInfon -> [] *)


(* let rec doTypeList g terms typs =  *)
(*   zip_p<term, typ, (types G)> (fun (tm:term) (ty:typ) ->  *)
(*                                  match doType g tm with  *)
(*                                    | Some (ty', proof) when ty=ty' -> proof *)
(*                                    | _ -> None) terms typs *)
(* use map_p for variable arguments for AndInfon etc. *)

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
  | App EmptyInfon [] -> Some(Infon, Types_AppEmptyInfon g)
  | App AsInfon [(SubstrateQueryTerm q)] ->
      Some((Infon, Types_AppAsInfon g q))
  | App SaidInfon [p; i] ->
      (match (doType g p, doType g i) with
	     | (Some((Principal, prp)), Some((Infon, pri))) ->
		      Some((Infon, Types_AppSaidInfon g p i prp pri))
		 | _ -> None)
  | App AndInfon [i; j] ->
      (match (doType g i, doType g j) with
	     | (Some((Infon, pri)), Some((Infon, prj))) ->
		      Some((Infon, Types_AppAndInfon g i j pri prj))
		 | _ -> None)
  | App ImpliesInfon [i; j] ->
      (match (doType g i, doType g j) with
	     | (Some((Infon, pri)), Some((Infon, prj))) ->
		      Some((Infon, Types_AppImpliesInfon g i j pri prj))
		 | _ -> None)
  | App JustifiedInfon [i; e] ->
      (match (doType g i, doType g e) with
	     | (Some((Infon, pri)), Some((Evidence, pre))) ->
		      Some((Infon, Types_AppJustifiedInfon g i e pri pre))
		 | _ -> None)
  | App (RelationInfon r) l -> 
       Some((Infon, Types_AppRelationInfon g r l))
  | _ -> None

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
    | App f [] -> (App f [], Subst_App0 f x u)
	| App f [i] -> let (i', pri) = subst i x u in
	                 App f [i'], Subst_App1 f x u i i' pri
    | App f [i; j] -> let (i', pri) = subst i x u in
	                  let (j', prj) = subst j x u in
	                    App f [i'; j'], Subst_App2 f x u i i' j j' pri prj
	| SubstrateQueryTerm q ->
	    SubstrateQueryTerm q, Subst_SubstrateQueryTerm q x u

val doDerive: S:substrate -> I:infostrate -> G:varDecl
             -> pref:prefix -> target:term
			 -> option (target':term *
                        MkPrefix pref target target' * 
						entails S I G target')
val tryDerive: S:substrate -> I:infostrate -> G:varDecl
             -> pref:prefix -> target:term
             -> inf:term
			 -> option (target':term *
                        MkPrefix pref target target' * 
						entails S I G target')
let rec doDerive _S _I _G pref target = 
  let memInfostrate_target = memInfostrate target _I in
  match target with 
  | App(EmptyInfon, [])-> let (j, m) = mkPrefix pref target in
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

  | App(AsInfon, [SubstrateQueryTerm q]) ->  
      (match pref with 
         | [] -> let (j, m) = mkPrefix pref target in 
             (match check_substrate _S q with  
                | false -> None 
                | true -> Some ((j, m, Entails_Hyp_Substrate _S _I _G q))) 
         | _ -> None) 

  | ForallT x i ->
      (match doDerive _S _I (x::_G) pref i with
	     | None -> None
		 | Some((i', mi, ei)) -> 
             let (j, m) = mkPrefix pref target in
	         Some((j, m, Entails_Q_Intro _S _I _G x i i' j pref m mi ei)))
	 
  | App(AndInfon, [i; j]) -> (* TODO: extend to n variables *)
      (match (doDerive _S _I _G pref i, doDerive _S _I _G pref j) with
	     | (None, _) -> None
	     | (_, None) -> None
	     | (Some((i', mi, ei)), Some((j', mj, ej))) -> 
		      let (a, m) = mkPrefix pref target in
		      Some((a, m,
                    Entails_And_Intro _S _I _G i i' j j' a pref m mi mj ei ej)))
					
  | App(ImpliesInfon, [i; j]) ->
      (match doDerive _S _I _G pref j with
	     | None -> None
         | Some((j', mj, ej)) ->
             let (a, m) = mkPrefix pref target in
			 Some((a, m,
			       Entails_W_Imp_Intro _S _I _G i j j' a pref mj m ej)))

  (*| _ -> tryDerive *)
	  

      
