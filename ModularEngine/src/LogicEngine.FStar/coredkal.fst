(************************)
(* build:               *)
(* fstar coredkal.fst   *)
(************************)

module CoreDKAL

(********************)
(* Type definitions *)
(********************)

(** From types.fst **)
type principal = string

type typ =  
  | Infon : typ
  | Principal : typ
  | SubstrateUpdate : typ
  | SubstrateQuery : typ
  | Action : typ
  | Condition : typ
  | RuleT : typ
  | Evidence : typ
  | Boolean : typ
  | Int32 : typ
  | Double : typ
  | String : typ

type var = string * typ
  
type constant = 
  | True : constant
  | False : constant
  (* | Bool : bool -> constant *)
  | PrincipalConstant : principal -> constant
  | SubstrateConstant : object -> constant 

type relationInfon
type func




type substitution


type term (*= 
  | Var : var -> term
  | Const : constant -> term
  | Forall : var -> term -> term
  | App : func -> list term -> term
  (* | ... *)
*)
  


(** to be replaced **)
type query

type evidence =
  | EmptyEv : evidence
  | SigEv : evidence
  | MPEv : evidence
  | AndEv : evidence
  | AsInfonEv : evidence

type infon =
  | Variable : var -> infon
(*   | c  *)
  | Forall : var -> infon -> infon
  | Empty : infon
  | AsInfon : query -> infon
  | And : infon -> infon -> infon
  | Said : infon -> infon -> infon
(*                | And i j *)
  | Implies : infon -> infon -> infon
  
  
(** added here **)
type infostrate = list infon
type substrate = list query
type varDecl = list var
type prefix = list infon
(*
let said (p:term) (i:term) = App(Said, [p; i])
*)
  
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

val memSubstrate : q:query -> s:substrate -> option (Mem q s)
  (* Rk: using _S instead of s in the type declaration confuses F* *)
let memSubstrate q _S = mem q _S
  (* Rk: does not accept : let memSubstrate = mem which is essentially the same *)
  
val memInfostrate : i:infon -> is:infostrate -> option (Mem i is)
let memInfostrate i _I = mem i _I


(* Intuition:                                                 *)
(* MkPrefix [p1;...;pn] i (p1 said (p2 said ... (pn said i))) *)
type MkPrefix :: list infon => infon => infon => P =
  | MkPrefix_Nil : i:infon -> MkPrefix [] i i
  | MkPrefix_Cons : p:infon -> ps:list infon -> i:infon -> i':infon 
                   -> MkPrefix ps i i'
                   -> MkPrefix (p::ps) i (Said p i')

(* Intuition:                                                    *) 
(* mkPrefix [p1;...;pn] i = ( (p1 said (... pn said i)), proof ) *)
val mkPrefix: pref:prefix -> i:infon -> (i':infon * dummy:(MkPrefix pref i i'){pref=[] => i=i'})
let rec mkPrefix pref i =
  (match pref with
     | [] -> (i, MkPrefix_Nil i)
     | h::t -> let (j, m) = mkPrefix t i in
	             ((Said h j), MkPrefix_Cons h t i j m))				   


(***********************)
(* Inductive judgments *)
(***********************) 

(*type types :: varDecl => ? => typ => P =*)

type entails :: substrate => infostrate => varDecl => infon => P =
  | Entails_Emp : S:substrate -> I:infostrate -> G:varDecl
               -> pref: prefix
			   -> result: infon
			   -> MkPrefix pref Empty result
			   -> entails S I G result

  | Entails_Hyp_Knowledge : S:substrate -> I:infostrate -> G:varDecl
                          -> i:infon
						  -> Mem i I
						  -> entails S I G i
						  
  | Entails_Hyp_Substrate : S:substrate -> I:infostrate -> G:varDecl
                          -> q:query 
                          -> Mem q S  
						  (* Eventually, we get a signature from the DB about this fact *)
                          -> entails S I G (AsInfon q)

  (* Q: here order of G matters; make sure that's all right; I think so *)
  | Entails_Q_Intro : S:substrate -> I:infostrate -> G:varDecl
                    -> x:var -> i:infon -> i':infon
					-> result:infon
					-> pref:prefix
			        (*-> MkPrefix pref (Forall x i) result *) (* TODO?? *)
					-> MkPrefix pref i i'
			        -> entails S I (x::G) i'
					-> entails S I G result
				 
  | Entails_And_Intro : S:substrate -> I:infostrate -> G:varDecl
                     -> i:infon -> i':infon
                     -> j:infon -> j':infon 
                     -> result:infon
                     -> pref: prefix
                     -> MkPrefix pref (And i j) result
                     -> MkPrefix pref i i'
                     -> MkPrefix pref j j'
                     -> entails S I G i' 
                     -> entails S I G j' 
                     -> entails S I G result

  | Entails_And_Elim1 : S:substrate -> I:infostrate -> G:varDecl
     -> i : infon -> i': infon
     -> j : infon
     -> pref : list infon 
     -> hyp : infon
     -> MkPrefix pref (And i j) hyp
     -> MkPrefix pref i i'
     -> entails S I G hyp
     -> entails S I G i'
	 
  | Entails_W_Imp_Intro : S:substrate -> I:infostrate -> G:varDecl 
                      -> i:infon
                      -> j:infon -> j':infon 
                      -> result:infon
                      -> pref: list infon
                      -> MkPrefix pref (Implies i j) result
                      -> MkPrefix pref j j'
                      -> entails S I G j' 
                      -> entails S I G result

					  
(*
type Typing :: varDecl => term => typ => P =
   | Typing_var : g:varDecl -> x:var -> t:typ -> Mem (x,t) g -> Typing g (VarTerm x) t
*)
(* val subst: i:infon -> x:var -> u:term -> (i':infon * Subst i x u i') *)
(* let subst i x u = raise "todo" *)

type Subst :: infon => var => term => infon => P = 
   | Subst_admit : i:infon -> x:var -> t:term -> i':infon -> Subst i x t i'

val subst: i:infon -> x:var -> u:term -> (i':infon * Subst i x u i')
let subst i x u = raise "todo"

(************************)
(* Derivation algorithm *)
(************************) 

val doDerive: S:substrate -> I:infostrate -> G:varDecl 
           -> pref:prefix -> i:infon 
           -> option (i':infon * MkPrefix pref i i' * entails S I G i')
let rec doDerive _S _I _G pref inf = 
  let memInfostrate_inf = memInfostrate inf _I in
  match inf with 
  | Empty -> let (j, m) = mkPrefix pref inf in
		              (* if unable to make F* prove sth, put in a runtime check *)
			          (* if j = Empty then *)
                   Some((j, m, 
				         Entails_Emp _S _I _G pref j m))
				      (* else raise "stupid F*" *)

  | i when ((pref = []) && (not (memInfostrate_inf = None))) ->
      let (j, m) = mkPrefix pref inf in
	  (match memInfostrate_inf with
	     | None -> raise "assert false"
         | Some mis -> Some((j, m, Entails_Hyp_Knowledge _S _I _G i mis)))
		 
  | AsInfon q -> 
      (match pref with
         | [] -> let (j, m) = mkPrefix pref inf in
             (match memSubstrate q _S with 
                | None -> None
                | Some pf -> Some ((j, m, Entails_Hyp_Substrate _S _I _G q pf)))
         | _ -> None)

  | Forall x i ->
      (match doDerive _S _I (x::_G) pref i with
	     | None -> None
		 | Some((i', mi, ei)) -> 
             let (j, m) = mkPrefix pref inf in
	         Some((j, m, Entails_Q_Intro _S _I _G x i i' j pref (*m*) mi ei)))
	 
  | And i j ->
      (match (doDerive _S _I _G pref i, doDerive _S _I _G pref j) with
	     | (None, _) -> None
	     | (_, None) -> None
	     | (Some((i', mi, ei)), Some((j', mj, ej))) -> 
		      let (a, m) = mkPrefix pref inf in
		      Some((a, m,
                    Entails_And_Intro _S _I _G i i' j j' a pref m mi mj ei ej)))
					
  | Implies i j ->
      (match doDerive _S _I _G pref j with
	     | None -> None
         | Some((j', mj, ej)) ->
             let (a, m) = mkPrefix pref inf in
			 Some((a, m,
			       Entails_W_Imp_Intro _S _I _G i j j' a pref m mj ej)))
	  

      
