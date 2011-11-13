module Typing
open TypeHeaders
open Types
open Util

(* 1. Dynamic type tests from .NET *)
type TypeOf :: constant => typ => E
(*val check_object_typ : x:object -> t:typ -> b:bool{b=true => TypeOf x t}*) (* never used *)

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

val containsVar : g:vars -> x:var -> bool
let containsVar g x = 
  List_exists (fun (y:var) ->
                 let yn:string = y.name in 
                 let xn:string = x.name in 
                   ((yn=xn) : bool)) g

(* NS: Can we write this as a logic function? *)
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

(* Well-formedness of an environment of variables *)
(* (a list of var = a list of {name; typ} ) *)
(* i.e., no variable appears several times *)
(* TODO *)
(* JBJ: this well-foundedness condition checks that the same pair *)
(* (variable, typ) does not appear twice, not that the *)
(* same variable does not appear twice. See def of In in prims.fst *)
type wfG :: vars => P = 
  | WFG_Empty : wfG []
  | WFG_Cons : 
       x:var 
    -> g:vars{not (In x g)}
    -> wfG g 
    -> wfG (x::g)

val decideWFG : g:vars -> option (wfG g)
let rec decideWFG g = match g with 
  | [] -> Some (WFG_Empty)
  | x::tl -> 
      if contains x tl 
      then None
      else match decideWFG tl with 
        | None -> None
        | Some pf -> Some (WFG_Cons x tl pf)

type typing :: vars => term => typ => P =
  | Typing_Var : 
       G:vars
    -> v:var
    -> Mem v G
    -> typing G (Var v) v.typ

  | Typing_ConstTrueT : 
       G:vars 
    -> typing G (Const TrueT) Boolean

  | Typing_ConstFalseT : 
       G:vars 
    -> typing G (Const FalseT) Boolean

  | Typing_ConstPrincipalConstant : 
       G:vars 
    -> p:principal
    -> typing G (Const (PrincipalConstant p)) Principal

  | Typing_ConstSubstrateConstant : 
       G:vars 
    -> t:typ 
    -> o:constant{TypeOf o t}
    -> typing G (Const (SubstrateConstant o)) t

  | Typing_SubstrateQueryTerm : 
       G:vars 
    -> q:ISubstrateQueryTerm 
    -> typing G (SubstrateQueryTerm q) SubstrateQuery

  | Typing_App : 
       G:vars 
    -> f:func
    -> args:list term
    -> formal_ts:list typ
    -> result_t:typ
    -> FuncTyping f formal_ts result_t
    -> ZipP term typ (typing G) args formal_ts
    -> typing G (App f args) result_t
    
val constantTyping: g:vars -> c:constant -> (ty:typ * typing g (Const c) ty) 
let constantTyping g c = match c with 
  | TrueT -> (Boolean, Typing_ConstTrueT g)
  | FalseT -> (Boolean, Typing_ConstFalseT g)
  | SubstrateConstant _ -> raise "NYI: SubstrateConstant typing"
  | PrincipalConstant p -> (Principal, Typing_ConstPrincipalConstant g p)

(* raise type error *)
let terr msg x = raise (Concat "Typing failure: " (Concat msg (string_of_any x)))

val doTyping: g:vars -> t:term -> (ty:typ * (typing g t ty))
let rec doTyping g = function
  | Var v -> (match mem v g with 
                | None -> terr "Variable not found: " (string_of_any v)
                | Some m -> (v.typ, Typing_Var g v m))
      
  | Const c -> constantTyping g c

  | SubstrateQueryTerm q -> SubstrateQuery, Typing_SubstrateQueryTerm g q

  | SubstrateUpdateTerm _ -> raise "NYI: Typing SubstrateUpdateTerm"
      
  | App f args -> 
      let formal_ts, przip = map_p (doTyping g) args in 
        match funcTyping f formal_ts with
          | None -> terr "Function typing failed: " f
          | Some ((result_t, ftyping)) -> 
              (result_t, Typing_App g f args formal_ts result_t ftyping przip)

val checkTyping : g:vars -> t:term -> ty:typ -> option (typing g t ty)
let checkTyping g t ty =
  let ty', pf = doTyping g t in 
    if ty=ty' then Some pf else None

val doTypingList : g:vars -> ts:list term -> xs:vars -> option (ZipP term var (fun (i:term) (x:var) => typing g i x.typ) ts xs)
let doTypingList g ts xs = zip_p<term, var, (fun (i:term) (x:var) => typing g i x.typ)> (fun t x -> checkTyping g t x.typ) ts xs

type polytyping :: vars => polyterm => P = 
  | PolyTyping_Mono : 
       G:vars 
    -> i:term
    -> wfG G
    -> typing G i Infon
    -> polytyping G (MonoTerm i) 

  | PolyTyping_ForallT : 
       G:vars 
    -> xs:vars
    -> i:term
    -> wfG (lconcat xs G)
    -> typing (lconcat xs G) i Infon
    -> polytyping G (ForallT xs i)

  | PolyTyping_Justified : 
       G:vars
    -> p:term
    -> i:polyterm
    -> d:term
    -> wfG G
    -> typing G p Principal
    -> polytyping G i 
    -> typing G d BytesT
    -> polytyping G (JustifiedPoly p i d)
    
val doPolyTyping: g:vars -> p:polyterm -> Partial (polytyping g p)
let rec doPolyTyping g p = match p with
  | MonoTerm t -> 
      (match decideWFG g with 
         | None -> terr "ill-formed environment" (string_of_any g)
         | Some wfg -> 
             (match doTyping g t with 
                | Infon, pf -> MkPartial (PolyTyping_Mono g t wfg pf)
                | ty, _ -> terr "Expected Infon type, got " (string_of_any ty)))
               
  | ForallT xs t -> 
      let g' = concat xs g in
        (match decideWFG g' with 
           | None -> terr "PolyTerms must bind distinct names. Repeated names: " xs
           | Some wf -> 
               match doTyping g' t with 
                 | Infon, pf -> MkPartial (PolyTyping_ForallT g xs t wf pf)
                 | ty, _ -> terr "Expected Infon type, got " (string_of_any ty))
          
  | JustifiedPoly q i d -> 
      match decideWFG g with 
        | None -> terr "ill-formed environment: " (string_of_any g)
        | Some wfg -> 
            match doTyping g q, doPolyTyping g i, doTyping g d with 
              | (Principal, wf_q), (MkPartial wf_i), (BytesT, wf_d) -> 
                  MkPartial (PolyTyping_Justified g q i d wfg wf_q wf_i wf_d)
              | _ -> terr "Ill-typed justified infon: " (string_of_any p)
                  
                      
type wfK :: infostrate => P  =
  | WFK_Empty : wfK []
  | WFK_Cons : 
         p:polyterm
      -> K:infostrate
      -> polytyping [] p
      -> wfK K
      -> wfK (p::K)

val checkWFK : k:infostrate -> Partial (wfK k)
let rec checkWFK k = match k with 
  | [] -> MkPartial (WFK_Empty)
  | p::tl -> 
      match doPolyTyping [] p with
        | MkPartial tp -> 
            match checkWFK tl with 
              | MkPartial pftl -> MkPartial (WFK_Cons p tl tp pftl)
        

type WF :: substrate => infostrate => vars => P =
  | WF_SKG: 
      S:substrate -> K:infostrate -> G:vars 
    -> wfK K 
    -> wfG G
    -> WF S K G

val checkWF : s:substrate -> k:infostrate -> g:vars -> option (WF s k g)
let checkWF s k g = 
  match checkWFK k, decideWFG g with 
    | MkPartial wfk, Some wfg -> Some (WF_SKG s k g wfk wfg)
    | _ -> None
