module Typing
open Types
open Util

(* 1. Dynamic type tests from .NET *)
type TypeOf :: object => typ => E
val check_object_typ : x:object -> t:typ -> b:bool{b=true => TypeOf x t}

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

type WF :: substrate => infostrate => varDecl => P 
val checkWF : s:substrate -> k:infostrate -> g:varDecl -> option (WF s k g)
(* TODO, i.e., unique names in vardecl *)

val containsVar : g:varDecl -> x:var -> bool
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

