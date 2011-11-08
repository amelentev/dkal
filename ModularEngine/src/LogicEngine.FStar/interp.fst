module Interp
open TypeHeaders
open Types
open Util
open Marshall
open State
open Subst

type vars = list var
type substitutions = list substitution

let me = Crypto.lookup_my_credentials () 

type condition =
  | If   : polyterm -> condition
  | Upon : polyterm -> condition

type conditions = list condition

type action =
  | Learn : polyterm -> action (* infon *)
  | Drop  : polyterm -> action (* infon *)
  | Fwd   : term -> polyterm -> action (* prin, infon *)
  | Send  : term -> polyterm -> action (* prin, infon *)

type actions = list action

type infon = polyterm
type communication = i:infon{Net.Received i}
type communications = list communication

(* -------------------------------------------------------------------------------- *)
(* Spec: Validity of condition(s) *)
(* -------------------------------------------------------------------------------- *)
type Includes :: vars => vars => E (* TODO *)

logic function CondSubst : condition -> substitution -> condition
assume forall (i:infon) (s:substitution). (CondSubst (If i) s) = (If (PolySubst i s))
assume forall (i:infon) (s:substitution). (CondSubst (Upon i) s) = (Upon (PolySubst i s))

logic function ActionSubst : action -> substitution -> action
assume forall (i:infon) (s:substitution). (ActionSubst (Learn i) s) = (Learn (PolySubst i s))
assume forall (i:infon) (s:substitution). (ActionSubst (Drop i) s) = (Drop (PolySubst i s))
assume forall (p:term) (i:infon) (s:substitution). (ActionSubst (Fwd p i) s) = (Fwd (Subst p s) (PolySubst i s))
assume forall (p:term) (i:infon) (s:substitution). (ActionSubst (Send p i) s) = (Send (Subst p s) (PolySubst i s))

type HoldsOne :: vars => condition => substitution => E
assume forall (xs:vars) (i:infon) (subst:substitution) (s:substrate) (k:infostrate). 
         ((Includes xs (Domain subst)) && 
          (Includes xs (FreeVarsPoly i)) &&
          (InfonLogic.polyentails s k [] (PolySubst i subst)))
      => HoldsOne xs (If i) subst
assume forall (xs:vars) (i:infon) (subst:substitution) (c:communication). 
         ((Includes xs (Domain subst)) &&
          (Includes xs (FreeVarsPoly i)) &&
          ((PolySubst i subst)=c))
      => HoldsOne xs (Upon i) subst

type HoldsMany :: substitution => vars => list condition => substitution => E
assume forall (s:substitution) (xs:vars).
          (Includes (Domain s) xs) && (Includes xs (Domain s))
          =>  HoldsMany s xs [] s
assume forall (s1:substitution) (xs:vars) (c:condition) (s1':substitution) 
              (cs:conditions) (s2:substitution). 
          (HoldsOne xs (CondSubst c s1) s1') && 
          (HoldsMany s1' xs cs s2)
          => HoldsMany s1 xs cs s2
          
type Holds :: _ = fun (xs:vars) (cs:list condition) (s:substitution) => (HoldsMany EmptySubst xs cs s)

(* -------------------------------------------------------------------------------- *)
(* Spec: Enabled actions *)
(* -------------------------------------------------------------------------------- *)
type Enabled :: action => E
assume forall (i:infon). (Enabled (Learn i)) => Knows i
assume forall (me:principal) (p:principal) (i:infon).
          (Enabled (Send (Const (PrincipalConstant p)) i) && Crypto.IsMe me) 
       => SaysTo me p i

type rule =
  | Rule : xs:vars 
        -> cs:conditions 
        -> acts:actions{forall (a:action) (subst:substitution). In a acts && Holds xs cs subst => (Enabled (ActionSubst a subst))}
        -> rule

type rules = list rule

(* ================= Wrapping external functions ===================== *)

val _derive:  u:vars
          -> goal:infon
          -> s0:substitution
          -> list (s:substitution{HoldsOne u (CondSubst (If goal) s0) s})
let derive u goal s0 = 
  let s = State.getSubstrate () in
  let k = State.getInfostrate () in 
    match goal with 
      | MonoTerm i -> 
          (match InfonLogic.deriveQuant u s k [] s0 i with 
             | None -> []
             | Some ((s, pf)) ->  [(s:substitution)])
      | ForallT xs i -> 
          (match InfonLogic.deriveQuant u s k xs s0 i with 
             | None -> []
             | Some ((s, pf)) ->  [(s:substitution)])

(* ================= Process state and messaging ===================== *)
val comms : ref (list communication)
let comms = ref []

val get_communications: unit -> unit
let rec get_communications _unit = 
  let message = Net.receive () in
    match Marshall.parseInfon (b2s message) with 
      | Some infon -> comms := infon::(!comms)
      | _ -> get_communications ()
          
val _dropCommunications: i:infon{Enabled (Drop i)} -> unit
let dropCommunications i = 
  let changed, comms' = 
    fold_right (fun (j:communication) (changed, out) -> 
                  if (i:infon)=j then (true, out) 
                  else (changed, j::out)) (!comms) (false, []) in 
    (comms := comms');
    changed

let outbuffer = ref []
let clear_out_buffer () = outbuffer := []

let dispatch p m = 
  if List_exists (fun (m':msg) -> m'=m) (!outbuffer)
  then false
  else
    ((outbuffer := m::(!outbuffer));
     (Net.send p (Marshall.msg2bytes m));
     true)

val _fwd: p:principal -> i:infon{Enabled (Fwd (Const (PrincipalConstant p)) i)} -> bool 
let fwd p i = 
  let m = Forwarded (me, p, i) in 
    dispatch p m 
  
val send : p:principal -> i:infon{Enabled (Send (Const (PrincipalConstant p)) i)} -> bool 
let send p i = 
  let z : principal = me in 
  let msg : message SaysTo = ( (z, p, (i: (i:infon{SaysTo z p i}))) : (z:principal * p:principal * i:infon{SaysTo z p i})) in 
  let m = Justified msg in 
    dispatch p m 
                               
(* ========================== Rule engine ==================== *)
val _matchComm : comm:communication 
             -> xs:vars -> pat:infon 
             -> s0:substitution 
             -> list (s1:substitution{HoldsOne xs (Upon (PolySubst pat s0)) s1})
let matchComm (comm:communication) xs pat s = 
  match Unify.unify_poly s [] xs comm (Subst.polysubst pat s) with 
    | None -> []
    | Some s1 -> [(s1:substitution)]

let matchComms xs pat s = collect (fun comm -> matchComm comm xs pat s) (!comms)
  
val _substAction: substs:substitutions 
              -> a:action 
              -> acts:actions{Zip substitution action (fun (subst:substitution) (act:action) => (act=(ActionSubst a subst))) substs acts}
let substAction substs a =
  match a with
    | Learn i -> map (fun s -> Learn (Subst.polysubst i s)) substs
    | Drop i -> map (fun s -> Drop (Subst.polysubst i s)) substs
    | Fwd p i -> map (fun s -> Fwd (Subst.subst p s) (Subst.polysubst i s)) substs
    | Send p i -> map (fun s -> Send (Subst.subst p s) (Subst.polysubst i s)) substs

(* val _evalCond: xs:vars -> s1:substitutions -> c:condition -> list (s:substition{}) *)
let evalCond xs sl c = match c with 
  | If i -> collect (fun s -> derive xs (Subst.polysubst i s) s) sl
  | Upon i -> collect (fun s -> matchComms xs (Subst.polysubst i s) s) sl

val _evalConds: xs:vars -> cs:conditions -> list (s:substitution{Holds xs cs s})      
let evalConds xs cs = fold_left (evalCond xs) [(Subst.emptySubst false)] cs

val _enabledActions : r:rule -> list (a:action{Enabled a})
let enabledActions r = match r with 
  | Rule xs cs acts ->  collect (substAction (evalConds xs cs)) acts

val _allEnabledActions : rs:list rule -> list (a:action{Enabled a})
let allEnabledActions rs = collect enabledActions rs

val asPrincipal: p:term -> q:principal{p=(Const (PrincipalConstant q))}
let asPrincipal = function 
  | Const (PrincipalConstant p) -> p
  | _ -> raise "Unexpected type"

val _applyAction: b:bool -> a:action{Enabled a} -> bool 
let applyAction b a = assume (Enabled a);
  match a with 
    | Learn i -> State.addToInfostrate i; true
    | Drop i -> dropCommunications i 
    | Fwd p i -> fwd (asPrincipal p) i
    | Send p i -> send (asPrincipal p) i

val until_fix: rules -> unit
let rec until_fix rs = 
  let actions = allEnabledActions rs in
    if fold_left applyAction false actions
    then until_fix rs 
    else ()
  
val run: rules -> unit
let rec run rs = 
  let _ = clear_out_buffer () in 
  let _ = get_communications () in (* blocks until new comms arrive *)
  let _ = until_fix rs in
    run rs
