module Interp
open TypeHeaders
open Types
open Util
open Marshall

type substrate
type vars = list var
type substitutions = list substitution

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

type rule =
  | Rule : vars -> conditions -> actions -> rule

type rules = list rule
type infon = polyterm
type communications = list infon

(* ================= Wrapping external functions ===================== *)
(* val me : principal *)
let me = Crypto.lookup_my_credentials ()

(* val derive: u:vars -> goal:infon -> s0:substitution -> list substitution *)
let derive u goal s0 = 
  let s = State.getSubstrate () in
  let k = State.getInfostrate () in 
    match goal with 
      | MonoTerm i -> 
          (match InfonLogic.deriveQuant u s k [] s0 i with 
             | None -> []
             | Some ((s, pf)) ->  [s])
      | ForallT xs i -> 
          (match InfonLogic.deriveQuant u s k xs s0 i with 
             | None -> []
             | Some ((s, pf)) ->  [s])

(* ================= Process state and messaging ===================== *)
let comms = ref []

let get_communications _unit = 
  let message = Net.receive () in
  let infons = Marshall.parse message in 
    comms := append infons (!comms)

(* drop infons in co that are the same as i *)
let dropCommunications i = 
  let changed, comms' = 
    fold_right (fun j (changed, out) -> 
                  if i=j then (true, out) 
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

(* val fwd : principal -> infon -> bool *)
let fwd p i = 
  let m = Forwarded ({msg_from=me;
                      msg_to=p;
                      msg_payload=i}) in 
    dispatch p m 
  
(* val send : principal -> infon -> bool *)
let send p i = 
  let m = Justified ({msg_from=me;
                      msg_to=p;
                      msg_payload=i}) in 
    dispatch p m 
                               
(* ========================== Rule engine ==================== *)
(* val matchComm : comm:infon -> xs:vars -> pat:infon -> substitution -> substitutions  *)
let matchComm comm xs pat s = 
  match Unify.unify_poly s [] xs comm (Subst.polysubst pat s) with 
    | None -> []
    | Some s1 -> [(s1:substitution)]
    
let matchComms xs pat s = collect (fun comm -> matchComm comm xs pat s) (!comms)
  
(* val substAction: substitutions -> action -> actions *)
let substAction substs a =
  match a with
    | Learn i -> map (fun s -> Learn (Subst.polysubst i s)) substs
    | Drop i -> map (fun s -> Drop (Subst.polysubst i s)) substs
    | Fwd p i -> map (fun s -> Fwd (Subst.subst p s) (Subst.polysubst i s)) substs
    | Send p i -> map (fun s -> Send (Subst.subst p s) (Subst.polysubst i s)) substs

(* val evalCond: vars -> substitutions -> condition -> substitutions  *)
let evalCond xs sl c = match c with 
  | If i -> collect (fun s -> derive xs (Subst.polysubst i s) s) sl
  | Upon i -> collect (fun s -> matchComms xs (Subst.polysubst i s) s) sl
      
let evalConds xs cs = fold_left (evalCond xs) [(Subst.emptySubst())] cs

(* val holdsOne : rule -> actions *)
let holdsOne r = match r with 
  | Rule xs cs acts ->  collect (substAction (evalConds xs cs)) acts
let holds rs = collect holdsOne rs

let asPrincipal = function 
  | Const (PrincipalConstant p) -> p
  | _ -> raise "Unexpected type"

(* val applyAction: bool -> action -> bool  *)
let applyAction b a =  match a with 
  | Learn i -> 
      (assume (State.Valid i)); (* TODO: remove this. *)
      State.addToInfostrate i;
      true
  | Drop i -> dropCommunications i 
  | Fwd p i -> fwd (asPrincipal p) i
  | Send p i -> send (asPrincipal p) i

val until_fix: rules -> unit
let rec until_fix rs = 
  let actions = holds rs in
    if fold_left applyAction false actions
    then until_fix rs 
    else ()
  
val run: rules -> unit
let rec run rs = 
  let _ = clear_out_buffer () in 
  let _ = get_communications () in (* blocks until new comms arrive *)
  let _ = until_fix rs in
    run rs
