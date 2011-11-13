module Net
open Types

type Received :: 'a::* => 'a => E
assume forall (x:'a) (l:list 'a). In x l && Received l => Received x

val methods: Ref (list (principal * (bool -> bytes) * (bytes -> bool)))
let methods = newref []
val suscribe: list (principal * (bool -> bytes) * (bytes -> bool)) -> unit
let suscribe l = (methods:= l)

val findMethod: principal -> list (principal * (bool -> bytes) * (bytes -> bool)) 
    -> ((bool->bytes) * (bytes->bool))
let rec findMethod p l = match l with
  | [] -> raise (strcat "cannot find methods for " p)
  | (p', recv, send_to) :: rest ->
    if p = p' then (recv, send_to)
    else findMethod p rest
  
val receive: unit -> b:bytes{Received b}
let receive () =
  let me = Crypto.lookup_my_credentials() in
  match findMethod me (!methods) with
    | (recv, _ ) ->
      let b = recv true in
      (assume (Received b); b)
    | _ -> raise "Net.receive"
  
val send: principal -> bytes -> unit
let send p b =
  match findMethod p (!methods) with
    | (_, send_to) ->
      let _ = send_to b in
      ()
    | _ -> raise "Net.send"

