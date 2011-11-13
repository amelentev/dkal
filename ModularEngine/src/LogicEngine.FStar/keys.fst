module Keys
open Types

val keys: Ref (list (principal * string * string))
let keys = newref []

val addKey: principal -> unit
let addKey p =
  match (keyGen true) with
    | (pub, priv) -> keys := (p, pub, priv)::(!keys)

val findKey: list (principal * string * string) -> principal
    -> option (string * string)
let rec findKey l p = match l with
  | [] -> None
  | (p', pub, priv)::rest ->
    if (p = p') then Some (pub, priv)
    else findKey rest p

val findPubKey: principal -> option string
let findPubKey p = 
  let keys = findKey (!keys) p in
  match keys with
    | None -> None
    | Some keypair -> (match keypair with | (pub, _) -> Some pub)

val findPrivKey: p:principal{Crypto.IsMe p} -> option string
let findPrivKey p =
  let keys = findKey (!keys) p in
  match keys with
    | None -> None
    | Some keypair -> (match keypair with | (_, priv) -> Some priv)