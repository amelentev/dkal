module Authenticate
open Types
open Crypto

type pk :: _ = (fun (p:principal) => pubkey polyterm (Says p) p)
type sk :: _ = (fun (p:principal) => privkey polyterm (Says p) p)

val config: Ref (option(p:principal{IsMe p} * pk p * sk p * TCPListener * int * list (q:principal * pk q * int)))
let config = newref None

type config = {prin:string; pubkey:string; privkey:option string; port:int}

val initOthers : list config -> list (q:principal * pk q * int)
let rec initOthers = function 
  | [] -> []
  | cfg::rest -> 
      let p = cfg.prin in
      let tl = initOthers rest in 
      let k: pk p = MkPubKey p (cfg.pubkey) in
      let other: (q:principal * pk q * int) = (p, k, cfg.port) in
        other::tl

val initialize : list config -> unit
let initialize = function
  | mycfg::others -> 
      let priv = match mycfg.privkey with 
        | None -> raise "Incorrect config: no privkey for local prin"
        | Some priv -> priv in 
      let me = mycfg.prin in 
      let pubKey: pk me = MkPubKey me (mycfg.pubkey) in
      let privKey: sk me = MkPrivKey me priv in
      let _ = assume (IsMe me) in
      let _ = println (strcat "LISTENING ON PORT " (string_of_any (mycfg.port))) in
      let listener = createComm (mycfg.port) in 
      let _ = config := Some(me, pubKey, privKey, listener, mycfg.port, initOthers others) in 
        ()
  | _ -> raise "Incorrect config: no me"

val readOthersInfo: StreamReader -> int -> list (q:principal * pk q * int) -> list (q:principal * pk q * int)
let rec readOthersInfo stream c pks = 
   if (c = 0) then pks 
   else let p = StreamReaderReadLine stream in
        let k: pk p = MkPubKey p (StreamReaderReadLine stream) in 
        let portNum = stringToInt(StreamReaderReadLine stream) in
        let other: (q:principal * pk q * int) = (p, k, portNum) in
		let _ = println (strcat "got the other principal " p) in
        let pks': list (q:principal * pk q * int) =  other :: pks in
        let c' = c - 1 in
        readOthersInfo stream c' pks'

(* read a configuration file: me, public key for me, private key for me, portNumber, number of principal-pubkey-portNumber tuples, list of (principal * pub keys * port number) *)
val readConfig: string -> unit
let readConfig fileName =
  let stream = StreamReaderCtor fileName in
  let me = StreamReaderReadLine stream in
  let pubKey: pk me = MkPubKey me (StreamReaderReadLine stream) in
  let privKey: sk me = MkPrivKey me (StreamReaderReadLine stream) in
  let _ = assume (IsMe me) in
  let portNumber = stringToInt(StreamReaderReadLine stream) in
  let listener = createComm portNumber in
  let othersCount = stringToInt(StreamReaderReadLine stream) in
  let _ = config := Some(me, pubKey, privKey, listener, portNumber, readOthersInfo stream othersCount []) in
  ()

val lookup_me: unit -> (p:principal{IsMe p})
let lookup_me () =
 match (!config) with
    | Some c -> (match c with (p, _, _, _, _, _) -> p)
    | _ -> raise "No configuration"

val lookup_my_credentials : unit -> (p:principal{IsMe p} * pk p * sk p)
let lookup_my_credentials() =
  match (!config) with
    | Some c -> (match c with (p, pk, sk, _, _, _) -> (p, pk, sk))
    | _ -> raise "No configuration"

val myReceive : unit -> bytes
let myReceive () =
  match (!config) with
    | Some c -> (match c with (_, _, _, listener, _, _) -> getRecv listener)
    | None -> raise "myReceive: no configuration"

val mySend: int -> bytes -> bool
let mySend portNum b = getSend portNum b

val findKey: list (q:principal * pk q * int) -> p:principal -> pk p
let rec findKey l p =
  match l with
    | [] -> raise (strcat "cannot find key for " p)
    | (p', k, _) :: rest -> if (p = p') then k else findKey rest p

val lookup_pubkey: p:principal -> pk p
let lookup_pubkey p =
  match (!config) with
    | Some c -> (match c with (q, pubk, _, _, _, keys) -> if p=q then pubk else findKey keys p)
    | None -> raise "lookup_pubkey: no configuration"

val findPortNum: list (q:principal * pk q * int) -> p:principal -> int
let rec findPortNum l p =
  match l with
    | [] -> raise (strcat "cannot find port number for " p)
    | (p', _, num) :: rest -> if (p = p') then num else findPortNum rest p

val lookup_portNum: principal -> int
let lookup_portNum p =
  match (!config) with
    | Some c -> (match c with (q, _, _, _, myport, keys) -> if p=q then myport else findPortNum keys p)
    | None -> raise "lookup_portNum: no configuaration"
