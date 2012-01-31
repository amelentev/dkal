module Authenticate
open Types
open Crypto

type pk :: _ = (fun (p:principal) => pubkey polyterm (Says p) p)
type sk :: _ = (fun (p:principal) => privkey polyterm (Says p) p)

type recvSig = bool -> bytes
type sendSig = int -> bytes -> bool  (* port number of the receiver, buf *)

val config: Ref (option(p:principal{IsMe p} * pk p * sk p * recvSig * sendSig * list (q:principal * pk q * int)))
let config = newref None

val readOthersInfo: StreamReader -> int -> list (q:principal * pk q * int) -> list (q:principal * pk q * int)
let rec readOthersInfo stream c pks = 
   if (c = 0) then pks 
   else let p = StreamReaderReadLine stream in
        let k: pk p = MkPubKey p (StreamReaderReadLine stream) in 
        let portNum = stringToInt(StreamReaderReadLine stream) in
        let other: (q:principal * pk q * int) = (p, k, portNum) in
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
  let recv, send = createComm portNumber in
  let othersCount = stringToInt(StreamReaderReadLine stream) in
  config := Some(me, pubKey, privKey, recv, send, readOthersInfo stream othersCount [])

val lookup_my_credentials : unit -> (p:principal{IsMe p} * pk p * sk p)
let lookup_my_credentials() =
  match (!config) with
    | Some c -> (match c with (p, pk, sk, _, _, _) -> (p, pk, sk))
    | _ -> raise "No configuration"

val myReceive : unit -> bool -> bytes
let myReceive () =
  match (!config) with
    | Some c -> (match c with (_, _, _, recv, _, _) -> recv)
    | None -> raise "myReceive: no configuration"

val mySend: int -> bytes -> bool
let mySend portNum =
  match (!config) with
    | Some c -> (match c with (_, _, _, _, send, _) -> send portNum)
    | None -> raise "mySend: no configuration"

val findKey: list (q:principal * pk q * int) -> p:principal -> pk p
let rec findKey l p =
  match l with
    | [] -> raise (strcat "cannot find key for " p)
    | (p', k, _) :: rest -> if (p = p') then k else findKey rest p

val lookup_pubkey: p:principal -> pk p
let lookup_pubkey p =
  match (!config) with
    | Some c -> (match c with (_, _, _, _, _, keys) -> findKey keys p)
    | None -> raise "lookup_pubkey: no configuration"

val findPortNum: list (q:principal * pk q * int) -> p:principal -> int
let rec findPortNum l p =
  match l with
    | [] -> raise (strcat "cannot find port number for " p)
    | (p', _, num) :: rest -> if (p = p') then num else findPortNum rest p

val lookup_portNum: principal -> int
let lookup_portNum p =
  match (!config) with
    | Some c -> (match c with (_, _, _, _, _, keys) -> findPortNum keys p)
    | None -> raise "lookup_portNum: no configuaration"
