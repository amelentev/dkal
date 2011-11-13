module Authenticate
open Types
open Crypto
open Marshall

type message :: _ =  fun ('P::principal => principal => polyterm => E) => 
    ((pfrom:principal *
        pto:principal *
          i:polyterm{'P pfrom pto i}))

type Says :: principal => polyterm => E
type trivial :: _ = (fun (p:principal) (q:principal) (i:polyterm) => True)
type jmessage = message (fun (p:principal) (q:principal) (i:polyterm) => (Says p i)) 
type msg = 
  | Forwarded : message trivial -> msg
  | Justified : jmessage -> msg

type pk :: _ = (fun (p:principal) => pubkey polyterm (Says p) p)
type sk :: _ = (fun (p:principal) => privkey polyterm (Says p) p)

val lookup_my_credentials : unit -> (p:principal{IsMe p} * pk p * sk p)
val lookup_pubkey: p:principal -> option (pk p)
let myKeys = lookup_my_credentials ()

val infon2bytes: i:polyterm -> b:bytes{Serialized i b}
(* let infon2bytes i =  *)
(*   let str = printInfon i in  *)
(*     FromUnicodeString str  *)

val msg2bytes: msg -> bytes
(* let msg2bytes msg =  *)
(*   let (me, pkey, skey) = myKeys in  *)
(*     match msg with  *)
(*       | Forwarded((p, q, i)) -> infon2bytes i *)

(*       | Justified((p, q, i)) when p=me ->  *)
(*           assert (Says me i); *)
(*           let b = infon2bytes i in  *)
(*           let dsig = rsa_sign me skey i b in *)
(*           let ji = App JustifiedInfon [i; (App SignatureEvidence [p; i; dsig])] in  *)
(*             infon2bytes ji *)

val b2s: b:bytes -> s:string{Net.Received b => Net.Received s}
val bytes2infon: b:bytes -> option (i:polyterm{Net.Received b => Net.Received i})
(* let bytes2infon b =  *)
(*   let str = ToUnicodeString b in  *)
(*   let i = parseInfon i  *)
  

