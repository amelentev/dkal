module Crypto
open Types

extern reference Crypto { language = "F#";
                          dll="FSCrypto";
                          namespace="Microsoft.FStar";
                          classname="Crypto"}

extern Crypto val rsa_new_key_pair: bool -> (string*string)
extern Crypto val rsa_sign_impl: msg:bytes -> pk:string -> bytes
extern Crypto val rsa_verify_impl: msg:bytes -> ds:bytes -> sk:string -> bool

type Serialized:: 'a::* => 'a => bytes => E

type dsig = bytes
(* read creds from a config file *)
type IsMe :: principal => E
assume forall (p:principal) (q:principal). IsMe p && IsMe q => p=q

type pubkey :: 'a::* => ('a => E) => principal => * = 
   | MkPubKey: 'a::* -> 'P::('a => E) -> p:principal -> string -> pubkey 'a 'P p
type privkey :: 'a::* => ('a => E) => principal => * = 
   | MkPrivKey: 'a::* -> 'P::('a => E) -> p:principal -> string -> privkey 'a 'P p

type KeyPair :: 'a::* => 'b::* => 'a => 'b => E

val rsa_keygen: 'a::*  -> 'P::('a => E) 
             -> p:principal
             -> unit
             -> (pk:pubkey 'a 'P p * (sk:privkey 'a 'P p{KeyPair pk sk}))
let rsa_keygen p _x =
  let pub, priv = rsa_new_key_pair false in
  let pubk = (MkPubKey p pub : pubkey 'a 'P p) in
  let privk = (MkPrivKey p priv : privkey 'a 'P p) in
  let _ = assume (KeyPair pubk privk) in
    (pubk, privk)  

val rsa_sign:  'a::* -> 'P::('a => E)
             -> p:principal
             -> privkey 'a 'P p
             -> x:'a 
             -> b:bytes{Serialized x b && 'P x}
             -> dsig
let rsa_sign p sk x b = match sk with
  | MkPrivKey p skb -> rsa_sign_impl b skb 

val rsa_verify:  'a::* -> 'P::('a => E) 
             -> p:principal
             -> pubkey 'a 'P p
             -> x:'a 
             -> b:bytes{Serialized x b}
             -> s:dsig 
             -> r:bool{r=true => 'P x}
let rsa_verify p pk x b s =
  match pk with
    | MkPubKey p pkb ->
        let res = rsa_verify_impl b s pkb in
          if res
          then let _ = assume ('P x) in res
          else res

val rsa_keyleak:  'a::* -> 'P::('a => E) 
             -> p:principal
             -> x:privkey 'a 'P p{forall (x:'a). 'P x}
             -> bytes
let rsa_keyleak p x = match (x:privkey 'a 'P p) with 
  | MkPrivKey _ b -> FromUnicodeString b
      

end
