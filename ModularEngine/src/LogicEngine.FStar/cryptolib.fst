module CryptoLib
extern reference Crypto { language = "F#";
                          dll="crypto";
                          namespace="Microsoft.FStar";
                          classname="Crypto"}
                          
type Pub :: ('a::* => 'a => E)
extern Crypto val rsa_new_key_pair: bool -> (string*string)
extern Crypto val rsa_sign_impl: msg:bytes -> pk:string -> (dsig:bytes{Pub msg => Pub dsig})
extern Crypto val rsa_verify_impl: msg:bytes -> ds:bytes -> sk:string -> bool

type dsig = bytes
type pubkey :: 'a::* => ('a => E) => * = 
   | MkPubKey: 'a::* -> 'P::('a => E) -> string -> pubkey 'a 'P
type privkey :: 'a::* => ('a => E) => * = 
   | MkPrivKey: 'a::* -> 'P::('a => E) -> string -> privkey 'a 'P

type KeyPair :: 'a::* => 'b::* => 'a => 'b => E
assume Pubkey_is_pub: forall (pubk:pubkey<'a, 'P::'a => $E>). (Pub pubk)
assume Pub_iff_saturated: forall  (privk:privkey<'a, 'P::'a => $E>).
                                   (Pub privk) <=> (forall (x:'a). P x)
assume Ser_of_pub_is_pub: forall (x:'a), (y:bytes). (Serialized x y && Pub x) => (Pub y)

val rsa_keygen: 'a::* -> 'P::('a => E) 
             -> unit
             -> (pk:pubkey 'a 'P * (sk:privkey 'a 'P{KeyPair pk sk}))
let rsa_keygen _x =
  let pub, priv = rsa_new_key_pair false in
  let pubk = (MkPubKey pub : pubkey 'a 'P) in
  let privk = (MkPrivKey priv : privkey 'a 'P) in
  let _ = assume (KeyPair pubk privk) in
    (pubk, privk)  

val rsa_sign:  'a::* -> 'P::('a => E) 
             -> privkey 'a 'P
             -> x:'a 
             -> b:bytes{Serialized x b && 'P x}
             -> s:dsig{(Pub b) => (Pub s)}
let rsa_sign sk x b = match sk with
  | MkPrivKey skb -> rsa_sign_impl b skb 

val rsa_verify:  'a::* -> 'P::('a => E) 
             -> pubkey 'a 'P
             -> x:'a 
             -> b:bytes{Serialized x b}
             -> s:dsig 
             -> r:bool{r=true => 'P x}
let rsa_verify pk x b s =
  match pk with
    | MkPubKey pkb ->
        let res = rsa_verify_impl b s pkb in
          if res
          then let _ = assume ('P x) in res
          else res

val rsa_keyleak:  'a::* -> 'P::('a => E) 
             -> x:privkey 'a 'P{forall (x:'a). 'P x}
             -> k':bytes{(Pub k')}
end

