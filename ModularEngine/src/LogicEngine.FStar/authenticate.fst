module Authenticate
open Types
open Crypto
open Marshall

type message :: _ =  fun ('P::principal => principal => polyterm => E) => 
    ((pfrom:principal *
        pto:principal *
          i:polyterm{'P pfrom pto i}))

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

val polyterm2bytes: i:polyterm -> b:bytes{Serialized i b}
let polyterm2bytes i =
  let str = printInfon i in
    FromUnicodeString str

val msg2bytes: msg -> bytes
let msg2bytes m =
  let (me, pkey, skey) = myKeys in
    match m with
      | Forwarded ((p,q,i)) -> polyterm2bytes i

      | Justified((p, q, i)) when p=me ->
          assert (Says me i);
          let b = polyterm2bytes i in
          let dsig = rsa_sign me skey i b in
          let ji = JustifiedPoly (Const (PrincipalConstant p)) i (Const (Bytes dsig)) in 
            polyterm2bytes ji

val checkMono : i:term -> b:bool{b=true => CheckedInfonMono i}
let rec checkMono i = match i with 
  | Var x -> true
  | Const c -> true
  | SubstrateQueryTerm _ -> true
  | App f tms -> 
      if f=JustifiedInfon then 
        match tms with 
          | [(Const (PrincipalConstant p)); i; (Const (Bytes ds))] -> 
              (match lookup_pubkey p with 
                 | None -> false
                 | Some pubk -> 
                     let bi = polyterm2bytes (MonoTerm i) in 
                       (rsa_verify p pubk (MonoTerm i) bi ds) && checkMono i)
          | _ -> false
      else for_all<term, CheckedInfonMono> checkMono tms 
        
val checkInfon : p:polyterm -> b:bool{b=true => CheckedInfon p}
let rec checkInfon p = match p with
  | MonoTerm i -> checkMono i
  | ForallT xs i -> checkMono i
  | JustifiedPoly (Const (PrincipalConstant p)) i (Const (Bytes ds)) -> 
      (match lookup_pubkey p with 
         | None -> false
         | Some pubk -> 
             let bi = polyterm2bytes i in 
               (rsa_verify p pubk i bi ds) && checkInfon i)
  | _ -> false

val bytes2infon: b:bytes -> option (i:infon{(Net.Received b => Net.Received i)})
let bytes2infon b = 
  let s = ToUnicodeString b in 
    assert (Net.Received b => Net.Received s); 
    match parseInfon s with 
      | Some poly when checkInfon poly -> 
          assert ((ReprPoly poly)=s);
          assert (Net.Received s => Net.Received poly);
            Some poly
      | _ -> None
    
