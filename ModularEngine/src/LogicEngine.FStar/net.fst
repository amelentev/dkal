module Net
open Types
open Crypto
open Authenticate
open Marshall

type Received :: 'a::* => 'a => E
assume forall (x:'a) (l:list 'a). In x l && Received l => Received x
assume forall (s:string). Received (UnicodeStringToBytes s) => Received s
assume forall (p:polyterm). (Received (ReprPoly p) => Received p)

val receive: unit -> b:bytes{Received b}
let receive () =
  let b = myReceive () in
  (assume (Received b); b)
  
val send: principal -> bytes -> unit
let send p b = 
  let portNum = lookup_portNum p in
  let _ = mySend portNum b in ()

type message :: _ =  fun ('P::principal => principal => polyterm => E) => 
    ((pfrom:principal *
        pto:principal *
          i:polyterm{'P pfrom pto i}))

type trivial :: _ = (fun (p:principal) (q:principal) (i:polyterm) => True)
type jmessage = message (fun (p:principal) (q:principal) (i:polyterm) => (Says p i)) 
type msg = 
  | Forwarded : message trivial -> msg
  | Justified : jmessage -> msg

val polyterm2bytes: i:polyterm -> b:bytes{Serialized i b}
let polyterm2bytes i =
  let str = printInfon i in
    FromUnicodeString str

val msg2bytes: msg -> bytes
let msg2bytes m =
  let (me, pkey, skey) = lookup_my_credentials() in
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
              let pubk = lookup_pubkey p in
              let bi = polyterm2bytes (MonoTerm i) in 
              (rsa_verify p pubk (MonoTerm i) bi ds) && checkMono i
          | _ -> false
      else for_all<term, CheckedInfonMono> checkMono tms 
        
val checkInfon : p:polyterm -> b:bool{b=true => CheckedInfon p}
let rec checkInfon p = match p with
  | MonoTerm i -> checkMono i
  | ForallT xs i -> checkMono i
  | JustifiedPoly (Const (PrincipalConstant p)) i (Const (Bytes ds)) -> 
      let pubk = lookup_pubkey p in
      let bi = polyterm2bytes i in 
	  let _ = println (strcat "#### p = " (string_of_any_for_coq p)) in
	  let _ = println (strcat "#### pubk = " (string_of_any_for_coq pubk)) in
	  let _ = println (strcat "#### i = " (string_of_any_for_coq i)) in
	  let _ = println (strcat "#### bi = " (string_of_any_for_coq bi)) in
	  let _ = println (strcat "#### ds = " (string_of_any_for_coq ds)) in
	  let b1 = rsa_verify p pubk i bi ds in
	  let b2 = checkInfon i in
	  let _ = if (b1) then println "rsa_verified" else false in
	  let _ = if (b2) then println "checkInfon done" else false in
	  b1 && b2
      (*(rsa_verify p pubk i bi ds) && checkInfon i*)
  | _ -> false

val bytes2infon: b:bytes -> option (i:infon{(Received b => Received i)})
let bytes2infon b = 
  let s = ToUnicodeString b in 
    assert (Received b => Received s); 
    match parseInfon s with 
      | Some poly when checkInfon poly -> 
          assert ((ReprPoly poly)=s);
          assert (Received s => Received poly);
            Some poly
      | _ -> None

val sendForTest: string -> string -> bytes -> unit
let sendForTest me p bytes =
  let i = MonoTerm(Const (Bytes bytes)) in
  let _ = assume (Says me i) in
  let message = Justified(me, p, i) (*Forwarded(me, p, i)*) in
  send p (msg2bytes message) 


val recvForTest: bytes -> bytes
let recvForTest bytes =
  let bytesForEmpty = FromUnicodeString "" in
  match (bytes2infon bytes) with
    | None -> bytesForEmpty
	| Some i ->
	  (match i with
	    | JustifiedPoly p inf dsig ->
		  (match inf with
		     | MonoTerm t ->
		       (match t with | Const c -> (match c with | Bytes b -> b | _ -> bytesForEmpty)
		                     | _ -> bytesForEmpty)
			 | _ -> bytesForEmpty)
		| _ -> bytesForEmpty)