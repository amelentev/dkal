module Marshall
open TypeHeaders
open Types

logic function Strcat : list string -> string
logic function ReprVar : var -> string
logic function ReprVars : vars -> string
logic function ReprMono : term -> string
logic function ReprPoly : polyterm -> string
assume forall (xs:vars) (t:term). 
                         ReprPoly (ForallT xs t) = Strcat ["(Forall ("; 
                                                           (ReprVars xs);
                                                           ") ";
                                                           (ReprMono t);
                                                           ")"]
assume forall (t:term). ReprPoly (MonoTerm t) = ReprMono t

(* type Star :: P => * = *)
(*  | MkStar : 'a::P -> Star 'a *)

(* val rep_isa_function:  n:nat  *)
(* (\*                     -> total n polyterm () *\) *)
(*                     -> p:polyterm{SizeOf p = n} *)
(*                     -> s1:string{(ReprPoly p)=s1}  *)
(*                     -> s2:string{(ReprPoly p) = s2}  *)
(*                     -> Star (Eq s1 s2) *)

(* val rep_is_injective:   p1:polyterm  *)
(*                      -> p2:polyterm  *)
(*                      -> s:string{(ReprPoly p1=s) && (ReprPoly p2=s)} *)
(*                      -> Star (Eq p1 p2) *)

(* Note: these functions are recursive and so cannot be in P
         as such, we cannot prove directly that they are total functions. 
         If we can do this totality proof in some other way, for example, 
         by providing an explicit ranking function, then we will have proved 
         that at least ReprPoly is at least a function. *)
val printInfon: p:polyterm -> b:string{(ReprPoly p)=b}
let printInfon p = raise "TODO"

(* Note: As for the printer, this function is recursive and so not in P. *)
val parseInfon: b:string -> option (p:polyterm{(Net.Received b => Net.Received p) &&
                                               (ReprPoly p)=b})
let parseInfon b = raise "TODO"                                         


(* -------------------------------------------------------------------------------- *)

type message :: _ =  fun ('P::principal => principal => polyterm => E) => 
    ((pfrom:principal *
        pto:principal *
          i:polyterm{'P pfrom pto i}))

type SaysTo :: principal => principal => polyterm => E
type trivial :: _ = (fun (p:principal) (q:principal) (i:polyterm) => True)

type msg = 
  | Forwarded : message trivial -> msg
  | Justified : message SaysTo -> msg

val concat : list bytes -> bytes
val unconcat: bytes -> list bytes
val msg2bytes: msg -> bytes
