module Marshall
open TypeHeaders
open Types

logic function Strcat : list string -> string
logic function ReprVar : var -> string
logic function ReprVars : vars -> string
logic function ReprConst: constant -> string
logic function ReprSubstrateQueryTerm: ISubstrateQueryTerm -> string
logic function ReprSubstrateUpdateTerm: ISubstrateUpdateTerm -> string
logic function ReprFunc: func -> string
logic function ReprMono : term -> string
logic function ReprPoly : polyterm -> string
assume forall (xs:vars) (t:term). 
           (ReprPoly (ForallT xs t)) = (Strcat ([ "(Forall (";
                                               (ReprVars xs);
                                               ") ";
                                               (ReprMono t);
                                               ")"]))
assume forall (t:term). (ReprPoly (MonoTerm t)) = (ReprMono t)

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

val printSubstrateConstant: object -> string
val printSubstrateQuery: ISubstrateQueryTerm -> string

val printList: ('a -> string) -> list 'a -> string -> string
let rec printList printOne l delimiter =
  match l with
   | [] -> ""
   | hd::tl -> 
     (match tl with
        | [] -> printOne hd
        | _ -> strcat (printOne hd)
               (strcat delimiter
                (printList printOne tl delimiter)))

val printOption: ('a -> string) -> option 'a -> string
let printOption printA opt =
  match opt with
    | None -> "None"
    | Some a -> strcat "Some("
                (strcat (printA a)
                 ")")

val printTyp: typ -> string
let printTyp ty = match ty with
  | Infon -> "Infon"
  | Principal -> "Principal"
  | SubstrateUpdate -> "SubstrateUpdate"
  | SubstrateQuery -> "SubstrateQuery"
  | Action -> "Action"
  | Condition -> "Condition"
  | RuleT -> "RuleT"
  | Evidence -> "Evidence"
  | Boolean -> "Boolean"
  | Int32 -> "Int32"
  | Double -> "Double"
  | String -> "String"

val printVar: var -> string
let printVar v = strcat "Var("
                 (strcat (v.name)
                 (strcat ", "
                 (strcat (printTyp v.typ)
                  ")")))

val printPrincipal: principal -> string
let printPrincipal p = p

val printConst: constant -> string
let printConst c = match c with
  | TrueT -> "TrueT"
  | FalseT -> "FalseT"
  | SubstrateConstant o -> strcat "SubstrateConstant("
                           (strcat (printSubstrateConstant o) ")")
  | PrincipalConstant p ->
    strcat "PrincipalConstant(" (strcat (printPrincipal p) ")")

val printRelationInfon:relationInfon -> string
val printFunc: func -> string
val printMono: term -> string

let printRelationInfon r =
  strcat "RelationInfon("
  (strcat (r.name)
  (strcat ", "
  (strcat (printTyp r.retType)
  (strcat ", ["
  (strcat (printList printTyp r.argsType "; ")
  (strcat "], "
  (strcat (printOption printMono r.identity)
   ")")))))))
 
let printFunc f = match f with
  | SeqRule -> "SeqRule"
  | EmptyRule -> "EmptyRule"
  | Rule -> "Rule"
  | RuleOnce -> "RuleOnce"
  | SeqCondition -> "SeqCondition"
  | EmptyCondition -> "EmptyCondition"
  | WireCondition -> "WireCondition"
  | KnownCondition -> "KnownCondition"
  | SeqAction -> "SeqAction"
  | EmptyAction -> "EmptyAction"
  | Send -> "Send"
  | JustifiedSend -> "JustifiedSend"
  | JustifiedSay -> "JustifiedSay"
  | Learn -> "Learn"
  | Forget -> "Forget"
  | Install -> "Install"
  | Uninstall -> "Uninstall"
  | Apply -> "Apply"
  | Drop -> "Drop"
  | EmptyInfon -> "EmptyInfon"
  | AsInfon -> "AsInfon"
  | AndInfon -> "AndInfon"
  | ImpliesInfon -> "ImpliesInfon"
  | SaidInfon -> "SaidInfon"
  | JustifiedInfon -> "JustifiedInfon"
  | EmptyEvidence -> "EmptyEvidence"
  | SignatureEvidence -> "SignatureEvidence"
  | ModusPonensEvidence -> "ModusPonensEvidence"
  | AndEvidence -> "AndEvidence"
  | AsInfonEvidence -> "AsInfonEvidence"
  | RelationInfon r -> strcat "RelationInfon(" 
                       (strcat (printRelationInfon r) ")")

(*let printMono t = match t with
  | Var x -> printVar x
  | Const c -> printConst c
  | SubstrateQueryTerm q -> printSubstrateQuery q
  | SubstrateUpdateTerm u -> raise "unexpected SubstrateUpdate"
  | App f ts -> strcat "(App "
                (strcat (printFunc f)
                (strcat " ["
                (strcat (printList printMono ts "; ")
                 "])"))) *)

val printInfon: p:polyterm -> b:string{(ReprPoly p)=b}
let printInfon p = 
let str = match p with
  | MonoTerm t -> printMono t
  | ForallT xs t -> strcat "(Forall (" 
                    (strcat (printList printVar xs ",")
                    (strcat ") "
                    (strcat (printMono t)
                     ")"))) in
assume ((ReprPoly p) = str);
str

(* ============================ parsing ========================= *)
val parseSubstrateConstant: string -> object
val parseSubstrateQuery: string -> ISubstrateQueryTerm

val parseList: (string -> 'a) -> string -> string -> list 'a

val parseOption: (string -> 'a) -> string -> option 'a

val parseTyp: string -> typ
let parseTyp str =
  if str = "Infon" then Infon
  else if str = "Principal" then Principal
  else if str = "SubstrateUpdate" then SubstrateUpdate
  else if str = "SubstrateQuery" then SubstrateQuery
  else if str = "Action" then Action
  else if str = "Condition" then Condition
  else if str = "RuleT" then RuleT
  else if str = "Evidence" then Evidence
  else if str = "Boolean" then Boolean 
  else if str = "Int32" then Int32
  else if str = "Double" then Double 
  else if str = "String" then String
  else raise (strcat "unexpected Typ: " str)

val parseVar: string -> var
let parseVar str =
  if not(strStartsWith str "Var") 
  then raise (strcat "unexpected var: " str)
  else let indexOfComma = strIndexOf str "," in
  let name = strSubstring str 4 (indexOfComma-4) in
  let indexOfRightPar = strIndexOf str ")" in
  let typ = parseTyp (strSubstring str (indexOfComma+1) (indexOfRightPar-indexOfComma-1)) in
  {name = name; typ = typ}

val parsePrincipal: string -> principal
let parsePrincipal str = str

val parseConst: string -> constant
let parseConst str =
  if str = "TrueT" then TrueT
  else if str = "FalseT" then FalseT
  else if strStartsWith str "SubstrateConstant" then 
    let lastIndexOfrp = strLastIndexOf str ")" in
    let strSubstrate = strSubstring str 18 (lastIndexOfrp - 18) in
    let o = parseSubstrateConstant strSubstrate in
    SubstrateConstant o
  else if strStartsWith str "PrincipalConstant" then
    let lastIndexOfrp = strLastIndexOf str ")" in
    let strPrincipal = strSubstring str 18 (lastIndexOfrp - 18) in
    let p = parsePrincipal strPrincipal in
    PrincipalConstant p
  else raise (strcat "unexpected constant: " str)

val parseRelationInfon: string -> relationInfon
val parseFunc: string -> func
val parseMono: string -> term

let parseRelationInfon str =
 let indexOfComma = strIndexOf str "," in
 let name = strSubstring str 14 (indexOfComma - 14) in
 let rest = strSubstringNoLength str (indexOfComma+1) in (* skip the whitespace *)
 let indexofComma = strIndexOf rest "," in
 let retType = parseTyp (strSubstring rest 0 (indexOfComma-1)) in
 let rest = strSubstringNoLength rest (indexOfComma+2) in 
 let indexOfPar = strIndexOf rest "]" in
 let argsType = parseList parseTyp (strSubstring rest 0 (indexOfPar-1)) ";" in
 let identity = parseOption parseMono (strSubstring rest (indexOfPar+3) (strIndexOf rest ")" - indexOfPar - 3)) in
 {name = name; retType = retType; argsType = argsType; identity = identity}
 

val parsePoly: string -> polyterm

(* Note: As for the printer, this function is recursive and so not in P. *)
val parseInfon: b:string -> option (p:polyterm{(Net.Received b => Net.Received p) &&
                                               (ReprPoly p)=b})
let parseInfon b = 
  let p = 
    if strStartsWith b "(Forall" then parsePoly b 
    else MonoTerm(parseMono b) in
  assume ((Net.Received b => Net.Received p) &&
          (ReprPoly p)=b);
  Some p

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
val b2s: b:bytes -> s:string{Net.Received b => Net.Received s}
