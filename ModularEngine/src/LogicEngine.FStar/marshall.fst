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

let rec printRelationInfon r =
  strcat "RelationInfon("
  (strcat (r.name)
  (strcat ", "
  (strcat (printTyp r.retType)
  (strcat ", ["
  (strcat (printList printTyp r.argsType "; ")
  (strcat "], "
  (strcat (printOption printMono r.identity)
   ")")))))))
 
and printFunc f = match f with
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

and printMono t = match t with
  | Var x -> printVar x
  | Const c -> strcat "Const " (printConst c)
  | SubstrateQueryTerm q -> strcat "SubstrateQuery " (printSubstrateQuery q)
  | SubstrateUpdateTerm u -> raise "unexpected SubstrateUpdate"
  | App f ts -> strcat "(App "
                (strcat (printFunc f)
                (strcat " ["
                (strcat (printList printMono ts "; ")
                 "])"))) 

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

val parseSubstrateConstant: string -> (object * string)
val parseSubstrateQuery: string -> (ISubstrateQueryTerm * string)

val rmPfxStr: string -> string -> string
let rmPfxStr str substr = 
  let str = strTrim str in
  if strStartsWith str substr
  then let n = strLength substr in
    strSubstringNoLength str n
  else raise (strcat str (strcat " does not have prefix " substr))

val getAll: (string -> ('a *string)) -> string -> string -> list 'a -> (list 'a * string)  
let rec getAll parseOne str delimiter ones = 
  let str = strTrim str in
  if strStartsWith str "["  
  then let rest = rmPfxStr str "[" in
    let one, rest = parseOne rest in
    let newones: list 'a = append ones [one] in
    let result: (list 'a * string) = getAll parseOne rest delimiter newones in
    result
  else if strStartsWith str delimiter
  then let rest = rmPfxStr str delimiter in
    let one, rest = parseOne rest in
    let newones = append ones [one] in
    getAll parseOne rest delimiter newones
  else if strStartsWith str "]" 
  then (ones, rmPfxStr str "]") 
  else raise (strcat "unexpected string in parseList: " str)

val parseList: (string -> ('a*string)) -> string -> string -> ((list 'a) * string)
let parseList (parseOne: string -> ('a * string)) (str: string) (delimiter:string) : (list 'a * string) =
  getAll parseOne str delimiter ([]: list 'a)

val parseOption: (string -> ('a*string)) -> string -> (option 'a * string)
let parseOption parseOne str =
  let str = strTrim str in
  if strStartsWith str "None"
  then (None, strSubstringNoLength str 4)
  else if strStartsWith str "Some" 
  then let rest = strSubstringNoLength str 5 in
    let one, rest = parseOne rest in
    let rest = rmPfxStr rest ")" in
    Some one, rest
  else raise (strcat "unexpected string in parseOption: " str)

val parseString: string -> string -> (string * string) 
let parseString str delimiter =
  let index = strIndexOf str delimiter in
  strSubstring str 0 index, strSubstringNoLength str index

val parseTyp: string -> (typ * string)
let parseTyp str =
  let str = strTrim str in
  let rmPfx n = strSubstringNoLength str n in
  if strStartsWith str "Infon" then (Infon, rmPfx 5)
  else if strStartsWith str "Principal" then (Principal, rmPfx 9)
  else if strStartsWith str "SubstrateUpdate" then (SubstrateUpdate, rmPfx 15)
  else if strStartsWith str "SubstrateQuery" then (SubstrateQuery, rmPfx 14)
  else if strStartsWith str "Action" then (Action, rmPfx 6)
  else if strStartsWith str "Condition" then (Condition, rmPfx 9)
  else if strStartsWith str "RuleT" then (RuleT, rmPfx 5)
  else if strStartsWith str "Evidence" then (Evidence, rmPfx 8)
  else if strStartsWith str "Boolean" then (Boolean, rmPfx 7)
  else if strStartsWith str "Int32" then (Int32, rmPfx 5)
  else if strStartsWith str "Double" then (Double, rmPfx 6)
  else if strStartsWith str "String" then (String, rmPfx 6)
  else raise (strcat "unexpected Typ: " str)

val parseVar: string -> (var*string)
let parseVar str =
  let str = strTrim str in
  if not(strStartsWith str "Var") 
  then raise (strcat "unexpected var: " str)
  else let rest = strSubstringNoLength str 4 in
  let name, rest = parseString rest "," in
  let rest = rmPfxStr rest "," in
  let typ, rest = parseTyp rest in
  let rest = rmPfxStr rest ")" in
  {name = name; typ = typ}, rest

val parseConst: string -> (constant*string)
let parseConst str =
  let str = strTrim str in
  let rmPfx n = strSubstringNoLength str n in
  if str = "TrueT" then (TrueT, rmPfx 5)
  else if str = "FalseT" then (FalseT, rmPfx 6)
  else if strStartsWith str "SubstrateConstant" then 
    let rest = rmPfx 18 in
    let o, rest = parseSubstrateConstant rest in
    let rest = rmPfxStr rest ")" in
    SubstrateConstant o, rest
  else if strStartsWith str "PrincipalConstant" then
    let rest = rmPfx 18 in
    let p, rest = parseString rest ")" in
    let rest = rmPfxStr rest ")" in
    PrincipalConstant p, rest
  else raise (strcat "unexpected constant: " str)

val parseRelationInfon: string -> (relationInfon*string)
val parseFunc: string -> (func*string)
val parseMono: string -> (term*string)

let rec parseRelationInfon str =
  let str = strTrim str in
  let rest = strSubstringNoLength str 14 in
  let name, rest = parseString rest "," in
  let rest = rmPfxStr rest "," in
  let retType, rest = parseTyp rest in
  let rest = rmPfxStr rest "," in
  let rest = rmPfxStr rest "[" in
  let argsType, rest = parseList parseTyp rest ";" in
  let rest = rmPfxStr rest "]" in
  let rest = rmPfxStr rest "," in
  let identity, rest = parseOption parseMono rest in
  {name = name; retType = retType; argsType = argsType; identity = identity}, rest
 
and parseFunc str =
  let str = strTrim str in
  let rmPfx n = strSubstringNoLength str n in
  if strStartsWith str "SeqRule" then (SeqRule, rmPfx 7)
  else if strStartsWith str "EmptyRule" then (EmptyRule, rmPfx 9)
  else if strStartsWith str "Rule" then (Rule, rmPfx 4)
  else if strStartsWith str "RuleOnce" then (RuleOnce, rmPfx 8)
  else if strStartsWith str "SeqCondition" then (SeqCondition, rmPfx 12)
  else if strStartsWith str "EmptyCondition" then (EmptyCondition, rmPfx 14)
  else if strStartsWith str "WireCondition" then (WireCondition, rmPfx 13)
  else if strStartsWith str "KnownCondition" then (KnownCondition, rmPfx 14)
  else if strStartsWith str "SeqAction" then (SeqAction, rmPfx 9)
  else if strStartsWith str "EmptyAction" then (EmptyAction, rmPfx 11)
  else if strStartsWith str "Send" then (Send, rmPfx 4)
  else if strStartsWith str "JustifiedSend" then (JustifiedSend, rmPfx 13) 
  else if strStartsWith str "JustifiedSay" then (JustifiedSay, rmPfx 12)
  else if strStartsWith str "Learn" then (Learn, rmPfx 5)
  else if strStartsWith str "Forget" then (Forget, rmPfx 6)
  else if strStartsWith str "Install" then (Install, rmPfx 7)
  else if strStartsWith str "Uninstall" then (Uninstall, rmPfx 9)
  else if strStartsWith str "Apply" then (Apply, rmPfx 5)
  else if strStartsWith str "Drop" then (Drop, rmPfx 4)
  else if strStartsWith str "EmptyInfon" then (EmptyInfon, rmPfx 10)
  else if strStartsWith str "AsInfon" then (AsInfon, rmPfx 7)
  else if strStartsWith str "AndInfon" then (AndInfon, rmPfx 8)
  else if strStartsWith str "ImpliesInfon" then (ImpliesInfon, rmPfx 12)
  else if strStartsWith str "SaidInfon" then (SaidInfon, rmPfx 9)
  else if strStartsWith str "JustifiedInfon" then (JustifiedInfon, rmPfx 14)
  else if strStartsWith str "EmptyEvidence" then (EmptyEvidence, rmPfx 13)
  else if strStartsWith str "SignatureEvidence" then (SignatureEvidence, rmPfx 17)
  else if strStartsWith str "ModusPonensEvidence" then (ModusPonensEvidence, rmPfx 19)
  else if strStartsWith str "AndEvidence" then (AndEvidence, rmPfx 11)
  else if strStartsWith str "AsInfonEvidence" then (AsInfonEvidence, rmPfx 15)
  else if strStartsWith str "RelationInfon"
  then let rest = rmPfx 14 in
    let r, rest = parseRelationInfon rest in
    RelationInfon r, rest 
  else raise (strcat "unexpected Func: " str)

and parseMono str = 
  let str = strTrim str in
  if strStartsWith str "Var" then
    let x, rest = parseVar str in
    Var x, rest
  else if strStartsWith str "Const"
  then let rest = strSubstringNoLength str 5 in
    let c, rest = parseConst rest in
    Const c, rest
  else if strStartsWith str "SubstrateQuery"
  then let rest = strSubstringNoLength str 15 in
    let q , rest = parseSubstrateQuery rest in
    SubstrateQueryTerm q, rest
  else if strStartsWith str "(App" 
  then let rest = strSubstringNoLength str 4 in
    let f, rest = parseFunc rest in
    let rest = rmPfxStr rest "[" in
    let ts, rest = parseList parseMono rest ";" in
    App f ts, rest
  else raise (strcat "unexpected Mono: " str)

val parsePoly: string -> (polyterm*string)
let parsePoly str =
  let str = strTrim str in
  let rest = rmPfxStr str "(" in
  let xs, rest = parseList parseVar rest "," in
  let rest = rmPfxStr rest ")" in
  let t, rest = parseMono rest in
  let rest = rmPfxStr rest ")" in
  ForallT xs t, rest

(* Note: As for the printer, this function is recursive and so not in P. *)
val parseInfon: b:string -> option (p:polyterm{(Net.Received b => Net.Received p) &&
                                               (ReprPoly p)=b})


let parseInfon b = 
  let b = strTrim b in
  let p, rest = 
    if strStartsWith b "(Forall" then parsePoly b 
    else let t, rest = parseMono b in
      MonoTerm t, rest in
  if strLength rest = 0 
  then let _ = assume ((Net.Received b => Net.Received p) &&
                      (ReprPoly p)=b) in
    (*Some p*) raise ""
  else raise (strcat "unexpected remaining string: " rest)
  

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
