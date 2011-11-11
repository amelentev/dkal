module Marshall
open Types

logic function ReprQuery: ISubstrateQueryTerm -> string

logic function ReprTyp: typ -> string
assume ((ReprTyp Infon) = "Infon")
assume ((ReprTyp Principal) = "Principal")
assume ((ReprTyp SubstrateUpdate) = "SubstrateUpdate")
assume ((ReprTyp SubstrateQuery) = "SubstrateQuery")
assume ((ReprTyp Action) = "Action")
assume ((ReprTyp Condition) = "Condition")
assume ((ReprTyp RuleT) = "RuleT")
assume ((ReprTyp Evidence) = "Evidence")
assume ((ReprTyp Boolean) = "Boolean")
assume ((ReprTyp Int32) = "Int32")
assume ((ReprTyp Double) = "Double")
assume ((ReprTyp String) = "String")

logic function ReprVar : var -> string
assume forall (name:string) (typ:typ).
  ((ReprVar ({name=name;typ=typ})) = (Strcat "Var("
                                     (Strcat name
                                     (Strcat ","
                                     (Strcat (ReprTyp typ)
                                      ")")))))

logic function ReprVars: list var -> string
assume ((ReprVars []) = "")
assume forall (hd:var) (tl: list var).
    ((ReprVars (hd::tl)) = (Strcat (ReprVar hd)
                           (Strcat ";"
                           (ReprVars tl )))) 

logic function ReprConst: constant -> string
assume ((ReprConst TrueT) = "TrueT")
assume ((ReprConst FalseT) = "FalseT")
assume forall (i:int).
  ((ReprConst (Int i)) = (Strcat "Int("
                         (Strcat (ReprInt i)
                          ")")))
assume forall (o:constant).
  ((ReprConst (SubstrateConstant o)) = (Strcat "SubstrateConstant("
                                       (Strcat (ReprConst o)
                                        ")")))
assume forall (p:principal).
  ((ReprConst (PrincipalConstant p)) = (Strcat "PrincipalConstant("
                                       (Strcat p ")")))

logic function ReprTyps: list typ -> string
assume ((ReprTyps []) = "")
assume forall (hd:typ) (tl: list typ).
  ((ReprTyps (hd::tl)) = (Strcat (ReprTyp hd)
                         (Strcat ";"
                         (ReprTyps tl))))

logic function ReprRelationInfon: relationInfon -> string
logic function ReprFunc: func -> string
logic function ReprMono : term -> string
logic function ReprMonoOption: option term -> string
logic function ReprMonos: list term -> string

assume forall (r:relationInfon). 
   ((ReprRelationInfon r) = 
    (Strcat "RelationInfon("
    (Strcat (r.name)
    (Strcat ","
    (Strcat (ReprTyp (r.retType))
    (Strcat ",["
    (Strcat (ReprTyps (r.argsType))
    (Strcat "],"
    (Strcat (ReprMonoOption (r.identity))
    ")")))))))))

assume ((ReprFunc SeqRule) = "SeqRule")
assume ((ReprFunc EmptyRule) = "EmptyRule")
assume ((ReprFunc Rule) = "Rule")
assume ((ReprFunc RuleOnce) = "RuleOnce")
assume ((ReprFunc SeqCondition) = "SeqCondition")
assume ((ReprFunc EmptyCondition) = "EmptyCondition")
assume ((ReprFunc WireCondition) = "WireCondition")
assume ((ReprFunc KnownCondition) = "KnownCondition")
assume ((ReprFunc SeqAction) = "SeqAction")
assume ((ReprFunc EmptyAction) = "EmptyAction")
assume ((ReprFunc Send) = "Send")
assume ((ReprFunc JustifiedSend) = "JustifiedSend")
assume ((ReprFunc JustifiedSay) = "JustifiedSay")
assume ((ReprFunc Learn) = "Learn")
assume ((ReprFunc Forget) = "Forget")
assume ((ReprFunc Install) = "Install")
assume ((ReprFunc Uninstall) = "Uninstall")
assume ((ReprFunc Apply) = "Apply")
assume ((ReprFunc Drop) = "Drop")
assume ((ReprFunc EmptyInfon) = "EmptyInfon")
assume ((ReprFunc AsInfon) = "AsInfon")
assume ((ReprFunc AndInfon) = "AndInfon")
assume ((ReprFunc ImpliesInfon) = "ImpliesInfon")
assume ((ReprFunc SaidInfon) = "SaidInfon")
assume ((ReprFunc JustifiedInfon) = "JustifiedInfon")
assume ((ReprFunc EmptyEvidence) = "EmptyEvidence")
assume ((ReprFunc SignatureEvidence) = "SignatureEvidence")
assume ((ReprFunc ModusPonensEvidence) = "ModusPonensEvidence")
assume ((ReprFunc AndEvidence) = "AndEvidence")
assume ((ReprFunc AsInfonEvidence) = "AsInfonEvidence")
assume forall (r:relationInfon).
  ((ReprFunc (RelationInfon r)) = (ReprRelationInfon r))

assume forall (x:var).
  ((ReprMono (Var x)) = (ReprVar x))
assume forall (c:constant).
  ((ReprMono (Const c)) = (Strcat "Const " (ReprConst c)))
assume forall (q:ISubstrateQueryTerm).
  ((ReprMono (SubstrateQueryTerm q)) =
   (Strcat "SubstrateQuery " (ReprQuery q)))
assume forall (f:func) (ts:list term).
  ((ReprMono (App f ts)) =
   (Strcat "(App "
   (Strcat (ReprFunc f)
   (Strcat ",["
   (Strcat (ReprMonos ts)
   "])")))))                           

assume ((ReprMonoOption None) = "None")
assume forall (t:term).
  ((ReprMonoOption (Some t)) = (Strcat "Some("
                               (Strcat (ReprMono t)
                               ")")))

assume ((ReprMonos []) = "")
assume forall (hd:term) (tl:list term).
  ((ReprMonos (hd::tl)) = (Strcat (ReprMono hd)
                        (Strcat ";"
                        (ReprMonos tl))))

logic function ReprPoly : polyterm -> string
assume forall (xs:vars) (t:term). 
           (ReprPoly (ForallT xs t)) = (Strcat "(Forall ["
                                       (Strcat (ReprVars xs)
                                       (Strcat "]"
                                       (Strcat (ReprMono t)
                                               ")"))))

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

val printSubstrateQuery: q:ISubstrateQueryTerm -> s:string{(ReprQuery q)=s}

val printTyp: t:typ -> s:string{(ReprTyp t) = s}
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

val printVar: v:var -> s:string{(ReprVar v) = s}
let printVar v = strcat "Var("
                 (strcat (v.name)
                 (strcat ","
                 (strcat (printTyp v.typ)
                  ")")))

val printVars: l:list var -> s:string{(ReprVars l)=s}
let rec printVars l =
  match l with
    | [] -> ""
    | v::tl -> strcat (printVar v)
               (strcat ";"
               (printVars tl))

val printConst: c:constant -> s:string{(ReprConst c) = s}
let rec printConst c = match c with
  | TrueT -> "TrueT"
  | FalseT -> "FalseT"
  | Int i -> strcat "Int(" (strcat (intToString i) ")")
  | SubstrateConstant o -> strcat "SubstrateConstant("
                           (strcat (printConst o) ")")
  | PrincipalConstant p ->
    strcat "PrincipalConstant(" (strcat p ")")

val printTyps: l:list typ -> s:string{(ReprTyps l) = s}
let rec printTyps l =
  match l with
    | [] -> ""
    | hd::tl -> strcat (printTyp hd)
                (strcat ";"
                (printTyps tl))

(*val printRelationInfon: r:relationInfon -> string
val printFunc: f:func -> string
val printMono: t:term -> string
val printMonoOption: o:option term -> string
val printMonos: l:list term -> string
*)

val printRelationInfon: r:relationInfon -> s:string{(ReprRelationInfon r) = s}
val printFunc: f:func -> s:string{(ReprFunc f) = s}
val printMono: t:term -> s:string{(ReprMono t) = s}
val printMonoOption: o:option term -> s:string{(ReprMonoOption o) = s}
val printMonos: l:list term -> s:string{(ReprMonos l) = s} 

let rec printRelationInfon r =
  strcat "RelationInfon("
  (strcat (r.name)
  (strcat ","
  (strcat (printTyp r.retType)
  (strcat ",["
  (strcat (printTyps r.argsType)
  (strcat "],"
  (strcat (printMonoOption r.identity)
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
  | RelationInfon r -> printRelationInfon r

and printMono t = match t with
  | Var x -> printVar x
  | Const c -> strcat "Const " (printConst c)
  | SubstrateQueryTerm q -> strcat "SubstrateQuery " (printSubstrateQuery q)
  | SubstrateUpdateTerm u -> raise "unexpected SubstrateUpdate"
  | App f ts -> strcat "(App "
                (strcat (printFunc f)
                (strcat ",["
                (strcat (printMonos ts)
                 "])"))) 

and printMonoOption (opt: option term) =
  match opt with
    | None -> "None"
    | Some t -> strcat "Some("
                (strcat (printMono t)
                 ")")

and printMonos (l: list term) =
  match l with
    | []  -> ""
    | hd::tl -> strcat (printMono hd)
                (strcat ";"
                (printMonos tl))

val printInfon: p:polyterm -> b:string{(ReprPoly p)=b}
let printInfon p = 
 match p with
  | MonoTerm t -> printMono t
  | ForallT xs t -> strcat "(Forall [" 
                    (strcat (printVars xs)
                    (strcat "]"
                    (strcat (printMono t)
                     ")")))


(* ============================ parsing ========================= *)

val parseSubstrateQuery: string -> (ISubstrateQueryTerm * string)

(* remove the prefix of a string *)
val rmPfxStr: s:string -> pfx:string -> string
let rmPfxStr str pfx = 
  if strStartsWith str pfx
  then let n = strLength pfx in
    strSubstringNoLength str n
  else raise (strcat str (strcat " does not have prefix " pfx))

val getAll: (string -> ('a *string)) -> string -> string -> list 'a -> (list 'a * string)  
let rec getAll parseOne str delimiter ones = 
  if strStartsWith str delimiter
  then getAll parseOne (rmPfxStr str delimiter) delimiter ones 
  else if strStartsWith str "]" 
  then (ones, str) 
  else 
    let one, rest = parseOne str in
    let newones = append ones [one] in
    getAll parseOne rest delimiter newones

val parseList: (string -> ('a*string)) -> string -> string -> ((list 'a) * string)
let parseList (parseOne: string -> ('a * string)) (str: string) (delimiter:string) : (list 'a * string) =
  getAll parseOne str delimiter ([]: list 'a)

val parseOption: (string -> ('a*string)) -> string -> (option 'a * string)
let parseOption parseOne str =
  if strStartsWith str "None"
  then (None, strSubstringNoLength str 4)
  else if strStartsWith str "Some"  (* Some(...) *)
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
  if not(strStartsWith str "Var") 
  then raise (strcat "unexpected var: " str)
  else let rest = strSubstringNoLength str 4 in
  let name, rest = parseString rest "," in
  let rest = rmPfxStr rest "," in
  let typ, rest = parseTyp rest in
  let rest = rmPfxStr rest ")" in
  {name = name; typ = typ}, rest

val parseConst: string -> (constant*string)
let rec parseConst str =
  let rmPfx n = strSubstringNoLength str n in
  if strStartsWith str "TrueT" then (TrueT, rmPfx 5)
  else if strStartsWith str "FalseT" then (FalseT, rmPfx 6)
  else if strStartsWith str "Int" then 
    let rest = rmPfx 4 in
    let indexOfRP = strIndexOf rest ")" in
    (Int (stringToInt (strSubstring rest 0 indexOfRP)), strSubstringNoLength rest (indexOfRP+1))
  else if strStartsWith str "SubstrateConstant(" then 
    let rest = rmPfx 18 in
    let o, rest = parseConst rest in
    let rest = rmPfxStr rest ")" in
    SubstrateConstant o, rest
  else if strStartsWith str "PrincipalConstant(" then
    let rest = rmPfx 18 in
    let p, rest = parseString rest ")" in
    let rest = rmPfxStr rest ")" in
    PrincipalConstant p, rest
  else raise (strcat "unexpected constant: " str)

val parseRelationInfon: string -> (relationInfon*string)
val parseFunc: string -> (func*string)
val parseMono: string -> (term*string)

let rec parseRelationInfon str = (* RelationInfon(name, typ, [;], identity) *)
  let rest = strSubstringNoLength str 14 in
  let name, rest = parseString rest "," in
  let rest = rmPfxStr rest "," in
  let retType, rest = parseTyp rest in
  let rest = rmPfxStr rest ",[" in
  let argsType, rest = parseList parseTyp rest ";" in
  let rest = rmPfxStr rest "]," in
  let identity, rest = parseOption parseMono rest in
  let rest = rmPfxStr rest ")" in
  {name = name; retType = retType; argsType = argsType; identity = identity}, rest
 
and parseFunc str =
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
  then let r, rest = parseRelationInfon str in
    RelationInfon r, rest 
  else raise (strcat "unexpected Func: " str)

and parseMono str = 
  if strStartsWith str "Var" then
    let x, rest = parseVar str in
    Var x, rest
  else if strStartsWith str "Const "
  then let rest = strSubstringNoLength str 6 in
    let c, rest = parseConst rest in
    Const c, rest
  else if strStartsWith str "SubstrateQuery "
  then let rest = strSubstringNoLength str 15 in
    let q , rest = parseSubstrateQuery rest in
    SubstrateQueryTerm q, rest
  else if strStartsWith str "(App " 
  then let rest = strSubstringNoLength str 5 in
    let f, rest = parseFunc rest in
    let rest = rmPfxStr rest ",[" in
    let ts, rest = parseList parseMono rest ";" in
    let rest = rmPfxStr rest "])" in
    App f ts, rest
  else raise (strcat "unexpected Mono: " str)

val parsePoly: string -> (polyterm*string)
let parsePoly str =
  let rest = rmPfxStr str "(Forall [" in
  let xs, rest = parseList parseVar rest "," in
  let rest = rmPfxStr str "]" in
  let t, rest = parseMono rest in
  let rest = rmPfxStr rest ")" in
  ForallT xs t, rest

(* Note: As for the printer, this function is recursive and so not in P. *)
val parseInfon: b:string -> option (p:polyterm{(Net.Received b => Net.Received p) &&
                                               (ReprPoly p)=b})


let parseInfon b = 
  let p, rest = 
    if strStartsWith b "(Forall" then parsePoly b 
    else let t, rest = parseMono b in
      MonoTerm t, rest in
  if strLength rest = 0 
  then assume ((Net.Received b => Net.Received p) &&
                      (ReprPoly p)=b);
    Some p
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
