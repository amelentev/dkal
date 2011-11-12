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
    (Strcat (r.fname)
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

val printRelationInfon: r:relationInfon -> s:string{(ReprRelationInfon r) = s}
val printFunc: f:func -> s:string{(ReprFunc f) = s}
val printMono: t:term -> s:string{(ReprMono t) = s}
val printMonoOption: o:option term -> s:string{(ReprMonoOption o) = s}
val printMonos: l:list term -> s:string{(ReprMonos l) = s} 

let rec printRelationInfon r =
  strcat "RelationInfon("
  (strcat (r.fname)
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

val parseSubstrateQuery: s:string -> (q:ISubstrateQueryTerm * r1:string * r2:string{((ReprQuery q)=r1 && (Strcat r1 r2) = s)})

(* remove the prefix of a string *)
val rmPfxStr: s:string -> pfx:string -> r:string{(Strcat pfx r) = s}
let rmPfxStr str pfx = 
  if strStartsWith str pfx
  then strRmPfx str pfx
  else raise (strcat str (strcat " does not have prefix " pfx))

val parseTyp: str:string -> (t:typ * r1:string * r2:string{(ReprTyp t) = r1 && (Strcat r1 r2) = str})
let parseTyp str =
  if strStartsWith str "Infon" then (Infon, "Infon", strRmPfx str "Infon")
  else if strStartsWith str "Principal" then (Principal, "Principal", strRmPfx str "Principal")
  else if strStartsWith str "SubstrateUpdate" then (SubstrateUpdate, "SubstrateUpdate", strRmPfx str "SubstrateUpdate")
  else if strStartsWith str "SubstrateQuery" then (SubstrateQuery, "SubstrateQuery", strRmPfx str "SubstrateQuery")
  else if strStartsWith str "Action" then (Action, "Action", strRmPfx str "Action")
  else if strStartsWith str "Condition" then (Condition, "Condition", strRmPfx str "Condition")
  else if strStartsWith str "RuleT" then (RuleT, "RuleT", strRmPfx str "RuleT")
  else if strStartsWith str "Evidence" then (Evidence, "Evidence", strRmPfx str "Evidence")
  else if strStartsWith str "Boolean" then (Boolean, "Boolean", strRmPfx str "Boolean")
  else if strStartsWith str "Int32" then (Int32, "Int32", strRmPfx str "Int32")
  else if strStartsWith str "Double" then (Double, "Double", strRmPfx str "Double")
  else if strStartsWith str "String" then (String, "String", strRmPfx str "String")
  else raise (strcat "unexpected Typ: " str)

val parseTyps: str:string -> (ts: list typ * r1:string * r2:string{(ReprTyps ts) = r1 && (Strcat r1 r2) = str})
let rec parseTyps str =
  if strStartsWith str "]" then ([], "", str)
  else let hd, r_hd, rest = parseTyp str in
    let rest = strRmPfx rest ";" in
    let tl, r_tl, rest = parseTyps rest in
    (hd::tl), strcat r_hd (strcat ";" r_tl), rest

val parseVar: str:string -> (v:var * r1:string * r2:string{(ReprVar v) = r1 && (Strcat r1 r2) = str})
let parseVar str =
  if not(strStartsWith str "Var") 
  then raise (strcat "unexpected var: " str)
  else let rest = strRmPfx str "Var(" in
  let name, rest = strSplitByDelimiter rest "," in
  let rest = strRmPfx rest "," in
  let typ, r_typ, rest = parseTyp rest in
  let rest = strRmPfx rest ")" in
  {name = name; typ = typ}, strcat "Var(" (strcat name (strcat "," (strcat r_typ ")"))), rest

val parseVars: str:string -> (vs: list var * r1:string * r2:string{(ReprVars vs) = r1 && (Strcat r1 r2) = str})
let rec parseVars str =
  if strStartsWith str "]"
  then ([], "", str)
  else let v, r_v, rest = parseVar str in
    let rest = strRmPfx rest ";" in
    let tl, r_tl, rest = parseVars rest in
    (v::tl), (strcat r_v (strcat ";" r_tl)), rest

val parseConst: str:string -> (c:constant * r1:string * r2:string{(ReprConst c) = r1 && (Strcat r1 r2) = str})
let rec parseConst str =
  if strStartsWith str "TrueT" then (TrueT, "TrueT", strRmPfx str "TrueT")
  else if strStartsWith str "FalseT" then (FalseT, "FalseT", strRmPfx str "FalseT")
  else if strStartsWith str "Int(" then 
    let rest = strRmPfx str "Int(" in
    let r_n, rest = strSplitByDelimiter rest ")" in
    let rest = strRmPfx rest ")" in
    (Int (stringToInt r_n)), strcat "Int(" (strcat r_n ")"), rest
  else if strStartsWith str "SubstrateConstant(" then 
    let rest = strRmPfx str "SubstrateConstant(" in
    let o, r_o, rest = parseConst rest in
    let rest = strRmPfx rest ")" in
    SubstrateConstant o, strcat "SubstrateConstant(" (strcat r_o ")"), rest
  else if strStartsWith str "PrincipalConstant(" then
    let rest = strRmPfx str "PrincipalConstant(" in
    let p, rest = strSplitByDelimiter rest ")" in
    let rest = rmPfxStr rest ")" in
    PrincipalConstant p, strcat "PrincipalConstant(" (strcat p ")"), rest
  else raise (strcat "unexpected constant: " str)

val parseRelationInfon: str:string -> (r:relationInfon * r1:string * r2:string{(ReprRelationInfon r)=r1 && (Strcat r1 r2) = str})
val parseFunc: str:string -> (f:func * r1:string * r2:string{(ReprFunc f) = r1 && (Strcat r1 r2) = str})
val parseMono: str:string -> (t:term * r1:string * r2:string{(ReprMono t) = r1 && (Strcat r1 r2) = str})
val parseMonoOption: str:string -> (t:option term * r1:string * r2:string{(ReprMonoOption t) = r1 && (Strcat r1 r2)=str})
val parseMonos: str:string -> (ts:list term * r1:string * r2:string{(ReprMonos ts)=r1 && (Strcat r1 r2) = str})

let rec parseRelationInfon str = 
  let rest1 = strRmPfx str "RelationInfon(" in
  let _ = assert ((Strcat "RelationInfon(" rest1)=str) in
  let namer, rest2 = strSplitByDelimiter rest1 "," in
  let rest3 = strRmPfx rest2 "," in
  let retType, r_rt, rest4 = parseTyp rest3 in
  let rest5 = strRmPfx rest4 ",[" in
  let argsType, r_argts, rest6 = parseTyps rest5 in
  let rest7 = strRmPfx rest6 "]," in
  let identity, r_id, rest8 = parseMonoOption rest7 in
  let rest9 = strRmPfx rest8 ")" in
  let r = {fname = namer; retType = retType; argsType = argsType; identity = identity} in
  let _ = assert (namer = r.fname) in
  let _ = assert ((ReprRelationInfon r) =
                  (Strcat "RelationInfon(" 
                   (Strcat (namer)
                   (Strcat ","
                   (Strcat (r_rt)
                   (Strcat ",["
                   (Strcat (ReprTyps (r.argsType))
                   (Strcat "],"
                   (Strcat (ReprMonoOption (r.identity)) ")"))))))))) in
  let r_r = 
  strcat "RelationInfon("
   (strcat namer 
   (strcat "," 
   (strcat r_rt 
   (strcat ",["
   (strcat r_argts
   (strcat "],"
   (strcat r_id ")"))))))) in 
  r, r_r, rest9

and parseFunc str =
  if strStartsWith str "SeqRule" then (SeqRule, "SeqRule", strRmPfx str "SeqRule")
  else if strStartsWith str "EmptyRule" then (EmptyRule, "EmptyRule", strRmPfx str "EmptyRule")
  else if strStartsWith str "Rule" then (Rule, "Rule", strRmPfx str "Rule")
  else if strStartsWith str "RuleOnce" then (RuleOnce, "RuleOnce", strRmPfx str "RuleOnce")
  else if strStartsWith str "SeqCondition" then (SeqCondition, "SeqCondition", strRmPfx str "SeqCondition")
  else if strStartsWith str "EmptyCondition" then (EmptyCondition, "EmptyCondition", strRmPfx str "EmptyCondition")
  else if strStartsWith str "WireCondition" then (WireCondition, "WireCondition", strRmPfx str "WireCondition")
  else if strStartsWith str "KnownCondition" then (KnownCondition, "KnownCondition", strRmPfx str "KnownCondition")
  else if strStartsWith str "SeqAction" then (SeqAction, "SeqAction", strRmPfx str "SeqAction")
  else if strStartsWith str "EmptyAction" then (EmptyAction, "EmptyAction", strRmPfx str "EmptyAction")
  else if strStartsWith str "Send" then (Send, "Send", strRmPfx str "Send")
  else if strStartsWith str "JustifiedSend" then (JustifiedSend, "JustifiedSend", strRmPfx str "JustifiedSend") 
  else if strStartsWith str "JustifiedSay" then (JustifiedSay, "JustifiedSay", strRmPfx str "JustifiedSay")
  else if strStartsWith str "Learn" then (Learn, "Learn", strRmPfx str "Learn")
  else if strStartsWith str "Forget" then (Forget, "Forget", strRmPfx str "Forget")
  else if strStartsWith str "Install" then (Install, "Install", strRmPfx str "Install")
  else if strStartsWith str "Uninstall" then (Uninstall, "Uninstall", strRmPfx str "Uninstall")
  else if strStartsWith str "Apply" then (Apply, "Apply", strRmPfx str "Apply")
  else if strStartsWith str "Drop" then (Drop, "Drop", strRmPfx str "Drop")
  else if strStartsWith str "EmptyInfon" then (EmptyInfon, "EmptyInfon", strRmPfx str "EmptyInfon")
  else if strStartsWith str "AsInfon" then (AsInfon, "AsInfon", strRmPfx str "AsInfon")
  else if strStartsWith str "AndInfon" then (AndInfon, "AndInfon", strRmPfx str "AndInfon")
  else if strStartsWith str "ImpliesInfon" then (ImpliesInfon, "ImpliesInfon", strRmPfx str "ImpliesInfon")
  else if strStartsWith str "SaidInfon" then (SaidInfon, "SaidInfon", strRmPfx str "SaidInfon")
  else if strStartsWith str "JustifiedInfon" then (JustifiedInfon, "JustifiedInfon", strRmPfx str "JustifiedInfon")
  else if strStartsWith str "EmptyEvidence" then (EmptyEvidence, "EmptyEvidence", strRmPfx str "EmptyEvidence")
  else if strStartsWith str "SignatureEvidence" then (SignatureEvidence, "SignatureEvidence", strRmPfx str "SignatureEvidence")
  else if strStartsWith str "ModusPonensEvidence" then (ModusPonensEvidence, "ModusPonensEvidence", strRmPfx str "ModusPonensEvidence")
  else if strStartsWith str "AndEvidence" then (AndEvidence, "AndEvidence", strRmPfx str "AndEvidence")
  else if strStartsWith str "AsInfonEvidence" then (AsInfonEvidence, "AsInfonEvidence", strRmPfx str "AsInfonEvidence")
  else if strStartsWith str "RelationInfon"
  then let r, r_r, rest = parseRelationInfon str in
    RelationInfon r, r_r, rest 
  else raise (strcat "unexpected Func: " str)

and parseMono str = 
  if strStartsWith str "Var" then
    let x, r_x, rest = parseVar str in
    Var x, r_x, rest
  else if strStartsWith str "Const "
  then let rest = strRmPfx str "Const " in
    let c, r_c, rest = parseConst rest in
    Const c, strcat "Const " r_c, rest
  else if strStartsWith str "SubstrateQuery "
  then let rest = strRmPfx str "SubstrateQuery " in
    let q, r_q, rest = parseSubstrateQuery rest in
    SubstrateQueryTerm q, strcat "SubstrateQuery " r_q, rest
  else if strStartsWith str "(App " 
  then let rest = strRmPfx str "(App " in
    let f, r_f, rest = parseFunc rest in
    let rest = strRmPfx rest ",[" in
    let ts, r_ts, rest = parseMonos rest in
    let rest = strRmPfx rest "])" in
    App f ts, strcat "(App " (strcat r_f (strcat ",[" (strcat r_ts "])"))), rest
  else raise (strcat "unexpected Mono: " str)

and parseMonoOption str =
  if strStartsWith str "None" then None, "None", strRmPfx str "None"
  else if strStartsWith str "Some(" then
    let rest = strRmPfx str "Some(" in
    let t, r_t, rest = parseMono rest in
    let rest = strRmPfx rest ")" in
    Some t, strcat "Some(" (strcat r_t ")"), rest
  else raise (strcat "unexpected option: " str)

and parseMonos str =
  if strStartsWith str "]" then [], "", str
  else let hd, r_hd, rest = parseMono str in
    let rest = strRmPfx rest ";" in
    let tl, r_tl, rest = parseMonos rest in
    (hd::tl), strcat r_hd (strcat ";" r_tl), rest

val parsePoly: str:string -> (p:polyterm * r1:string * r2:string{(ReprPoly p) = r1 && (Strcat r1 r2)= str})
let parsePoly str =
  let rest = strRmPfx str "(Forall [" in
  let xs, r_xs, rest = parseVars rest in
  let rest = strRmPfx rest "]" in
  let t, r_t, rest = parseMono rest in
  let rest = strRmPfx rest ")" in
  ForallT xs t, strcat "(Forall [" (strcat r_xs (strcat "]" (strcat r_t ")"))), rest

(* Note: As for the printer, this function is recursive and so not in P. *)
val parseInfon: b:string -> option (p:polyterm{(Net.Received b => Net.Received p) &&
                                               (ReprPoly p)=b})

let parseInfon b = 
 if strStartsWith b "(Forall" 
 then let p, r_p, rest = parsePoly b in
   if r_p = b 
   then (assume (Net.Received b => Net.Received p); Some p)
   else raise (strcat "unexpected remaining string: " rest)
 else let t, r_t, rest = parseMono b in
   if r_t = b 
   then let p = MonoTerm t in (assume (Net.Received b => Net.Received p); Some p)
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
