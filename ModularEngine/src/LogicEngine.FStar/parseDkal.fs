#light "off"
module DkalParser

open System

open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

let pr = Printf.printf 
let spr = Printf.sprintf 
let dir = ref ""

let curpos : Parser<Position, 'a> = fun s -> 
    Reply(s.Position, s)


(* Utility combinators *)
let u2b : Parser<unit, 'a> -> Parser<bool, 'a> = fun pu state -> 
    let reply = pu state in 
    Reply(reply.Status=Ok, reply.State)
let a2u (p:Parser<'a, 'b>) : Parser<unit, 'b> =
    p >>. preturn ()

let pfail : Parser<'a,'b> = fun state -> (Reply(Error, expectedError "", state))
let perr msg : Parser<'a,'b> = fun state -> (Printf.printf "%s" msg; Reply(Error, expectedError msg, state))
let prestore msg state : Parser<'a,'b> = fun _ -> Reply(Error, expectedError msg, state)
let prevCharMatches c = u2b (previousCharSatisfies (fun c' -> c=c'))
let currentCharMatches c = u2b (currentCharSatisfies (fun c' -> c=c'))
let nextCharMatches c = u2b (nextCharSatisfies (fun c' -> c=c'))
let peekMatches s = u2b (followedByString s)
let nextToken p = u2b (followedBy p)

let peekn n (s:State<'a>) =
  let rec aux m out = 
    if m>=n then out
    else aux (m+1) (Printf.sprintf "%s%c" out (s.Iter.Peek(m)))
  in
    aux 0 ""

(* If p matches, then parse p and consume it.
   Otherwise, leave stream unchanged and return unit. 
   Always succeeds. *)
let ifNextToken p q r =
    (nextToken p) >>= (fun b -> 
    if b then p >>= q else r)

let peekIfNextToken (p:Parser<'a,'b>) q r = fun s -> 
    let reply = p s in 
        match reply.Status with 
            | Ok -> q s
            | _ -> r s

let peekIfNextTokenMsg (p:Parser<'a,'b>) q r = fun s -> 
  let reply = p s in 
    match reply.Status with 
      | Ok -> q (peekn 5 s) s
      | _ -> r (peekn 5 s) s
          
(* Backtracking *)
let bt msg (p:Parser<'a,'b>) = fun s -> 
    let reply = p s in 
    match reply.Status with 
        | FParsec.Primitives.Error -> 
            Reply(reply.Status, reply.Error, s) 
        | _ -> 
            reply
              
let btOpt msg (p:Parser<'a,'b>) = fun s -> 
    let reply = p s in 
    match reply.Status with 
        | Ok -> Reply(Some reply.Result, reply.State)
        | FParsec.Primitives.Error -> 
            (* pr "Backtracking: %s\n" msg;  *)
            Reply(None, s)
        | status -> Reply(status, reply.Error, reply.State)

let btOptList msg (p :Parser<list<'a>, 'b>) = 
    (btOpt msg p) >>= (fun ol -> 
    match ol with 
        | Some l -> preturn l
        | None -> preturn [])

let cstart : Parser<unit, 'a> = fun s -> s |> ((skipString "(*") |>> ignore)
let cend : Parser<unit, 'a> = fun s -> s |> ((skipString "*)") |>> ignore)
    
let rec comment : Parser<unit, 'a> = fun s -> s |>
    (cstart >>. comment_end)
and comment_end : Parser<unit, 'a> = fun s -> s |>
    ((manyCharsTill anyChar (lookAhead (cstart <|> cend))) >>.
     (cend  <|> (comment >>. comment_end)))

let rec ws = fun s -> s |>
begin
    spaces >>. (btOpt "comment" comment) >>= (function None -> preturn ()
                                           | _ -> ws)
end

let wsUntil (c:Parser<'a,'b>) = fun s ->
 let reply1 = ws s in
 let reply2 = c reply1.State in
 match reply2.Status with 
    | Ok -> reply1
    | _ -> Reply(reply2.Status, reply2.Error, reply1.State)
    
let ch  c = skipChar c >>. ws
let str s = skipString s >>. ws

let keywords = ["upon";
                "eval";
                "using";
                "and";
                "as";
                "of";
                "then";
                "if";
                "said";
                "fwd";
                "add";
                "drop";
                "learn";
                "with";
                "fresh";
                "Ev";
                "where";
                "pubkey";
                "privkey";
                "port"]

let keywordSet = System.Collections.Generic.HashSet<_> keywords

let isIdentStartChar x = isLower x || isUpper x || (x ='_') 
let isIdentChar x = isLower x || isUpper x || isDigit x || (x = '_') || (x = '\'') 
let firstIDENT = fun s -> s |> ((satisfy isIdentStartChar) >>= (fun c -> preturn (String.of_char c)))

let IDENT startChar restChar : Parser<string, 'a> = fun state ->
    let idStr = satisfy startChar >>= (fun c -> 
                let first = String.of_char c in 
                opt (many1Satisfy restChar) >>= (fun restOpt -> 
                  match restOpt with                                                   
                    | None -> preturn first
                    | Some rest -> preturn (first^rest))) .>> ws in (* [_a-zA-Z][a-zA-Z0-9'_]*  *)
    let reply = idStr state in
    if reply.Status = Ok then
        let id = reply.Result in
        if (not (keywordSet.Contains(id))) && (id <> "_") then
            Reply(reply.Result, reply.State)
        else
            Reply(Error, expectedError "identifier", state)
    else // reconstruct error
        Reply(reply.Status, reply.Error, reply.State)

let ANYIDENT s = IDENT isIdentStartChar isIdentChar s
let varIdent = IDENT isLower isIdentChar
let relationIdent = IDENT isUpper isIdentChar
let principalIdent = IDENT (fun x -> x='_') (fun x -> isUpper x || (x = '_') || isDigit x)

type tok<'a> = Parser<unit,'a>
let tok s : tok<'a> = fun st -> st |> 
begin
    (skipString s) >>. 
    (u2b (currentCharSatisfies isIdentStartChar)) >>= 
    (fun b -> if b then prestore "not token" st else ws)
end
      
let FORALL    st = tok "forall" st 
let AND       st = tok "and" st 
let IMPLIES   st = tok "=>" st 
let AS        st = tok "as" st 
let IF        st = tok "if"  st 
let THEN      st = tok "then"  st 
let TRUE      st = tok "true"  st 
let WITH      st = tok "with"  st 
let UPON      st = tok "upon"  st 
let JUSTIFIED st = tok "justified"  st 
let SAID      st = tok "said"  st 
let SEND      st = tok "send"  st 
let FWD       st = tok "fwd"  st 
let ADD       st = tok "add"  st 
let DROP      st = tok "drop"  st 
let LEARN     st = tok "learn"  st 
let FRESH     st = tok "fresh"  st 
let EVIDENCE st = tok "Ev"  st 
let WHERE     st = tok "where"  st 
let USING     st = tok "using"  st 
let OF        st = tok "of"  st 
let PUBKEY    st = tok "pubkey"  st 
let PRIVKEY   st = tok "privkey"  st 
let PORT      st = tok "port"  st 
let INT       st = tok "int"  st 
let BOOL      st = tok "bool"  st 
let STRING_T  st = tok "string"  st 
let PRINCIPAL st = tok "prin"  st 
let INFON     st = tok "infon"  st 
let EVIDENCE_T st = tok "ev"  st 
let EVAL      st = tok "eval"  st 

let DOUBLE_QUOTE st = skipString "\"" st 
let QUOTE     st = skipString "'"  st 

type stok<'a> = Parser<string,'a>
let stok s : stok<'a> = pstring s .>> ws
let LBRACE    st = stok "{"  st 
let RBRACE    st = stok "}"  st 
let LPAREN    st = stok "("  st 
let RPAREN    st = stok ")"  st 
let COMMA     st = stok ","  st 
let RARROW    st = stok "->"  st 
let RRARROW   st = stok "=>"  st 
let RQUOTE    st = stok "'"  st
let DOT       st = stok "."  st
let EQUALS    st = stok "="  st 
let DOLLAR    st = stok "$"  st 
let SEMICOLON st = stok ";"  st 

let STRING : Parser<string, 'a> = fun s ->
    let isStringChar x = not (x = '\n' || x= '\r' || x='"' || x='\\') in
    let rec not_string_end : Parser<string, 'a> = 
         opt (many1Satisfy isStringChar) >>= (fun init -> 
         currentCharMatches '\\' >>=  (fun b -> 
         if b (* escape char *) 
         then (anyChar >>. anyChar >>= (fun escaped ->  (* TODO: Handle other escape characters *)
               not_string_end >>= (fun rest -> 
                                    let tl = (String.of_char escaped) ^ rest in
                                      preturn (match init with None -> tl | Some hd -> hd ^ tl))))
         else
           currentCharMatches '"' >>= (fun b -> 
           if b then preturn (match init with None -> "" | Some i -> i)
           else perr "unexpected char in string literal"))) in
    (DOUBLE_QUOTE >>. not_string_end .>> DOUBLE_QUOTE .>> ws) s
    
let numberFormat =     NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent

let numberLit s = (numberLiteral numberFormat "number" .>> ws) s

type token = 
  | D_FORALL 
  | D_AND
  | D_IMPLIES
  | D_AS
  | D_IF
  | D_THEN
  | D_WITH
  | D_UPON
  | D_SAID
  | D_EVIDENCE
  | D_FWD
  | D_ADD
  | D_DROP
  | D_LEARN
  | D_FRESH
  | D_IDENT of string
  | D_STRING of string
  | D_EOF
  | D_WS

let unicodeEncoding = new System.Text.UnicodeEncoding()
let STRING_BYTES : Parser<byte[], 'a> = fun st -> st |>
  (STRING |>> (fun str -> unicodeEncoding.GetBytes(str)))

let EOF = fun s -> s |>
begin
    ws >>. eof 
end
      
let token : Parser<token, 'b> = fun st -> st |>
begin
       (FORALL |>> fun _ -> D_FORALL)
   <|> (AND |>> fun x -> D_AND) 
   <|> (IMPLIES |>> fun x -> D_IMPLIES) 
   <|> (AS |>> fun x -> D_AS) 
   <|> (IF |>> fun x -> D_IF) 
   <|> (THEN |>> fun x -> D_THEN) 
   <|> (WITH |>> fun x -> D_WITH) 
   <|> (UPON |>> fun x -> D_UPON) 
   <|> (SAID |>> fun x -> D_SAID) 
   <|> (EVIDENCE |>> fun x -> D_EVIDENCE)
   <|> (FWD |>> fun x -> D_FWD) 
   <|> (ADD |>> fun x -> D_ADD) 
   <|> (DROP |>> fun x -> D_DROP) 
   <|> (LEARN |>> fun x -> D_LEARN) 
   <|> (FRESH |>> fun x -> D_FRESH) 
   <|> (FRESH |>> fun x -> D_FRESH) 
   <|> (STRING |>> D_STRING)
   <|> (ANYIDENT |>> D_IDENT)
   <|> (EOF >>. preturn D_EOF)

end

let rec tokenize : Parser<list<token>, 'a> = fun s -> 
  let reply = token s in 
     (match reply.Status with 
        | Ok -> (* pr "Parsed one token\n"; *)
            (match reply.Result with 
               | D_EOF -> ((* pr "reached EOF";  *)
                   preturn [D_EOF])
               | t -> ((* pr "tokenizing more ...";  *)tokenize) >>= fun ts -> preturn (t::ts))
        | Error -> Printf.printf "Error: %A" (reply.Error, reply.State.Line, reply.State.Column); preturn []) reply.State

type qinfon =
  | MonoTerm of infon
  | PolyTerm of (string * typ) list * infon
  | JustifiedPoly of (term * qinfon * term)
  | PolyVar of string

and infon = 
  | True 
  | Var of string
  | Relation of (string * term list)
  | Said of term * infon
  | Implies of infon * infon
  | And of infon * infon
  | Evidence of term * infon
  | Eval of string

and term = 
  | TermVar of (string * typ)
  | Integer of int
  | Str of string
  | Principal of string
  | EvalTerm of typ * string

and typ = 
  | Int
  | Bool
  | String
  | Infon
  | Prin
  | Ev
  | Uvar of (typ option) ref
exception Failure of string

let unknown_typ () = Uvar (ref None)
let rec compress typ = match typ with
  | Uvar t -> (match !t with 
                 | None -> typ 
                 | Some t -> compress t)
  | _ -> typ
let set_uvar u typ : unit = match compress u with 
  | Uvar t -> t := Some typ
  | _ -> raise (Failure "Uvar already set")

let eval = fun s -> s |> 
    begin
      (EVAL >>.
       DOLLAR >>.
      (manyTill anyChar DOLLAR) |>> (fun s -> new String(s |> Array.of_list)))
    end

let term = fun s -> s |>
    begin
      (varIdent |>> (fun i -> TermVar(i, unknown_typ ())))
 <|>  (STRING   |>> (fun s -> Str s))
 <|>  (pint32   |>> (fun i -> Integer i))
 <|>  (principalIdent |>> (fun i -> Principal i))
 <|>  (eval |>> (fun code -> (EvalTerm(unknown_typ(), code))))
    end

let sequenceMod delim p = 
  p >>= (fun first -> 
           let pp = (delim >>.  p) in 
           many pp  |>> (fun rest -> first::rest))

let sequence delim p = 
  p >>= (fun first -> 
           let pp = (delim >>. p) in 
           many pp  |>> (fun rest -> first::rest))

let terms = fun s -> s |> (sequence COMMA term)

let rec infon = fun s -> s |>
    begin
      (eval |>> Eval)
<|>                                       
      (relationIdent >>= (fun r -> 
       opt (LPAREN >>.
            terms >>= (fun args -> 
            RPAREN >>. preturn args)) >>= (function None -> preturn (Relation(r, [])) 
                                     | Some args ->  preturn (Relation(r, args)))))
<|>
    (bt "said" 
       (term >>= (fun p -> 
          SAID >>. 
          infon |>> (fun i -> Said(p, i)))))

<|>  (varIdent |>> (fun r -> Var r))

<|> 
     (IMPLIES >>.
     LPAREN >>.
     infon >>= (fun i -> 
     COMMA >>. 
     infon >>= (fun j -> 
     RPAREN >>. preturn (Implies(i,j)))))
<|>
    (AND >>. 
     LPAREN >>.
     infons >>= (fun il -> 
     RPAREN >>. (match il with 
                    | c1::c2::rest -> 
                        preturn (List.fold_left (fun out i -> And(out, i)) (And(c1, c2)) rest)
                    | _ -> perr "Need at least two conjuncts")))

<|>
    (EVIDENCE >>.
     term >>= (fun t -> 
     infon |>> (fun i -> Evidence(t, i))))

<|> (LPAREN >>.
     infon >>= (fun i -> 
     RPAREN >>. preturn i))
    end

and infons = fun s -> s |> (sequence COMMA infon)

let qinfon = fun s -> s |> 
    begin
      (FORALL >>. 
      (sequence COMMA varIdent) >>= (fun vars -> 
       DOT >>.
       infon |>> (fun i -> PolyTerm(List.map (fun x -> (x, unknown_typ())) vars, i))))

<|>   (varIdent |>> (fun r -> PolyVar r))

<|>   (infon |>> MonoTerm)

    end

(* -------------------------------------------------------------------------------- *)
type condition =
  | If    of qinfon
  | Upon  of (qinfon * string option)
  | UponJ of (qinfon * string option)
type conditions = condition list

type action =
  | Learn of qinfon
  | Drop  of qinfon
  | Add   of qinfon 
  | WithFresh of string * action list 
  | Fwd   of term * qinfon 
  | Send  of term * qinfon 
type actions = action list

type rule =
  | Rule of (string * typ) list * conditions * actions

type idconf = {prin:string; pubkey:string; privkey:string option; port:int}
type config = 
  | IdCfg of idconf
  | RelCfg of string * typ list
  | Prelude of string

let firstCondition = fun s -> s |> (IF <|> UPON)

let uponCondition = fun s -> s |>
    begin
      UPON >>.
      (opt JUSTIFIED) >>= (fun jopt -> 
      qinfon >>= (fun i -> 
      (opt (AS >>. varIdent)) |>> (fun xopt -> 
          match jopt with 
            | None -> Upon(i, xopt) 
            | Some _ -> UponJ(i,xopt))))
    end

let ifCondition = fun s -> s |>
    begin
      IF >>.
      qinfon |>> (fun i -> If(i))
    end

let condition = fun s -> s |>
    begin
      uponCondition <|> ifCondition
    end

let conditions = fun s -> s |>
    begin
      (sequence (wsUntil firstCondition) condition)
    end

let sendAction = fun s -> s |>
    begin
      SEND >>.
      term >>= (fun p -> 
      qinfon |>> (fun i -> Send(p, i)))
    end 

let fwdAction = fun s -> s |>
    begin
      FWD >>.
      term >>= (fun p -> 
      qinfon |>> (fun i -> Fwd(p, i)))
    end 

let addAction = fun s -> s |>
    begin
      ADD >>.
      qinfon |>> (fun i -> Add(i))
    end 

let dropAction = fun s -> s |>
    begin
      DROP >>.
      qinfon |>> (fun i -> Drop(i))
    end 

let learnAction = fun s -> s |>
    begin
      LEARN >>.
      qinfon |>> (fun i -> Learn(i))
    end 

let firstAction =  LEARN <|> DROP <|> ADD <|> WITH <|> FWD <|> SEND 

let rec freshAction = fun s -> s |>
    begin
      WITH >>. FRESH >>. varIdent >>= (fun x ->
      (LPAREN >>. actions >>= (fun al -> RPAREN >>. preturn (WithFresh(x,al)))) <|>
      (action |>> (fun a -> WithFresh(x, [a]))))
    end 
and action = fun s -> s |>
    begin
      learnAction <|> dropAction <|> addAction <|> fwdAction <|> sendAction <|> freshAction 
    end
and actions = fun s -> s |>
    (sequence (wsUntil firstAction) action)

let oneRule = fun s -> s |>
    begin
      (opt (conditions >>= (fun conds -> 
            THEN  >>. preturn conds))) >>= (fun condsOpt -> 
      actions |>> (fun actions ->  match condsOpt with None -> Rule([], [], actions) | Some conds -> Rule([], conds, actions)))
    end

let rules = fun s -> s |> sequence (wsUntil firstCondition) oneRule

let key = fun s -> s |> 
    begin
      (manyTill anyChar SEMICOLON) >>= (fun pk ->
                                       preturn (new System.String(pk |> Array.of_list)))
      (* RBRACE >>. preturn (new System.String(pk |> Array.of_list))) *)
end

let identityConfig = fun s -> s |>
    begin
      principalIdent >>= (fun p -> 
      EQUALS >>. 
      LBRACE >>.
      opt (PRIVKEY >>. EQUALS >>. key ) >>= (fun pkopt -> 
      (PUBKEY >>. EQUALS >>. key) >>= (fun pubkey -> 
      PORT >>. EQUALS >>. pint32 >>= (fun port -> 
      RBRACE >>.  preturn (IdCfg {prin=p; privkey=pkopt; pubkey=pubkey; port=port}))))) 
    end 

let onetype = fun s -> s |>
    begin
      (INT >>. preturn Int) 
<|>   (BOOL >>. preturn Bool)
<|>   (STRING_T >>. preturn String)
<|>   (INFON >>. preturn Infon)
<|>   (PRINCIPAL >>. preturn Prin)
<|>   (EVIDENCE_T >>. preturn Ev)
    end

let types = fun s -> s |>
    begin 
      (onetype |>> (fun t -> [t]))
<|>   
      (LPAREN >>.
      (sequence COMMA onetype) .>>
       RPAREN)
    end

let relationConfig = fun s -> s |>
    begin
      relationIdent >>= (fun r -> 
      OF >>.
      types |>> (fun tl -> RelCfg(r, tl)))
    end

let preludeConfig = fun s -> s |>
    begin 
      (DOLLAR >>.
      (manyTill anyChar DOLLAR) |>> 
      (fun s -> Prelude (new String(s |> Array.of_list))))
    end

let config = identityConfig <|> relationConfig <|> preludeConfig

let rec configs = fun s -> s |> 
    begin 
      config >>= (fun c -> 
      opt (AND >>. configs) >>= (function None -> preturn [c] | Some cs -> preturn (c::cs)))
    end 

let parseFile fn pars =
  let stream = new System.IO.FileStream(fn, System.IO.FileMode.Open, System.IO.FileAccess.Read) in
  let reader = new System.IO.StreamReader(stream) in
  let fileAsString = reader.ReadToEnd() in
    stream.Close(); 
    let ast = runParserOnString (ws >>. pars) () fn fileAsString in 
      match ast with 
        | Success(r, _, _) -> r
        | _ -> pr "%A" ast; failwith (spr "failed to parse file %s" fn)


let rulesAndConfig = fun s -> s |> 
    begin
      rules >>= (fun rules -> 
      ((USING >>. STRING >>= (fun s -> AS >>. principalIdent |>> (fun p -> 
                                                                    let cfgs = parseFile (spr "%s/%s" !dir s) configs in 
                                                                    let [mine], rest = List.partition (function (IdCfg {prin=q}) when p=q -> true | _ -> false) cfgs in 
                                                                      mine::rest)))
       <|> 
           (WHERE >>. configs)) >>= (fun cfg -> 
      EOF >>. preturn (rules, cfg)))
    end

(* Printer *)
let rec printMany delim s l = match l with 
  | [] -> ""
  | [hd] -> s hd
  | hd::(_::_ as tl) -> spr "%s %s %s" (s hd) delim (printMany delim s tl)

let printManyIndex delim s l = 
  let rec aux ix = function 
    | [] -> ""
    | [hd] -> s ix hd
    | hd::(_::_ as tl) -> spr "%s %s %s" (s ix hd) delim (aux (ix+1) tl) in 
    aux 0 l


let rec printTyp t = match compress t with 
  | Int -> "Int32"
  | Bool -> "Boolean"
  | Infon -> "Infon"
  | String -> "String"
  | Ev -> "Evidence"
  | Prin -> "Principal"
  | Uvar r -> raise (Failure "Unknown type")

and printTyps tl = spr "[ %s ]" (printMany "; " printTyp tl)
  
let printVar (x,t) = spr "({name=\"%s\"; typ=%s})" x (printTyp t)

let rec printQinfon relations = function 
  | MonoTerm i -> spr "(MonoTerm %s)" (printInfon relations i)
  | PolyTerm(xs, i) -> spr "(ForallT [%s] %s)" (printMany "; " printVar xs) (printInfon relations i)
  | JustifiedPoly(p, i, dsig) -> spr "(JustifiedPoly %s %s %s)" 
      (printTerm p) (printQinfon relations i) (printTerm dsig)

and printInfon relations = function
  | True -> spr "(App EmptyInfon [])" 
  | Var x -> spr "(Var %s)" (printVar (x, Infon))
  | Relation(r, tl) -> 
      spr "(App %s\n %s)" (printRelation relations r) (printTerms tl)
  | Said(p, i) -> spr "(App SaidInfon\n [%s;\n %s])" (printTerm p) (printInfon relations i)
  | Implies(i, j) -> spr "(App ImpliesInfon\n [%s;\n %s])" (printInfon relations i) (printInfon relations j)
  | And(i, j) -> spr "(App AndInfon\n [%s; %s])" (printInfon relations i) (printInfon relations j)
  | Eval(code) -> spr "(Eval Infon (%s))" code
  | Evidence(t,i) -> "(Evidence XXX XXX)"

and printRelation relations r = 
  match List.find (function RelCfg(r',_) -> r=r' | _ -> false) relations with 
    | RelCfg(r, tl) -> 
        spr "(RelationInfon ({fname=\"%s\"; retType=Infon; argsType=%s; identity=None}))" r (printTyps tl) 
          
and printTerm = function 
  | TermVar x -> spr "(Var %s)" (printVar x)
  | Integer i -> spr "(Const (Int %d))" i
  | Str s -> spr "(Const (Str \"%s\"))" s
  | Principal p -> spr "(Const (PrincipalConstant \"%s\"))" p
  | EvalTerm(typ, code) -> spr "(Eval %s (%s))" (printTyp typ) code

and printTerms s = spr "[ %s ]" (printMany ";\n" printTerm s)
 
let rec printRule relations i (Rule (ctx, cl, al)) = 
  spr "let rule_%d = \n\t \
         let conditions = %s in \n\t \
         let actions = %s in \n\t \
           mkRule [%s] conditions actions\n" 
    i 
    (printConditions relations cl) 
    (printActions relations al)
    (printMany "; " printVar ctx)
    

and printCondition relations = function 
  | If   qi      -> spr "(If (%s) )" (printQinfon relations qi) 
  | UponJ (qi, s) 
  | Upon (qi, s) -> spr "(Upon (%s))" (printQinfon relations qi)


and printConditions relations conds = spr "[ %s ]" (printMany ";\n" (printCondition relations) conds)

and printAction relations = function
  | Learn qi -> spr "(Learn %s)" (printQinfon relations qi)
  | Drop  qi -> spr "(Drop %s)" (printQinfon relations qi)
  | Add   qi -> spr "(Add %s)" (printQinfon relations qi)
  | WithFresh (x, al) -> spr "(WithFresh %s %s)" (printVar (x, Int)) (printActions relations al)
  | Fwd  (p, qi) -> spr "(Fwd %s %s)" (printTerm p) (printQinfon relations qi)
  | Send  (p, qi) -> spr "(Send %s %s)" (printTerm p) (printQinfon relations qi)

and printActions relations actions = spr "[ %s ]" (printMany ";\n" (printAction relations) actions)

let printConfig = function 
  | RelCfg _ -> ""
  | IdCfg cfg ->
      spr "({prin=\"%s\"; pubkey=\"%s\"; privkey=%s; port=%d})" 
        cfg.prin 
        cfg.pubkey 
        (match cfg.privkey with 
           | None -> "None"
           | Some s -> spr "(Some \"%s\")" s)
        cfg.port

let printPrelude ps = printMany "\n\n" (function (Prelude code) -> code) ps
let printConfigs cl = spr "[ %s ]" (printMany ";\n" printConfig cl)

let printRulesAndConfig prelude relations (fn:string) (rules, configs) = 
  let mname = System.IO.Path.GetFileNameWithoutExtension(fn) in 
  let mname = mname.ToUpper() in
    pr "module %s\n" mname;
    pr "open Types\n";
    pr "open Interp\n";
    pr "open Subst\n";
    pr "open Authenticate\n\n";
    pr "%s\n" (printPrelude prelude);
    pr "%s\n" (printManyIndex "\n\n" (printRule relations) rules);
    pr ";;\n let _ = initialize (%s) in \n let _ = run [%s] in ()" 
      (printConfigs configs)
      (printManyIndex "; " (fun ix _ -> spr "rule_%d" ix) rules)

let newvar = 
  let x = ref 0 in 
    fun () -> 
      let i = !x in 
        incr x;
        spr "x_%d" i

type env = (string * typ) list * (string * qinfon) list

let annotate relations rules = 
  let findRelation r = 
    try 
      match List.find (function RelCfg(r',_) -> r=r' | _ -> false) relations with 
        | RelCfg(r, tl) -> Some tl
    with _ -> None 
  in

  let binds x  = List.exists (fun (x',_) -> x=x') in

  let ctxContains x (ctx, subst) = 
    List.exists (fun (x',_) -> x=x') ctx ||
      List.exists (fun (x',_) -> x=x') subst in 
    
  let extendCtx ((ctx, subst) as env) (x,t) = 
    if ctxContains x env 
    then failwith "Variable shadowing not allowed"
    else ((x,t)::ctx, subst) in

  let addSubst ((ctx, subst) as env) (x,qi) = 
    if ctxContains x env 
    then failwith "Variable shadowing not allowed"
    else (ctx, (x,qi)::subst) in
    
  let lookup x env =
    try 
      Some (snd (List.find (fun (x',_) -> x=x') env))
    with _ -> None in
    
  let rec annotRule ctx = function
    | Rule (_, c, a) -> 
        let c, ctx = annotConditions ctx c in
        let a = List.map (annotAction ctx) a in 
          Rule(fst ctx, c, a)

  and annotConditions (ctx:env) cl : condition list * env =
    let ctx, out = List.fold_left (fun (ctx, out) c -> 
                                     let c', ctx = annotCondition ctx c in 
                                       (ctx, c'::out)) (ctx, []) cl in
      List.rev out, ctx

  and annotCondition (ctx : env) : condition -> condition * env = function 
    | If qi -> 
        let qi, ctx = annotQinfon ctx qi in 
          (If qi), ctx
            
    | Upon (qi, m) -> 
        let qi, ctx = annotQinfon ctx qi in 
        let ctx = match m with 
          | None -> ctx
          | Some m -> addSubst ctx (m, qi) in 
          Upon(qi, m), ctx
            
    | UponJ (qi, m) -> 
        let qi, ctx = annotQinfon ctx qi in 
        let ctx' = ctx in 
        let ev_var = newvar(), Ev in 
        let p_var = newvar(), Prin in 
        let qi' = JustifiedPoly(TermVar p_var, qi, TermVar ev_var) in 
        let ctx = extendCtx (extendCtx ctx' ev_var) p_var in 
        let ctx = match m with 
          | None -> ctx
          | Some m -> addSubst ctx (m, qi') in 
          Upon(qi', m), ctx

  and annotAction (ctx:env) = function 
    | Learn qi -> Learn (typeQinfon ctx qi)
    | Drop qi -> Drop (typeQinfon ctx qi)
    | Add qi -> Add (typeQinfon ctx qi)
    | WithFresh (x, al) -> 
        let ctx = extendCtx ctx (x, Int) in 
          WithFresh(x, List.map (annotAction ctx) al)
    | Fwd(p, qi) -> Fwd((typeTerm ctx p Prin), typeQinfon ctx qi)
    | Send(p, qi) -> Send((typeTerm ctx p Prin), typeQinfon ctx qi)

  and annotQinfon = checkQinfon true
  and typeQinfon (ctx:env) qi = fst (checkQinfon false ctx qi)
  and typeTerm (ctx:env) t typ = fst (checkTerm false ctx t typ)
  and checkQinfon extendflag (ctx:env) : qinfon -> qinfon * env = function 
    | MonoTerm i -> let i, ctx = checkInfon extendflag ctx i in MonoTerm i, ctx
    | PolyTerm (xl, i) -> 
        let ctx' = List.fold_left (fun ctx x -> extendCtx ctx x) ctx xl in 
        let i, _ = checkInfon extendflag ctx' i in
          PolyTerm(xl, i), ctx
    | PolyVar x -> 
        let (_, subst) = ctx in 
          match lookup x subst with 
            | None -> raise (Failure (spr "Failed to eliminate polyvar %s\n" x))
            | Some tm -> tm, ctx

  and checkInfon extflag ctx = function
    | True -> True, ctx

    | Var x when binds x (fst ctx) -> 
        let ty = lookup x (fst ctx) in 
          Var x, ctx

    | Var x when (not (binds x (snd ctx))) && extflag -> 
        Var x, extendCtx ctx (x,Infon)
          
    | Relation (r, terms) -> 
        (match findRelation r with 
           | Some types when List.length terms = List.length types -> 
               let ctx, terms = 
                 List.fold_left (fun (ctx, terms) (term, typ) -> 
                                   let term', ctx' = checkTerm extflag ctx term typ in 
                                     (ctx', term'::terms)) 
                   (ctx, [])
                   (List.zip terms types) in 
                 Relation(r, List.rev terms), ctx
           | _ -> failwith (spr "Failed to type relation %s" r))
        
    | Said(x, i) -> 
        let x', ctx = checkTerm extflag ctx x Prin in 
        let i', ctx = checkInfon extflag ctx i in 
          Said(x', i'), ctx

    | Implies(i, j) -> 
        let i, ctx = checkInfon extflag ctx i in 
        let j, ctx = checkInfon extflag ctx j in 
          Implies(i,j), ctx

    | And(i, j) -> 
        let i, ctx = checkInfon extflag ctx i in 
        let j, ctx = checkInfon extflag ctx j in 
          And(i,j), ctx

    | Evidence(t, i) ->
        let t, ctx = checkTerm extflag ctx t Ev in 
        let i, ctx = checkInfon extflag ctx i in 
          Evidence(t,i), ctx

    | Eval code -> Eval code, ctx

    | i -> failwith (spr "Failed to type infon %A" i)

  and checkTerm extflag (ctx:env) tm typ : term * env = match tm with
    | TermVar(x,_) when not (binds x (snd ctx)) -> 
        (match lookup x (fst ctx) with 
           | None when extflag -> 
               let ctx = extendCtx ctx (x, typ) in
                 TermVar(x,typ), ctx
           | Some t -> 
               (match compress t with 
                  | Uvar t -> (t := Some typ; 
                               TermVar(x,typ), ctx)
                  | t' when t'=typ -> TermVar(x,typ), ctx
                  | t' -> failwith (spr "Failed to type variable %s. Expected %s got %s\n" x (printTyp typ) (printTyp t')))
           | _ -> failwith (spr "Failed to type variable %s" x))
          
    | Integer i when typ=Int -> tm, ctx
    | Str s when typ = String -> tm, ctx
    | Principal p when typ=Prin -> tm, ctx
    | EvalTerm(t, _) -> set_uvar t typ; tm, ctx
    | _ -> failwith (spr "Failed to type term %A; expected %A" tm typ) in
    
    List.map (annotRule ([],[])) rules

let parse fn = 
  dir := System.IO.Path.GetDirectoryName(fn);
  let rules, config = parseFile fn rulesAndConfig in
  let relations, identities_and_prelude = List.partition (function RelCfg _ -> true | _ -> false) config in
  let prelude, identities = List.partition (function Prelude _ -> true | _ -> false) identities_and_prelude in 
  let rules' = annotate relations rules in 
    printRulesAndConfig prelude relations fn (rules', identities)
      
;; 


let _ = 
  let len = Array.length Sys.argv in
    if len = 1 
    then Printf.printf "Usage: parseDkal <filename>.dkal\n"
    else 
      let filename = Sys.argv.(1) in 
        try 
          parse filename
        with Failure msg -> pr "%s" msg
  
