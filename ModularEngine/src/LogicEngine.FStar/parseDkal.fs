#light "off"
module DkalParser

open System

open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

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
                "as";
                "then";
                "if";
                "said";
                "fwd";
                "add";
                "drop";
                "learn";
                "with";
                "fresh";
                "Ev"]

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
let principalIdent = IDENT (fun x -> x='_') (fun x -> isUpper x || (x = '_'))

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
let SAID      st = tok "said"  st 
let SEND      st = tok "send"  st 
let FWD       st = tok "fwd"  st 
let ADD       st = tok "add"  st 
let DROP      st = tok "drop"  st 
let LEARN     st = tok "learn"  st 
let FRESH     st = tok "fresh"  st 
let EVIDENCE  st = tok "Ev"  st 

let DOUBLE_QUOTE st = skipString "\"" st 
let QUOTE     st = skipString "'"  st 

type stok<'a> = Parser<string,'a>
let stok s : stok<'a> = pstring s .>> ws
let LPAREN    st = stok "("  st 
let RPAREN    st = stok ")"  st 
let COMMA     st = stok ","  st 
let RARROW    st = stok "->"  st 
let RRARROW   st = stok "=>"  st 
let RQUOTE    st = stok "'"  st
let DOT       st = stok "."  st

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
  | PolyTerm of string list * infon

and infon = 
  | True 
  | Var of string
  | Relation of (string * term list)
  | Said of term * infon
  | Implies of infon * infon
  | And of infon * infon
  | Evidence of term * infon

and term = 
  | TermVar of string 
  | Integer of int
  | Str of string
  | Principal of string

let term = fun s -> s |>
    begin
      (varIdent |>> (fun i -> TermVar i))
 <|>  (STRING   |>> (fun s -> Str s))
 <|>  (pint32   |>> (fun i -> Integer i))
 <|>  (principalIdent |>> (fun i -> Principal i))
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
       infon |>> (fun i -> PolyTerm(vars, i))))
<|>   
      (infon |>> MonoTerm)
    end

(* -------------------------------------------------------------------------------- *)
type condition =
  | If   of qinfon
  | Upon of (qinfon * string)
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
  | Rule of conditions * actions

let firstCondition = fun s -> s |> (IF <|> UPON)

let uponCondition = fun s -> s |>
    begin
      UPON >>.
      qinfon >>= (fun i -> 
      AS >>.
      varIdent |>> (fun x -> Upon(i, x)))
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
      conditions >>= (fun conds -> 
      THEN  >>. 
      actions |>> (fun actions ->  Rule(conds, actions)))
    end

let rules = fun s -> s |> 
    begin
      (sequence (wsUntil firstCondition) oneRule) >>= (fun rules -> 
       EOF >>. preturn rules)                                                       
    end

let pr = Printf.printf 
let spr = Printf.sprintf 

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

let printVar t x = match t with
  | None -> spr "({name=\"%s\"; typ=Unknown})" x
  | Some s -> spr "({name=\"%s\"; typ=%s})" x s

let rec printQinfon = function 
  | MonoTerm i -> spr "(MonoTerm %s)" (printInfon i)
  | PolyTerm(xs, i) -> spr "(ForallT [%s] %s)" (printMany "; " (printVar None) xs) (printInfon i)

and printInfon = function
  | True -> spr "(App EmptyInfon [])" 
  | Var x -> spr "(Var %s)" (printVar (Some "Infon") x)
  | Relation(r, tl) -> 
      let rel = spr "(RelationInfon ({fname=\"%s\"; retType=Infon; argsType=[]; identity=None}))" r in 
        spr "(App %s\n %s)" rel (printTerms tl)
  | Said(p, i) -> spr "(App SaidInfon\n [%s;\n %s])" (printTerm p) (printInfon i)
  | Implies(i, j) -> spr "(App ImpliesInfon\n [%s;\n %s])" (printInfon i) (printInfon j)
  | And(i, j) -> spr "(App AndInfon\n [%s; %s])" (printInfon i) (printInfon j)
  | Evidence(t,i) -> "(Evidence XXX XXX)"

and printTerm = function 
  | TermVar s -> spr "(Var %s)" (printVar None s)
  | Integer i -> spr "(Const (Int %d))" i
  | Str s -> spr "(Const (Str \"%s\"))" s
  | Principal p -> spr "(Const (PrincipalConstant \"%s\"))" p

and printTerms s = spr "[ %s ]" (printMany ";\n" printTerm s)
 
let rec printRule i (Rule (cl, al)) = 
  spr "let rule_%d = \n\t \
         let conditions = %s in \n\t \
         let actions = %s in \n\t \
           mkRule [] conditions actions\n" 
    i (printConditions cl) (printActions al)

and printCondition = function 
  | If   qi      -> spr "(If (%s) )" (printQinfon qi) 
  | Upon (qi, s) -> spr "(Upon (%s) (* %s *))" (printQinfon qi) (printVar None s)

and printConditions conds = spr "[ %s ]" (printMany ";\n" printCondition conds)

and printAction = function
  | Learn qi -> spr "(Learn %s)" (printQinfon qi)
  | Drop  qi -> spr "(Drop %s)" (printQinfon qi)
  | Add   qi -> spr "(Add %s)" (printQinfon qi)
  | WithFresh (x, al) -> spr "(WithFresh %s %s)" (printVar (Some "Int32") x) (printActions al)
  | Fwd  (p, qi) -> spr "(Fwd %s %s)" (printTerm p) (printQinfon qi)
  | Send  (p, qi) -> spr "(Send %s %s)" (printTerm p) (printQinfon qi)

and printActions actions = spr "[ %s ]" (printMany ";\n" printAction actions)

let printRules (fn:string) rules = 
  let mname = fn.Split('.').(0) in 
  pr "module %s\n" (mname.ToUpper());
  pr "open Types\n";
  pr "open Interp\n\n";
  pr "%s\n" (printManyIndex "\n\n" printRule rules);
  pr ";;\n let _ = run [%s] in ()" (printManyIndex "; " (fun ix _ -> spr "rule_%d" ix) rules)

exception Failure 

let parse fn = 
  let stream = new System.IO.FileStream(fn, System.IO.FileMode.Open, System.IO.FileAccess.Read) in
  let reader = new System.IO.StreamReader(stream) in
  let fileAsString = reader.ReadToEnd() in
    stream.Close(); 
    let ast = runParserOnString rules () fn fileAsString in 
      match ast with 
        | Success(rules, _, _) -> printRules fn rules
        | _ -> raise Failure  
;; 


let _ = 
  let len = Array.length Sys.argv in
    if len = 1 
    then Printf.printf "Usage: parseDkal <filename>.dkal\n"
    else 
      let filename = Sys.argv.(1) in 
        parse filename
  
