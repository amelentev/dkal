namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.Ast

module Parser =
  
  let ts2s t = String.concat " " (List.map (fun (p:Tok) -> p.ToString()) t) 
  let ps2s p = String.concat " " (List.map (fun (p:Pat) -> p.ToString()) p) 
  
  let splitAtNewLine lst =
    let rec aux acc = function 
      | Tok.NewLine p as t :: rest -> (List.rev (t :: acc), rest)
      | x :: xs -> aux (x :: acc) xs
      | [] -> (List.rev acc, [])
    aux [] lst
  
  let err pos msg =
    raise (SyntaxError (pos, "syntax error: " + msg))
    
  let errl toks msg =
    match toks with
      | (t:Tok) :: _ -> err t.Pos msg
      | _ -> err fakePos msg
  
  let resolveType (ctx:Context) toks tp =
    match ctx.types.TryGetValue tp with
      | true, res -> res
      | _ when tp = "anytype" -> Type.Unbound
      | _ -> errl toks ("unbound type " + tp)
  
  let parseType (ctx:Context) = function
    | [Tok.Var (_, parm); Tok.Id (_, ":"); Tok.Id (_, tp)] as toks ->
      ctx.MkVar parm (resolveType ctx toks tp)
    | [Tok.Id (_, tp)] as toks -> 
      ctx.MkVar "" (resolveType ctx toks tp)
    | toks -> errl toks ("expecting type name (at " + ts2s toks + ")")
  
  let rec parseTypeList (ctx:Context) = function
    | Tok.Group (_, '}', typeName) :: rest ->
      parseType ctx typeName :: parseTypeList ctx rest
    | Tok.Id (_, ",") :: rest -> parseTypeList ctx rest
    | [] -> []
    | lst -> errl lst "expecting comma-separated list of {types}"

  let addStandardRules (ctx:Context) = ()
    
  let blockToList = function
    | Tok.Block (pos, lst) ->
      let rec aux acc = function
        | Tok.NewLine _ :: rest -> aux acc rest
        | [tok] -> aux (tok :: acc) []
        | tok :: Tok.NewLine _ :: rest -> aux (tok :: acc) rest
        | tok1 :: tok2 :: _ ->
          err tok1.Pos ("found two expressions in a single line:\n" + tok1.ToString() + "\nand:\n" + tok2.ToString())
        | [] -> List.rev acc
      Tok.App (pos, "list", aux [] lst)
    | _ -> failwith "shouldn't happen, block pattern, non-block arg"
      
  let simpleRule name pats =
    let make (args:list<Tok>) =
      let pos = args.Head.Pos
      let handleOne acc arg = function
        | Pat.Id _
        | Pat.NewLine -> acc
        | Pat.Block -> blockToList arg :: acc
        | _ -> arg :: acc      
      [Tok.App (pos, name, List.fold2 handleOne [] args pats |> List.rev)]
    { name = name; pats = pats; body = make; priority = -1 }
  
  let addRules (ctx:Context) =
    let fixRule = function
      | Pat.NewLine :: rest -> List.rev rest
      | x -> List.rev x
      
    let rec ruleBody isAttr mkRule ((name:string), rule, args) = 
      let self = ruleBody isAttr mkRule
      
      let ret retType =
        let name = if name.StartsWith "-" then name.Substring 1 else name
        let name =
          if List.length args = 2 && List.length rule = 3 then
            if name.StartsWith "*-" && name.EndsWith "-*" then
              name.Substring (2, name.Length - 4)
            else name
          else name
        let (rule:Rule) = mkRule name (fixRule rule)
        { name = rule.name; argTypes = List.rev args; retType = retType; id = ctx.NextId(); body = null }, rule
        
      function
      | [Tok.NewLine _] when isAttr -> ret (ctx.MkVar "" Type.Infon)
      | [p] -> err p.Pos "premature end of syntax rule"
      | Tok.Id (ep, "returns") :: _ when isAttr ->
        err ep "'returns' not allowed in attributes"
      | Tok.Id (ep, "returns") :: rest when not isAttr ->
        match rest with
          | [Tok.Group (_, '}', typeName); Tok.NewLine _] -> ret (parseType ctx typeName)
          | _ -> err ep "expecting type name after 'returns'"
      | Tok.Group (_, '}', typeName) :: rest ->
        match typeName with
          | Tok.Id (_, "block") :: typeName ->
            self (name + "-*", Pat.NewLine :: Pat.Block :: Pat.NewLine :: rule, parseType ctx typeName :: args) rest
          | _ ->
            self (name + "-*", Pat.Any :: rule, parseType ctx typeName :: args) rest
      | Tok.Group (_, ')', types) :: rest ->
        self (name, Pat.Group ')' :: rule, parseTypeList ctx types @ args) rest
      | Tok.Id (_, id) as t :: rest ->
        self (name + "-" + id, Pat.Id id :: rule, args) rest
      | x :: _ -> err x.Pos ("invalid token in syntax rule: " + x.ToString())
      | [] -> failwith "can't happen"
    
    let doLine toks =    
      let line, rest = splitAtNewLine toks
      match line with
        | Tok.Id (pos, ("function"|"attribute" as kind)) :: body ->
          let getName, pri, body =
            match body with
              | Tok.Group (_, ']', [Tok.Id (_, name); Tok.Int (_, k)]) :: rest ->
                (fun _ -> name), k, rest
              | Tok.Group (_, ']', [Tok.Int (_, k)]) :: rest ->
                (fun x -> x), k, rest
              | _ -> errl body "expecting [priority]"            
          let mkRule name rule =
            let name = getName name
            let r = simpleRule name rule
            { r with priority = pri }
          let (fn, rl) = ruleBody (kind = "attribute") mkRule ("", [], []) body
          ctx.AddRule rl
          if ctx.functions.ContainsKey fn.name then
            // TODO check if types match?
            ()
          else
            ctx.functions.Add (fn.name, fn)
          match splitAtNewLine rest with
            | Tok.Block (_, Tok.Id (_, "is") :: body) :: line, rest ->
              fn.body <- (body :> obj)
              ctx.pendingFunctions <- fn :: ctx.pendingFunctions
              line, rest
            | _ -> [], rest
        | [Tok.Id (_, "type"); Tok.Id (_, name); Tok.NewLine _] ->
          if not (ctx.types.ContainsKey name) then
            ctx.AddType name
          [], rest
        | [Tok.Id (_, "#"); Tok.Id (_, "set"); Tok.Id (_, name); (Tok.StringLiteral (_, value) | Tok.Id (_, value)); Tok.NewLine _] ->
          ctx.options.[name] <- value
          [], rest
        | _ -> line, rest
    
    let rec aux acc = function
      | [] -> List.rev acc
      | toks ->
        let (line, rest) = doLine toks
        aux (rev_append line acc) rest
    aux []
    
  
  let matchRule rule sofar =
    let rec cmp acc = function
      | Pat.Any :: pats, tok :: toks ->
        cmp (tok :: acc) (pats, toks)
      | Pat.App name :: pats, (Tok.App (_, name', _) as tok) :: toks when name = name' -> 
        cmp (tok :: acc) (pats, toks)
      | Pat.Group name :: pats, (Tok.Group (_, name', _) as tok) :: toks when name = name' -> 
        cmp (tok :: acc) (pats, toks)
      | Pat.Id name :: pats, (Tok.Id (_, name') as tok) :: toks when name = name' -> 
        cmp (tok :: acc) (pats, toks)
      | Pat.Block :: pats, (Tok.Block (_, _) as tok) :: toks ->
        cmp (tok :: acc) (pats, toks)
      | Pat.NewLine :: pats, (Tok.NewLine (_) as tok) :: toks ->
        cmp (tok :: acc) (pats, toks)
      | [], toks ->
        //System.Console.WriteLine ("match " + ts2s acc + " : " + ts2s toks)
        Some (List.rev acc, toks)
      | pats, rem -> 
        //System.Console.WriteLine ("no match: " + ts2s rem + " != " + ps2s pats)
        None      
    cmp [] (rule, sofar)
        
  
  let rec applyRules (ctx:Context) toks =
    let self = applyRules ctx
    // first do a recursive pass
    let recApply = function
      | Tok.Id (_, _)
      | Tok.Var (_, _)
      | Tok.Int (_, _)
      | Tok.StringLiteral (_, _)
      | Tok.NewLine _ as t -> t
      | Tok.App (_, _, _) as a ->
        failwith ("found app " + a.ToString())
      | Tok.Block (p, l) -> Tok.Block (p, self l)
      | Tok.Group (p, c, l) -> Tok.Group (p, c, self l)
    let toks = List.map recApply toks
    let applyPri toks pri =
      let rules = ctx.rules.[pri]
      let revToks x = if pri % 2 = 0 then x else List.rev x
      let revRule x = if pri % 2 = 0 then List.rev x else x
      
      let anyMatch sofar =
        rules |> 
        List.fold (fun state r1 ->
          match matchRule (revRule r1.pats) sofar, state with
            | Some (matched, rest), None -> Some (r1, revRule matched, rest)
            | None, r -> r
            | Some (m1, _), Some (r2, m2, _) ->
              errl sofar ("multiple rules match: " + r1.name + " and " + r2.name + " both match\n" + (Tok.Block (fakePos, m1)).ToString())) None
              
      let rec aux sofar rest =
        match anyMatch sofar with
          | Some (r, m, rem) ->
            aux (rev_append (r.body m) rem) rest
          | None ->
            match rest with
              | x :: xs -> aux (x :: sofar) xs
              | [] -> revRule sofar
               
      aux [] (revToks toks)
      
    ctx.rules.Keys |> Seq.toList |> List.sort |> List.fold applyPri toks
    
