namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.PreToken
open Microsoft.Research.DkalEngine.Ast

type SqlCommunicator(ctx:Context, me:Principal) =
  let sql = SqlConnector(ctx.options.["common_sql"])
  
  let principalName = dict()
  let principalId = dict()
  let fillPrincipals() = 
    if principalName.Count = 0 then
      use reader = sql.ExecReader "SELECT id, name FROM dkal_principals"
      while reader.Read() do
        let id = reader.GetInt32 0
        let name = (reader.GetString 1).Trim(' ')
        System.Console.WriteLine("{0} -> {1}", id,name)
        principalId.Add (name, id)
        principalName.Add (id, name)

  let rec principalById id =
    fillPrincipals()
    match ctx.principals.TryGetValue principalName.[id] with
      | true, p -> p
      | _ ->
        ctx.AddPrincipal principalName.[id]
        principalById id
  
  let principalId (p:Principal) =
    fillPrincipals()
    match principalId.TryGetValue p.name with
      | true, v -> v
      | _ -> failwith ("principal '" + p.name + "' not in database")
    
  let idChar c = Char.IsLetterOrDigit c || c = '_'
  
  let serializeTerm t =
    let sb = System.Text.StringBuilder()
    let wr (s:obj) = sb.Append s |> ignore
    let quote (s:string) =
      for c in s do
        if idChar c then wr c
        else wr '\\'; wr c       
    let visitedVars = dict()
    let rec aux = function
      | Term.Const (_, Int i) -> wr "#"; wr i
      | Term.Const (_, Bool b) -> wr "@"; wr (if b then "T" else "F")
      | Term.Const (_, Principal p) -> wr "%"; wr (principalId p)
      | Term.Const (_, Column _) -> failwith "cannot serilize column"
      | Term.Var (_, v) -> 
        wr "$"; quote v.name
        if not (visitedVars.ContainsKey v.id) then
          wr ":"; quote v.typ.name
          visitedVars.Add (v.id, true)
      | Term.App (_, f, []) ->
        quote f.name; wr "()"
      | Term.App (_, f, args) ->
        quote f.name
        wr "("
        for t in args do
          aux t
          wr ", "
        sb.Length <- sb.Length - 2
        wr ")"
    aux t
    sb.ToString()
          
  let deserializeTerm (s:string) =
    let parseName i =
      let sb = System.Text.StringBuilder()
      let wr (s:obj) = sb.Append s |> ignore
      let rec loop i =
        if s.[i] = '\\' then
          wr s.[i + 1]; loop (i + 2)
        elif idChar s.[i] then
          wr s.[i]; loop (i + 1)
        else
          (i, sb.ToString())
      loop i
    let parseInt i =
      let rec findEnd cur =
        if (i = cur && s.[i] = '-') || Char.IsDigit s.[cur] then findEnd (cur + 1)
        else cur
      let theEnd = findEnd i
      (theEnd, System.Int32.Parse (s.Substring (i, theEnd - i)))
    let p = fakePos
    let vars = dict()
    let rec parseTerm i =
      match s.[i] with
        | '#' ->
          let (i, v) = parseInt (i + 1)
          (i, Term.Const (p, Const.Int v))
        | '@' ->
          (i + 2, Term.Const (p, Const.Bool (s.[i+1] = 'T')))
        | '%' ->
          let (i, v) = parseInt (i + 1)
          (i, Term.Const (p, Const.Principal (principalById v)))
        | '$' ->
          let (i, name) = parseName (i + 1)
          let (i, tp) =
            if s.[i] = ':' then
              let (i, tpName) = parseName (i + 1)
              (i, Some (ctx.types.[tpName]))
            else (i, None)
          let v = 
            match vars.TryGetValue name with
              | true, v -> v
              | _ ->
                let v = ctx.MkVar name tp.Value
                vars.Add (name, v)
                v
          (i, Term.Var (p, v))          
        | c when c = '\\' || idChar c ->
          let (i, name) = parseName i
          let fn = ctx.functions.[name]
          let rec parseList acc i =
            match s.[i] with
              | ')' -> (i + 1, List.rev acc)
              | ' ' | ',' -> parseList acc (i + 1)
              | _ ->
                let (i, t) = parseTerm i
                parseList (t :: acc) i
          if s.[i] <> '(' then failwith "expecting ( in term app"
          let (i, args) = parseList [] (i + 1)
          (i, Term.App (p, fn, args))          
        | _ -> failwith "invalid character"  
    parseTerm 0
          

  let serializeTerms ts =
    serializeTerm (Term.App (fakePos, ctx.functions.["__tuple"], ts))
  
  let deserializeTerms s =
    match deserializeTerm s with
      | _, Term.App (_, { name = "__tuple" }, ts) -> ts
      | _ -> failwith "expecting __tuple(...)"
  
  member this.PrincipalById id = principalById id  
  member this.PrincipalId (p:Principal) = principalId p
  
  member this.SendMessage (msg:Message) =
    let cmd = sql.GetCommand "INSERT INTO dkal_messages (sender, reciver, msg) VALUES (@s, @r, @m)"
    let add (n, v) = cmd.Parameters.AddWithValue (n, v) |> ignore
    add ("s", this.PrincipalId msg.source)
    add ("r", this.PrincipalId msg.target)
    add ("m", serializeTerms [msg.message; msg.proviso; Term.Const (fakePos, Const.Bool msg.certified)])
    cmd.ExecuteNonQuery() |> ignore
  
  member this.CheckForMessage () =
    let markRead o =
      let cmd = sql.GetCommand "UPDATE dkal_messages SET readAt = CURRENT_TIMESTAMP WHERE sentAt = @s"
      let add (n, v) = cmd.Parameters.AddWithValue (n, v) |> ignore
      add ("s", o)
      cmd.ExecuteNonQuery() |> ignore
      
    let getMsg (rd:System.Data.SqlClient.SqlDataReader) =       
      // deserialize
      System.Console.WriteLine("{0}", rd.GetInt32 1)
      let src = principalById (rd.GetInt32 1)
      match deserializeTerms (rd.GetString 2) with
        | [msg; prov; Term.Const (_, Const.Bool cert)] ->
          let msg =
            { source = src
              target = me
              message = msg
              proviso = prov
              certified = cert } 
          rd.GetValue 0, msg
        | _ -> failwith "invalid message"
          
    fillPrincipals()
    let res = 
      sql.ExecQuery ("SELECT TOP 1 sentAt, sender, msg FROM dkal_messages WHERE reciver = @p__0 AND readAt IS NULL ORDER BY sentAt", 
                     [this.PrincipalId me :> obj], false) |> Seq.map getMsg |> Seq.toList
    match res with
      | [] -> None
      | (v, msg) :: _ -> markRead v; Some msg
      
  
    
