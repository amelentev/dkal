namespace Microsoft.Research.DkalEngine.Shell

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Util

type Global =
  {
    Contexts : Dict<string, Context>
  }

and Context(filename, gctx:Global, tr) =
  let pctx = ParsingCtx ()
  let opts = Options.Create ()
  let eng = Engine.Config opts
  let serializer = Serializer(pctx)

  let say (s:string) = 
    System.Console.WriteLine ("{0}: {1}", pctx.Me.Name, s)
  let xf = sprintf

  let principalName = dict()
  let principalId = dict()
  let fillPrincipals() = 
    if principalName.Count = 0 then
      let sql = eng.sql.Value
      use reader = sql.ExecReader "SELECT id, name FROM dkal_principals"
      while reader.Read() do
        let id = reader.GetInt32 0
        let name = (reader.GetString 1).Trim(' ')
        if opts.Trace > 1 then
          System.Console.WriteLine("{0} -> {1}", id,name)
        principalId.Add (name, id)
        principalName.Add (id, name)

  let rec principalById id =
    fillPrincipals()
    pctx.LookupOrAddPrincipal principalName.[id]
  
  let principalId (p:Ast.Principal) =
    fillPrincipals()
    match principalId.TryGetValue p.name with
      | true, v -> v
      | _ -> failwith ("principal '" + p.name + "' not in database")

  do
    let assertions = pctx.ParsePrelude () @ pctx.ParseFile filename
    opts.PrivateSql <- pctx.Options.["private_sql"]
    opts.Trace <- tr
    eng.Reset ()
    List.iter eng.AddAssertion assertions
    eng.AddDefaultFilter ()
    say "loaded"

  member this.Me = pctx.Me
  member this.Id = principalId this.Me
  member this.RecieveMessage ser =
    let msg = serializer.DeserializeMessage ser
    eng.Listen (this, msg)
  member this.Talk () = eng.Talk this
  member this.CheckPoint () = eng.CheckPoint ()
  member this.Finish () = eng.Finish ()

  interface ICommunicator with
    member this.RequestFinished () = ()
    member this.ExceptionHandler e =
      say ("exception: " + e.ToString())
    member this.Warning e = say ("warning: " + e)
    member this.Knows inf =
      if opts.Trace >= 1 then
        say (xf "knows %O" (inf.infon.Sanitize()))
    member this.QueryResults (inf, res) = ()
    member this.SendMessage msg =
      match gctx.Contexts.TryGetValue msg.target.name with
        | true, target ->
          let msg' = serializer.SerializeMessage msg
          say (xf "say to %O:\n%O" target.Me (msg.message.Sanitize().ToSX()))
          target.RecieveMessage msg'
        | _ ->
          say ("no such principal " + msg.target.ToString())
    member this.PrincipalById id = principalById id  
    member this.PrincipalId (p:Ast.Principal) = principalId p

module Main =
  let main () =
    let gctx = { Contexts = dict() }
    let ctxList = vec()
    let dumpSx = ref false
    let rec aux tr = function
      | "-v" :: rest -> aux (tr + 1) rest
      | "-sx" :: rest -> dumpSx := true; aux tr rest
      | fn :: rest ->
        if !dumpSx then
          let pctx = ParsingCtx ()
          let prelude = pctx.ParsePrelude ()
          let body = pctx.ParseFile fn
          let decls =
            [for a in pctx.LateFunctions() -> a.ToSX()] @ [for a in body -> a.ToSX()]
          decls |> Ast.optimizeSX |>  Seq.iter System.Console.Write 
        else
          let c = Context (fn, gctx, tr)
          gctx.Contexts.Add (c.Me.Name, c)
          ctxList.Add c        
          aux 0 rest
      | [] -> ()
    let args = System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail
    try
      aux 0 args
      for c in ctxList do c.Talk()
      for c in ctxList do c.CheckPoint()
      for c in ctxList do c.Finish()
    with SyntaxError (p, s) ->
      System.Console.WriteLine ("syntax error: {0}: {1}", p, s)
      
  do main()
