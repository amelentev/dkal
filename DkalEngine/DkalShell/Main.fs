namespace Microsoft.Research.DkalEngine.Shell

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Util

type Global =
  {
    Contexts : Dict<string, Context>
  }

and Context(filename, gctx:Global) =
  let pctx = ParsingCtx ()
  let opts = Options.Create ()
  let eng = Engine.Config opts
  let serializer = Serializer(pctx)

  let msg (s:string) = 
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
    eng.Reset ()
    List.iter eng.AddAssertion assertions
    eng.AddDefaultFilter ()
    msg "loaded"

  member this.Me = pctx.Me
  member this.Id = principalId this.Me
  member this.RecieveMessage ser =
    let msg = serializer.DeserializeMessage ser
    eng.Listen (this, msg)
  member this.Talk () = eng.Talk this

  interface ICommunicator with
    member this.RequestFinished () = ()
    member this.ExceptionHandler e =
      msg ("exception: " + e.ToString())
    member this.Warning e = msg ("warning: " + e)
    member this.Knows _ = ()
    member this.QueryResults (inf, res) = ()
    member this.SendMessage msg =
      let target = gctx.Contexts.[msg.target.name]
      let msg' = serializer.SerializeMessage msg
      target.RecieveMessage msg'
    member this.PrincipalById id = principalById id  
    member this.PrincipalId (p:Ast.Principal) = principalId p


module Main =
  let main () =
    let gctx = { Contexts = dict() }
    let ctxList = vec()
    try
      for f in System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail do
        let c = Context (f, gctx)
        gctx.Contexts.Add (c.Me.Name, c)
        ctxList.Add c
      for c in ctxList do c.Talk()
    with SyntaxError (p, s) ->
      System.Console.WriteLine ("syntax error: {0}: {1}", p, s)
      
  do main()
