namespace Microsoft.Research.DkalEngine

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast

module Main =
  let getEngine trace filename hooks =
    try
      let prelude = Tokenizer.fromString Prelude.text
      let toks = prelude @ Tokenizer.fromFile filename
      let ctx = Context.Make()
      ctx.trace <- trace
      Parser.addStandardRules ctx
      let toks = Parser.addRules ctx toks
      let toks = Parser.applyRules ctx toks
      //System.Console.WriteLine (Tok.Block (fakePos, toks))
      Resolver.resolveFunctions ctx
      let assertions = List.map (Resolver.resolve ctx) toks |> List.concat
      
      let me = ctx.principals.[ctx.options.["me"]]
      let engine = Engine.Make(ctx, me, hooks)
      engine.AddDefaultFilter()
      engine.Populate assertions
      engine.SetTrace trace
      engine
      
    with SyntaxError (pos, s) ->
      System.Console.WriteLine (filename + ":" + pos.ToString() + ": " + s)
      System.Environment.Exit 1
      failwith ""
  
  let getEngineCmdLine hooks =
    let args = System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail
    let trace, args =
      match args with
        | "-v" :: args -> 1, args
        | "-vv" :: args -> 2, args
        | _ -> 0, args
    
    if args.Length <> 1 then
      System.Console.WriteLine "USAGE: dkal <file>"
      System.Environment.Exit 1
      failwith ""
    else
      getEngine trace args.[0] hooks
  


(*
type MyBus(ctx) =
  let engines = dict()
  
  member this.AddEngineFor principal =
    let e = Engine.Make (ctx, principal, this)
    e.AddDefaultFilter()
    engines.Add (principal.id, e)
    e
  
  member this.Ask (q:Knows) =
    let e = engines.[q.ai.principal.id]
    System.Console.WriteLine ("query: {0}", q.infon)
    for s in (e.Derive Map.empty q.infon).All do
      System.Console.WriteLine "result:"
      q.infon.Vars() |> List.iter (fun v -> System.Console.WriteLine ("  {0} -> {1}", v, s.[v.id].Apply s)) 
  
  member this.Talk () =
    for e in engines.Values do
      e.Talk()
      
  interface IBus with
    member this.Send (msg:Message) =
      System.Console.WriteLine ("{0} to {1} : {2} <-- {3}", msg.source, msg.target, msg.message, msg.proviso)
      let te = engines.[msg.target.id]
      te.Listen msg
      
    member this.Recieved (k:Knows) =
      System.Console.WriteLine ("{0} now knows : {1}", k.ai.principal, k.infon )

*)


