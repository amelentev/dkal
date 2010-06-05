namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.FSharp.Collections

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

type ParsingCtx() =
  let ctx = PreAst.Context.Make()
  let mutable me = None
  let parseAssertions toks = 
    let toks = toks |> Parser.addRules ctx |> Parser.applyRules ctx
    Resolver.resolveFunctions ctx
    let decls = List.collect (Resolver.resolve ctx) toks
    
    for a in decls do
      let ai = a.AssertionInfo
      match me with
        | Some p ->
          if p.internal_id = ai.principal.internal_id then
            ()
          else
            raise (SyntaxError (ai.origin, "policy for multiple principals supplied"))
        | None ->
          me <- Some ai.principal
        
    decls


  member this.Options = ctx.options

  member this.Me = 
    match me with
      | Some v -> v
      | None -> raise (SyntaxError (fakePos, "principal identity not provided"))

  member this.LookupOrAddPrincipal name =
    match ctx.principals.TryGetValue name with
      | true, p -> p
      | _ ->
        ctx.AddPrincipal name
        ctx.principals.[name]

  member this.LookupFunction name =
    ctx.functions.[name]

  member this.LookupType name =
    ctx.types.[name]

  member this.MakeVar name tp =
    ctx.MkVar name tp

  member this.ParsePrelude () =
    let prelude = Tokenizer.fromString Prelude.text
    Parser.addStandardRules ctx
    parseAssertions prelude

  member this.ParseStream filename getStream stream =
    let toks = Tokenizer.fromFile filename getStream stream
    parseAssertions toks

  member this.ParseFile filename =
    let getStream (pos, included) =
      let path = System.IO.Path.GetDirectoryName filename
      let name = System.IO.Path.Combine (path, included)
      File.OpenText name
    this.ParseStream filename getStream (File.OpenText filename)

  member this.ParseInfon s =
    let toks = Tokenizer.fromString s
    let toks = Parser.applyRules ctx toks
    match toks with
      | [t]
      | [t; PreAst.Tok.NewLine _] -> Resolver.resolveInfon ctx t
      | [PreAst.Tok.NewLine _]
      | [] -> raise (SyntaxError (fakePos, "infon expected"))
      | _ -> raise (SyntaxError (fakePos, "only one infon expected"))
