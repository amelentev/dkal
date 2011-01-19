// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.FSharp.Collections

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

/// A context for parsing a single policy file. After parsing the policy
/// with all function declarations, it can be used to parse a single infon.
/// The deserialization also depends on the function definitions accumulated here.
type ParsingCtx() =
  let ctx = PreAst.Context.Make()
  let mutable me = None
  let mutable firstId = ctx.id
  let parseAssertions toks = 
    firstId <- ctx.id
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
      | None -> 
        if ctx.me.IsSome then me <- ctx.me; me.Value
        else
          raise (SyntaxError (fakePos, "principal identity not provided"))

  member this.LateFunctions () =
    ctx.functions.Values |> Seq.filter (fun f -> f.id > firstId) |> Seq.sortBy (fun f -> f.id)

  /// Lookup or create a new named principal.
  member this.LookupOrAddPrincipal name =
    match ctx.principals.TryGetValue name with
      | true, p -> p
      | _ ->
        ctx.AddPrincipal name
        ctx.principals.[name]

  member this.LookupFunction name =
    ctx.functions.[name]

  member this.LookupType name =
    match ctx.types.TryGetValue name with
      | true, t -> t
      | _ -> failwith ("unknown type " + name)

  member this.MakeVar name tp =
    ctx.MkVar name tp

  /// Parse the builtin prelude file. Should be called before calling any other methods.
  member this.ParsePrelude () =
    let prelude = Tokenizer.fromString Prelude.text
    Parser.addStandardRules ctx
    parseAssertions prelude

  member this.ParseStreamEx filename getStream stream =
    let toks = Tokenizer.fromFile filename getStream stream
    parseAssertions toks

  /// Returns a list of Assertions from the specified stream. The filename is only used for error messages.
  member this.ParseStream filename stream =
    let getStream (pos, _) =
      raise (SyntaxError (pos, "#include not supported when parsing streams"))
    let toks = Tokenizer.fromFile filename getStream stream
    parseAssertions toks

  member this.ParseFile filename =
    let getStream (pos, included) =
      let path = System.IO.Path.GetDirectoryName filename
      let name = System.IO.Path.Combine (path, included)
      File.OpenText name
    this.ParseStreamEx filename getStream (File.OpenText filename)

  member this.ParseInfon s =
    let toks = Tokenizer.fromString s
    ctx.vars.Clear()
    let toks = Parser.applyRules ctx toks
    let rec parse t = match t with
                      | PreAst.Tok.NewLine _ -> []
                      | t -> [Resolver.resolveInfon ctx t]
    match toks with
      | [] -> raise (SyntaxError (fakePos, "infon expected"))
      | ts -> Resolver.multiAnd (List.collect parse ts)

  member this.SXToAssertions s =
    List.collect (fun t -> ResolverSX.resolve ctx t) s

  member this.SXToInfon s =
    ResolverSX.resolveInfon ctx s