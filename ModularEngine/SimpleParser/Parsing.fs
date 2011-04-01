namespace Microsoft.Research.Dkal.SimpleParser

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.SimpleParser.SimpleAst

module Parsing =
  let parsePolicy s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.Policy Lexer.tokenize lexbuff

  let parseRelationDeclaration s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.RelationDeclaration Lexer.tokenize lexbuff

  let parseInfon s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.Infon Lexer.tokenize lexbuff


module Main = 
  do
    let args = System.Environment.GetCommandLineArgs() |> Seq.toList
      
    match args with
    | [_; file] ->

      if not (File.Exists (file)) then
        printfn "File not found: %O" file
      else
        let sp = Parsing.parsePolicy (File.ReadAllText(file))
        printfn "%A" sp.TypeDeclarations
        printfn "%A" sp.TableDeclarations
        printfn "%A" sp.RelationDeclarations 
        printfn "%A" sp.FunctionDeclarations
        printfn "%A" sp.Infons
        printfn ""

        let ctx = Context()
        ctx.LoadSimplePolicy(sp)
        
        printfn "%A" ctx.Types
        printfn "%A" ctx.Identifiers

//          let lifter = SimpleMetaTermLifter(ctx)
//          for i in sp.Infons do
//            let mt = lifter.LiftSimpleMetaTerm i
//            printfn "%A" <| mt
//            printfn "%A" <| mt.Typ

    | _ -> failwith "Expecting a single parameter"