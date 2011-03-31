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
    try
      let args = System.Environment.GetCommandLineArgs() |> Seq.toList
      
      match args with
      | [_; file] ->

        if not (File.Exists (file)) then
          printfn "File not found: %O" file
        else
          let sp = Parsing.parsePolicy (File.ReadAllText(file))
          printfn "%A" sp.RelationDeclarations 
          printfn "%A" sp.FunctionDeclarations
          printfn "%A" sp.Infons

          let ctx = Context()
          for srd in sp.RelationDeclarations do
            ctx.AddRelation srd
          for sfd in sp.FunctionDeclarations do
            ctx.AddFunction sfd

          let lifter = SimpleMetaTermLifter(ctx)
          for i in sp.Infons do
            let mt = lifter.LiftSimpleMetaTerm i
            printfn "%A" <| mt
            printfn "%A" <| mt.Typ

      | _ -> failwith "Expecting a single parameter"
    with e -> printfn "%O" e.Message