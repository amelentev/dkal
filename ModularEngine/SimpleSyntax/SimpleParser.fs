namespace Microsoft.Research.Dkal.SimpleSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst
open Microsoft.Research.Dkal.SimpleSyntax.TypeErrors

type SimpleParser(ctx: Context) = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface IParser with
    member sp.ParseInfon s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      typeCheck (ctx.LiftSimpleMetaTerm smt) Infon

    member sp.ParseRule s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      typeCheck (ctx.LiftSimpleMetaTerm smt) Rule
    
    member sp.ParsePolicy s = 
      let sp = Parser.Policy Lexer.tokenize (lexbuff s)
      ctx.LiftSimplePolicy sp

    member sp.ParseSignature s =
      let ss = Parser.Signature Lexer.tokenize (lexbuff s)
      ctx.LoadSimpleSignature ss
      ctx.LiftSimpleSignature ss

    member sp.ParseAssembly s =
      let sa = Parser.Assembly Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleAssembly sa
