namespace Microsoft.Research.Dkal.SimpleSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst

type SimpleParser(ctx: Context) = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface IParser with
    member sp.ParseTerm s = 
      let smt = Parser.Term Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleMetaTerm smt

    member sp.ParseInfon s = 
      let smt = Parser.Infon Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleMetaTerm smt

    member sp.ParseAssertion s = 
      let sa = Parser.Assertion Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleAssertion sa
    
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
