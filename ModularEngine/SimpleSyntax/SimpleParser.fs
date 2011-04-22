namespace Microsoft.Research.Dkal.SimpleSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with a Context that holds variable 
/// type information, relation declarations, etc.
type SimpleParser(ctx: Context) = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface IParser with
    member sp.ParseInfon s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleMetaTerm smt Infon

    member sp.ParseRule s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleMetaTerm smt Rule
    
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
