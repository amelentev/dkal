namespace Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with a Context that holds variable 
/// type information, relation declarations, etc.
type SimpleSqlParser(ns: string, ctx: Dictionary<string, IType>) = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.ParseTerm s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      ctx.LiftSimpleMetaTerm smt None

