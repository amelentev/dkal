namespace Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.SqlSubstrate

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleSqlParser(substrate: SqlSubstrate, ns: string, types: Dictionary<string, IType>) = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.ParseTerm s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      let t = (new Context(substrate, types)).LiftSimpleMetaTerm smt None
      new DummySubstrateTerm(t, ns) :> ISubstrateTerm

