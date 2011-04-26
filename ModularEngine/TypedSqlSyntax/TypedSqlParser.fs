namespace Microsoft.Research.Dkal.Substrate.TypedSqlSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

/// The TypedParser parses the typed concrete syntax, which carries type 
/// annotations in every function application and every variable
type TypedSqlParser(ns: string) = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.ParseTerm s =
      let t = Parser.ITerm Lexer.tokenize (lexbuff s)
      new DummySubstrateTerm(t, ns) :> ISubstrateTerm


