namespace Microsoft.Research.Dkal.Substrate.TypedSqlSyntax

open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

/// The TypedParser parses the typed concrete syntax, which carries type 
/// annotations in every function application and every variable
type TypedSqlParser() = 

  let mutable _ns: string = null
  
  interface ISubstrateParser with

    member sp.SetTypesContext (types: Dictionary<string, IType>) =
      ()

    member sp.SetNamespace (ns: string) = 
      _ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      ()

    member sp.ParseTerm s =
      let lexbuff s = LexBuffer<char>.FromString(s)
      let t = Parser.ITerm Lexer.tokenize (lexbuff s)
      new DummySubstrateTerm(t, _ns) :> ISubstrateTerm


