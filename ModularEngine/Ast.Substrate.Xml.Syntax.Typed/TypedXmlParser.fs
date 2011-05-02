namespace Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Typed

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic

/// The TypedParser parses from the typed concrete syntax, which uses explicit
/// typed variables
type TypedXmlParser() = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      ()

    member sp.SetNamespace (ns: string) = 
      Parser.Namespace <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      ()

    member sp.ParseTerm s = 
      let smt = Parser.XmlSubstrateTerm Lexer.tokenize (lexbuff s)
      smt :> ISubstrateTerm

