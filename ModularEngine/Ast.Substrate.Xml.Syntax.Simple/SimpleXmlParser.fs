namespace Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Simple

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleXmlParser() = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      Parser.Context <- Some context

    member sp.SetNamespace (ns: string) = 
      Parser.Namespace <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      ()

    member sp.ParseTerm s = 
      let smt = Parser.XmlSubstrateTerm Lexer.tokenize (lexbuff s)
      smt :> ISubstrateTerm

