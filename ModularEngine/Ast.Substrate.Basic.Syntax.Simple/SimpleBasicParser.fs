namespace Microsoft.Research.Dkal.Ast.Substrate.Basic.Syntax.Simple

open System.IO

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate.Basic

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleBasicParser() = 

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      Parser.ctx <- Some context

    member sp.SetNamespace (ns: string) = 
      Parser.ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      ()

    member sp.ParseTerm s = 
      GeneralParser.TryParse (Parser.SubstrateTerm Lexer.tokenize) s 
      
      
