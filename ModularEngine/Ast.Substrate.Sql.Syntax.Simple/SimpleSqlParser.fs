namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleSqlParser() = 

  let mutable _substrate: SqlSubstrate option = None
  let mutable _context: IParsingContext option = None
  let mutable _ns: string = null

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      _context <- Some context

    member sp.SetNamespace (ns: string) = 
      _ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      match substrate with
      | :? SqlSubstrate as substrate -> 
        _substrate <- Some substrate
      | _ -> failwith "Expecting SqlSubstrate on SimpleSqlParser"

    member sp.ParseTerm s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      let lifter = new Lifter(_substrate.Value, _context.Value, _ns)
      lifter.LiftSimpleMetaTerm smt
      
