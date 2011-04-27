namespace Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.SqlSubstrate

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleSqlParser() = 

  let mutable _substrate: SqlSubstrate option = None
  let mutable _ns: string = null
  let mutable _types: Dictionary<string, IType> = null

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.SetTypesContext (types: Dictionary<string, IType>) =
      _types <- types

    member sp.SetNamespace (ns: string) = 
      _ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      match substrate with
      | :? SqlSubstrate as substrate -> 
        _substrate <- Some substrate
      | _ -> failwith "Expecting SqlSubstrate on SimpleSqlParser"

    member sp.ParseTerm s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      let t = (new Context(_substrate.Value, _types)).LiftSimpleMetaTerm smt None
      new DummySubstrateTerm(t, _ns) :> ISubstrateTerm

