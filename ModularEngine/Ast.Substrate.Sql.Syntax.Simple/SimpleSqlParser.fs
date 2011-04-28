namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleSqlParser() = 

  let mutable _substrate: SqlSubstrate option = None
  let mutable _ns: string = null
  let mutable _macros: Dictionary<string, IType * ISubstrateTerm * IVar list> = null
  let mutable _types: Dictionary<string, IType> = null
  let mutable _tmpId: int = 0

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface ISubstrateParser with

    member sp.SetMacrosContext (macros: Dictionary<string, IType * ISubstrateTerm * IVar list>) = 
      _macros <- macros

    member sp.SetTypesContext (types: Dictionary<string, IType>) =
      _types <- types

    member sp.SetTempId (tmpId: int) =
      _tmpId <- tmpId

    member sp.SetNamespace (ns: string) = 
      _ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      match substrate with
      | :? SqlSubstrate as substrate -> 
        _substrate <- Some substrate
      | _ -> failwith "Expecting SqlSubstrate on SimpleSqlParser"

    member sp.ParseTerm s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      let t = (new Context(_substrate.Value, _types, _macros, _tmpId)).LiftSimpleMetaTerm smt None
      new DummySubstrateTerm(t, _ns) :> ISubstrateTerm

