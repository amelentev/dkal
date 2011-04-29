namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple.SimpleAst

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with a Context that holds variable 
/// type information, relation declarations, etc.
type SimpleParser() = 

  let mutable _parsingContext: IParsingContext option = None
  let mutable _lifter: Lifter option = None

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface IInfonParser with
    member sp.SetParsingContext (parsingContext: IParsingContext) = 
      _parsingContext <- Some parsingContext
      _lifter <- Some <| new Lifter(parsingContext)

    member sp.ParseType s = 
      let st = Parser.Type Lexer.tokenize (lexbuff s)
      _parsingContext.Value.TypeFromName st

    member sp.ParseTerm s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      _lifter.Value.LiftSimpleMetaTerm smt None

    member sp.ParseInfon s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      _lifter.Value.LiftSimpleMetaTerm smt (Some Type.Infon)

    member sp.ParseRule s = 
      let smt = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      _lifter.Value.LiftSimpleMetaTerm smt (Some Type.Rule)
    
    member sp.ParsePolicy s = 
      let sp = Parser.Policy Lexer.tokenize (lexbuff s)
      _lifter.Value.LiftSimplePolicy sp

    member sp.ParseSignature s =
      let ss = Parser.Signature Lexer.tokenize (lexbuff s)
      _lifter.Value.LoadSimpleSignature ss
      _lifter.Value.LiftSimpleSignature ss

    member sp.ParseAssembly s =
      let sa = Parser.Assembly Lexer.tokenize (lexbuff s)
      _lifter.Value.LiftSimpleAssembly sa
