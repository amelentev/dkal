namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple

open System.IO

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Syntax.Parsing

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with a Context that holds variable 
/// type information, relation declarations, etc.
type SimpleParser() = 

  let checkMacrosAndType (t: ITerm, solvedMacros: ISubstrateQueryTerm list) (typ: IType option) =
    if not solvedMacros.IsEmpty then
      failwithf "Unresolved macros in %O" t
    elif typ.IsSome && typ.Value <> t.Type then
      failwithf "Incorrect type, expecting %O, found: %O in %O" typ.Value.FullName t.Type.FullName t
    else
      t      

  interface IInfonParser with
    member sp.SetParsingContext (parsingContext: IParsingContext) = 
      Parser.ctxs.Push parsingContext

    member sp.ParseType s = 
      GeneralParser.TryParse (Parser.Type Lexer.tokenize) s 
      
    member sp.ParseTerm s = 
      let t = GeneralParser.TryParse (Parser.Term Lexer.tokenize) s 
      checkMacrosAndType t None
      
    member sp.ParseInfon s = 
      let t = GeneralParser.TryParse (Parser.Term Lexer.tokenize) s 
      checkMacrosAndType t (Some Type.Infon)
      
    member sp.ParseRule s = 
      let t = GeneralParser.TryParse (Parser.Term Lexer.tokenize) s 
      checkMacrosAndType t (Some Type.Rule)
    
    member sp.ParsePolicy s = 
      GeneralParser.TryParse (Parser.Policy Lexer.tokenize) s 

    member sp.ParseSignature s =
      GeneralParser.TryParse (Parser.Signature Lexer.tokenize) s

    member sp.ParseAssembly s =
      GeneralParser.TryParse (Parser.Assembly Lexer.tokenize) s 
