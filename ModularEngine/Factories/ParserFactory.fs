namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.SimpleSyntax
open Microsoft.Research.Dkal.Ast.TypedSyntax

/// The ParserFactory provides a factory to construct different parsers. A 
/// parser kind must be provided.
type ParserFactory() =
  static member AstParser (kind: string) = 
    match kind with
    | "simple" -> SimpleParser(Context()) :> IAstParser
    | "typed" -> TypedParser() :> IAstParser
    | k -> failwith <| "Unrecognized parser kind: " + k

