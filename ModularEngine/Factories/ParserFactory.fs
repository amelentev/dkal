namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleSyntax
open Microsoft.Research.Dkal.TypedSyntax

/// The ParserFactory provides a factory to construct different parsers. A 
/// parser kind must be provided.
type ParserFactory() =
  static member Parser (kind: string) = 
    match kind with
    | "simple" -> SimpleParser(Context()) :> IParser
    | "typed" -> TypedParser() :> IParser
    | k -> failwith <| "Unrecognized parser kind: " + k
