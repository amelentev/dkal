namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleSyntax
open Microsoft.Research.Dkal.TypedSyntax

type ParserFactory() =
  static member Parser (kind: string) = 
    match kind with
    | "simple" -> SimpleParser(Context()) :> IParser
    | "typed" -> TypedParser() :> IParser
    | k -> failwith <| "Unrecognized parser kind: " + k
