namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleSyntax

type ParserFactory() =
  static member Parser (kind: string) = 
    match kind with
    | "simple" -> SimpleParser(Context()) :> IParser
    | k -> failwith <| "Unrecognized parser kind: " + k
