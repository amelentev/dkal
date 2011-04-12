namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleSyntax
open Microsoft.Research.Dkal.TypedSyntax

type PrettyPrinterFactory() =
  static member Printer (kind: string) = 
    match kind with
    | "simple" -> SimplePrettyPrinter() :> IPrettyPrinter
    | "typed" -> TypedPrettyPrinter() :> IPrettyPrinter
    | k -> failwith <| "Unrecognized parser kind: " + k
