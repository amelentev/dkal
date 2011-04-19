namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleSyntax
open Microsoft.Research.Dkal.TypedSyntax

/// The PrettyPrinterFactory provides a factory to construct different pretty
/// printers. A pretty printer kind must be provided.
type PrettyPrinterFactory() =
  static member Printer (kind: string) = 
    match kind with
    | "simple" -> SimplePrettyPrinter() :> IPrettyPrinter
    | "typed" -> TypedPrettyPrinter() :> IPrettyPrinter
    | k -> failwith <| "Unrecognized pretty printer kind: " + k
