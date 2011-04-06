namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleSyntax

type PrettyPrinterFactory() =
  static member Printer (kind: string) = 
    match kind with
    | "simple" -> SimplePrettyPrinter() :> IPrettyPrinter
    | k -> failwith <| "Unrecognized parser kind: " + k
