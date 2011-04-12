namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleRouter

type RouterFactory() =
  static member Router (kind: string) (routingFile: string) (parser: IParser) (printer: IPrettyPrinter) = 
    match kind with
    | "simple" -> new SimpleRouter(routingFile, parser, printer) :> IRouter
    | k -> failwith <| "Unrecognized parser kind: " + k


