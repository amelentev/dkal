namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleRouter

/// The RouterFactory provides a factory to construct different routers. Router
/// kind and routingFile must be provided.
type RouterFactory() =
  static member Router (kind: string) (routingFile: string) = 
    match kind with
    | "simple" -> 
      let parser, printer = ParserFactory.AstParser "typed", PrettyPrinterFactory.AstPrinter "typed"
      new SimpleRouter(routingFile, parser, printer) :> IRouter
    | k -> failwith <| "Unrecognized router kind: " + k


