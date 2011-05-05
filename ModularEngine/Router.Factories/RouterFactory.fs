namespace Microsoft.Research.Dkal.Router.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Factories
open Microsoft.Research.Dkal.Router.Simple
open Microsoft.Research.Dkal.Router.Local

open System.Collections.Generic

/// The RouterFactory provides a factory to construct different routers. Router
/// kind and routingFile must be provided.
type RouterFactory() =
  static member Router (kind: string) (routingFile: string) = 
    match kind with
    | "simple" -> 
      let parser, printer = ParserFactory.InfonParser("typed", ""), PrettyPrinterFactory.InfonPrinter "typed"
      let routingTable = SimpleRoutingTable.FromXml routingFile
      new SimpleRouter(routingTable, parser, printer) :> IRouter
    | k -> failwith <| "Unrecognized router kind: " + k


  static member ManyRouters (kind: string) (ppals: string list) =
    match kind with 
    | "local" -> 
      let ret = new Dictionary<string, IRouter>()
      let mailer = new LocalMailer()
      for ppal in ppals do
        let rt = new LocalRoutingTable(ppal, mailer)
        ret.[ppal] <- new LocalRouter(rt, mailer)
      ret
    | k -> failwith <| "Unrecognized router kind: " + k
