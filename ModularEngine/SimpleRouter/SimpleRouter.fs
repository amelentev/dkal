namespace Microsoft.Research.Dkal.SimpleRouter

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type SimpleRouter (routingFile: string, parser: IParser, printer: IPrettyPrinter) =
  let routingTable = RoutingTable.FromXml routingFile
  let mutable elevateMessageFunction = fun _ _ -> ()
  let connectionsHandler = new ConnectionsHandler(routingTable, 
                                                  fun msg from -> 
                                                    let infon = parser.ParseInfon msg
                                                    let ppal = Const(PrincipalConstant(from))
                                                    elevateMessageFunction infon ppal)

  interface IRouter with
    member sr.Receive newElevateMessageFunction =
      elevateMessageFunction <- newElevateMessageFunction
    
    member sr.Send infon ppal = 
      match ppal with
      | Const(PrincipalConstant(target)) -> 
        let msg = printer.PrintMetaTerm infon
        connectionsHandler.Send msg target
      | _ -> failwith "Expecting principal constant as destination when sending message"

    member sr.Start () = 
      connectionsHandler.StartServer()
      connectionsHandler.StartClients()

    member sr.Stop () =
      connectionsHandler.StopServer()
      connectionsHandler.StopClients()
