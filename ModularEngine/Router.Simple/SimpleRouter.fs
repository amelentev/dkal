namespace Microsoft.Research.Dkal.Router.Simple

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Interfaces


/// The SimpleRouter provides a IRouter interface by means of web services.
/// A RoutingTable is constructed by reading the XML from the given routingFile.
/// A ConnectionsHandler instance is used to do the actual sending and receiving
/// of messages. Infon MetaTerms are serialized and deserialized using the given
/// IParser and IPrinter implementations.
type SimpleRouter (routingFile: string, parser: IInfonParser, printer: IInfonPrettyPrinter) =
  
  /// Stores the principals names and addresses, including the local name 
  /// and address
  let routingTable = RoutingTable.FromXml routingFile

  /// This function is called every time a new message arrives. Initially it
  /// does nothing, it must be set by calling sr.Receive(...)
  let mutable elevateMessageFunction = fun _ _ -> ()

  /// A ConnectionHandler instance to manage the incoming and outcoming channels
  let connectionsHandler = new ConnectionsHandler(routingTable, 
                                                  fun msg from -> 
                                                    let infon = 
                                                      try
                                                        parser.ParseInfon msg
                                                      with
                                                        e -> failwithf "%O" e
                                                    let ppal = Const(PrincipalConstant(from))
                                                    elevateMessageFunction infon ppal)

  interface IRouter with
    member sr.Receive newElevateMessageFunction =
      elevateMessageFunction <- newElevateMessageFunction
    
    member sr.Send infon ppal = 
      match ppal with
      | Principal(target) -> 
        let msg = printer.PrintTerm infon
        connectionsHandler.Send msg target
      | _ -> failwith "Expecting principal constant as destination when sending message"

    member sr.Start () = 
      connectionsHandler.StartServer()
      connectionsHandler.StartClients()

    member sr.Stop () =
      connectionsHandler.StopServer()
      connectionsHandler.StopClients()

    
    