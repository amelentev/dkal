namespace Microsoft.Research.Dkal.Router.Simple

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Router

/// The SimpleRouter provides a IRouter interface by means of web services.
/// A RoutingTable is constructed by reading the XML from the given routingFile.
/// A ConnectionsHandler instance is used to do the actual sending and receiving
/// of messages. Infon MetaTerms are serialized and deserialized using the given
/// IParser and IPrinter implementations.
type SimpleRouter (routingTable: IRoutingTable, parser: IInfonParser, printer: IInfonPrettyPrinter) =
  
  /// This function is called every time a new message arrives. Initially it
  /// does nothing, it must be set by calling sr.Receive(...)
  let mutable elevateMessageFunction = fun _ -> ()

  /// A ConnectionHandler instance to manage the incoming and outcoming channels
  let connectionsHandler = new ConnectionsHandler(routingTable, 
                                                  fun msg -> 
                                                    let infon = 
                                                      try
                                                        parser.ParseInfon msg
                                                      with
                                                        e -> failwithf "%O" e
                                                    elevateMessageFunction infon)

  interface IRouter with
    member sr.Me = routingTable.Me
    
    member sr.Roster = routingTable.Principals

    member sr.Receive newElevateMessageFunction =
      elevateMessageFunction <- newElevateMessageFunction
    
    member sr.Send infon ppal = 
      match ppal with
      | PrincipalConstant(target) -> 
        sr.DoSend infon target
      | Var(v) -> 
        for ppalName in (sr :> IRouter).Roster do
          let s = Substitution.Id.Extend (v :> IVar, PrincipalConstant(ppalName))
          sr.DoSend (infon.Apply(s)) ppalName
      | _ -> failwithf "Expecting principal constant or variable as destination when sending message, found %O" ppal

    member sr.Start () = 
      connectionsHandler.StartServer()
      connectionsHandler.StartClients()

    member sr.Stop () =
      connectionsHandler.StopServer()
      connectionsHandler.StopClients()

  member private sr.DoSend infon ppalName = 
    printfn ">>>>>>\r\n>>>>>> SENT TO %O: %O\r\n>>>>>>" ppalName infon
    let msg = printer.PrintTerm infon
    connectionsHandler.Send msg ppalName

    
    