namespace Microsoft.Research.Dkal.Router.Simple

open System.ServiceModel
open System.Configuration
open System.Collections.Generic

/// A ConnectionHandler is responsible for creating and keeping connections
/// to every other principal that is known. It's also in charge of keeping a
/// running IPrincipalService to receive message from the outside
type ConnectionsHandler(rt: RoutingTable, messageProcessingFunc) =
  
  /// Stores the IPrincipalService reference for each known principal
  let channels = new Dictionary<string, IPrincipalService>()
  
  /// Stores the ChannelFactory for each known principal so that we can 
  /// close it later
  let factories = new Dictionary<string, ChannelFactory>()

  /// Stores a ServiceHost that runs the PrincipalService to wait for incoming
  /// messages
  let host = new ServiceHost(new PrincipalService(messageProcessingFunc), new System.Uri(rt.Address))

  /// Initializes the server-side host
  member ch.StartServer() =
    // Establish server to listen for incoming connections
    host.Open()
    host.Closing.Add (fun _ -> printfn "Incoming channel closing...")
    System.Console.WriteLine("Principal service is up and running on the following addresses:")
    for ep in host.Description.Endpoints do
      printfn "%A" ep.Address
    printfn ""

  /// Initializes the client-side channels (and factories)
  member ch.StartClients() =
    // Create a channel (and factory) for each declared client in the config file
    channels.Clear(); factories.Clear()
    for ppal in rt.Principals do
      printfn "Creating channel to communicate with %O" ppal
      let factory = new ChannelFactory<IPrincipalService>(new BasicHttpBinding(), new EndpointAddress(rt.GetPrincipalAddress(ppal)))
      channels.[ppal] <- factory.CreateChannel()
      factories.[ppal] <- factory
      factory.Closing.Add (fun _ -> printfn "Channel for %O closing..." ppal)

  /// Stops the server-side host
  member ch.StopServer() =
    host.Close()

  /// Stops the client-side channels
  member ch.StopClients() = 
    // Close client Side
    for factory in factories.Values do
      factory.Close()

  /// Sends a message to the given principal by invoking the proper channel
  member ch.Send (msg: string) (ppal: string) =
    let found, channel = channels.TryGetValue ppal
    if found then
      channel.ReceiveMessage(msg, rt.Me)
    else
      failwith <| "Unknown destination: " + ppal