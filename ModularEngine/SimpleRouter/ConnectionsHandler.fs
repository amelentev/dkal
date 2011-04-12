namespace Microsoft.Research.Dkal.SimpleRouter

open System.ServiceModel
open System.Configuration
open System.Collections.Generic

type ConnectionsHandler(rt: RoutingTable, messageProcessingFunc) =
  // Create data structures to store client-side channels and factories
  let channels = new Dictionary<string, IPrincipalService>()
  let factories = new Dictionary<string, ChannelFactory>()

  // Create service host
  let host = new ServiceHost(new PrincipalService(messageProcessingFunc), new System.Uri(rt.Address))
    
  member ch.StartServer() =
    // Establish server to listen for incoming connections
    host.Open()
    host.Closing.Add (fun _ -> printfn "Incoming channel closing...")
    System.Console.WriteLine("Principal service is up and running on the following addresses:")
    for ep in host.Description.Endpoints do
      printfn "%A" ep.Address
    printfn ""

  member ch.StartClients() =
    // Create a channel (and factory) for each declared client in the config file
    channels.Clear(); factories.Clear()
    for ppal in rt.Principals do
      printfn "Creating channel to communicate with %O" ppal
      let factory = new ChannelFactory<IPrincipalService>(new BasicHttpBinding(), new EndpointAddress(rt.GetPrincipalAddress(ppal)))
      channels.[ppal] <- factory.CreateChannel()
      factories.[ppal] <- factory
      factory.Closing.Add (fun _ -> printfn "Channel for %O closing..." ppal)

  member ch.StopServer() =
    host.Close()

  member ch.StopClients() = 
    // Close client Side
    for factory in factories.Values do
      factory.Close()

  member ch.Send (msg: string) (ppal: string) =
    let found, channel = channels.TryGetValue ppal
    if found then
      channel.ReceiveMessage(msg, rt.Me)
    else
      failwith <| "Unknown destination: " + ppal