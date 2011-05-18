// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Router.Simple

open System.ServiceModel
open System.Configuration
open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Router

/// A ConnectionHandler is responsible for creating and keeping connections
/// to every other principal that is known. It's also in charge of keeping a
/// running IPrincipalService to receive message from the outside
type ConnectionsHandler(rt: IRoutingTable, messageProcessingFunc: string -> string -> unit) as ch =
  let log = LogManager.GetLogger("Router.Simple")
  
  /// Stores the IPrincipalService reference for each known principal
  let channels = new Dictionary<string, IPrincipalService>()
  
  /// Stores the ChannelFactory for each known principal so that we can 
  /// close it later
  let factories = new Dictionary<string, ChannelFactory>()

  /// Stores a ServiceHost that runs the PrincipalService to wait for incoming
  /// messages
  let host = 
    match rt.MyAddress with
    | :? ServiceAddress as sa ->
      let wholeMessageProcessingFunc msg from fromAddress = 
        if rt.AddPrincipal from {Location = fromAddress} then
          ch.StartClient from
        messageProcessingFunc msg from
      new ServiceHost(new PrincipalService(wholeMessageProcessingFunc), new System.Uri(sa.Location))
    | _ -> failwith "Connections handler expects ServiceAddress"

  /// Initializes the server-side host
  member ch.StartServer() =
    // Establish server to listen for incoming connections
    host.Open()
    host.Closing.Add (fun _ -> log.Info("Incoming channel closing..."))
    log.Info("Principal service is up and running on the following addresses:")
    for ep in host.Description.Endpoints do
      log.Info("{0}", ep.Address)

  /// Initializes the client-side channels (and factories)
  member ch.StartClients() =
    // Create a channel (and factory) for each declared client in the config file
    channels.Clear(); factories.Clear()
    for ppal in rt.Principals do
      ch.StartClient ppal
          
  /// Initializes one client (and its factory)
  member private ch.StartClient (ppal: string) =
    log.Info("Creating channel to communicate with {0}", ppal)
    match rt.PrincipalAddress(ppal) with 
    | :? ServiceAddress as sa ->
      let factory = new ChannelFactory<IPrincipalService>(new BasicHttpBinding(), new EndpointAddress(sa.Location))
      channels.[ppal] <- factory.CreateChannel()
      factories.[ppal] <- factory
      factory.Closing.Add (fun _ -> log.Info("Channel for {0} closing...", ppal))
    | _ -> failwith "Connections handler expects ServiceAddress"

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
    let myAddress = (rt.MyAddress :?> ServiceAddress).Location
    if found then
      channel.ReceiveMessage(msg, rt.Me, myAddress)
    else
      failwith <| "Unknown destination: " + ppal