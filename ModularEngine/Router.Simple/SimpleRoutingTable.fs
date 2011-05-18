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

open System.Collections.Generic
open System.Xml
open System.IO

open Microsoft.Research.Dkal.Router

type ServiceAddress = 
  { Location: string } 
  with interface IPrincipalAddress 

/// A SimpleRoutingTable contains information about the current principal's name and
/// address as well as the names and addresses of all the other principals known
type SimpleRoutingTable(me: string, address: string) = 
  
  /// To keep the address of every other principal
  let principalAddresses = new Dictionary<string, ServiceAddress>()

  interface IRoutingTable with

    /// My name
    member rt.Me = me

    /// My address
    member rt.MyAddress = { Location = address } :> IPrincipalAddress

    /// Returns the list of principal names that I know
    member rt.Principals =
      [ for kv in principalAddresses -> kv.Key ]

    /// Returns true if the principal name is known to the RoutingTable
    member rt.HasPrincipal (name: string) =
      principalAddresses.ContainsKey name

    /// Adds the principal (with name and address) and returns true if the 
    /// principal was not present before addition
    member rt.AddPrincipal (name: string) (address: IPrincipalAddress) =
      match address with
      | :? ServiceAddress as address ->
        let hadPrincipal = (rt :> IRoutingTable).HasPrincipal name
        principalAddresses.[name] <- address
        not <| hadPrincipal
      | _ -> failwithf "Expecting ServiceAddress in SimpleRouter when adding principal"

    /// Gets the principal address from the RoutingTable
    member rt.PrincipalAddress (name: string) =
      let found, endpointAddress = principalAddresses.TryGetValue name
      if found then
        endpointAddress :> IPrincipalAddress
      else
        failwithf "Principal not known: %O" name

  /// Constructs a RoutingTable from an XML file
  static member FromXml (xmlFile: string) =
    let doc = new XmlDocument()
    doc.LoadXml(File.ReadAllText(xmlFile))
    let root = doc.DocumentElement
    match root.Name.ToLower() with
    | "routingtable" -> 
      let rt = new SimpleRoutingTable(root.Attributes.["me"].Value, root.Attributes.["address"].Value)
      for child in root.ChildNodes do
        if child.Name.ToLower() = "principal" then
          (rt :> IRoutingTable).AddPrincipal child.Attributes.["name"].Value {Location = child.Attributes.["address"].Value} |> ignore
      rt
    | _ -> failwith "Expecting 'RoutingTable' root element"
    
