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

    /// Gets the principal address from the RoutingTable
    member rt.PrincipalAddress (name: string) =
      let found, endpointAddress = principalAddresses.TryGetValue name
      if found then
        endpointAddress :> IPrincipalAddress
      else
        failwith <| "Principal not known: " + name

  /// Adds a principal to the RoutingTable
  member rt.AddPrincipal (name: string) (endpointAddress: string) = 
    principalAddresses.[name] <- { Location = endpointAddress }

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
          rt.AddPrincipal child.Attributes.["name"].Value child.Attributes.["address"].Value
      rt
    | _ -> failwith "Expecting 'RoutingTable' root element"
    
