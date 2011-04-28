namespace Microsoft.Research.Dkal.Router.Simple

open System.Collections.Generic
open System.Xml
open System.IO

/// A RoutingTable contains information about the current principal's name and
/// address as well as the names and addresses of all the other principals known
type RoutingTable(me: string, address: string) = 
  
  /// To keep the address of every other principal
  let principalAddresses = new Dictionary<string, string>()

  /// My name
  member rt.Me = me

  /// My address
  member rt.Address = address

  /// Returns the list of principal names that I know
  member rt.Principals =
    [ for kv in principalAddresses -> kv.Key ]

  /// Adds a principal to the RoutingTable
  member rt.AddPrincipal (name: string) (endpointAddress: string) = 
    principalAddresses.[name] <- endpointAddress

  /// Gets the principal address from the RoutingTable
  member rt.GetPrincipalAddress (name: string) =
    let found, endpointAddress = principalAddresses.TryGetValue name
    if found then
      endpointAddress
    else
      failwith <| "Principal not known: " + name

  /// Constructs a RoutingTable from an XML file
  static member FromXml (xmlFile: string) =
    let doc = new XmlDocument()
    doc.LoadXml(File.ReadAllText(xmlFile))
    let root = doc.DocumentElement
    match root.Name.ToLower() with
    | "routingtable" -> 
      let rt = new RoutingTable(root.Attributes.["me"].Value, root.Attributes.["address"].Value)
      for child in root.ChildNodes do
        if child.Name.ToLower() = "principal" then
          rt.AddPrincipal child.Attributes.["name"].Value child.Attributes.["address"].Value
      rt
    | _ -> failwith "Expecting 'RoutingTable' root element"
    
