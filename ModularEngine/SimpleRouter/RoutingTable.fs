namespace Microsoft.Research.Dkal.SimpleRouter

open System.Collections.Generic
open System.Xml
open System.IO

type RoutingTable(me: string, address: string) = 
  let principalAddresses = new Dictionary<string, string>()

  member rt.Me = me
  member rt.Address = address

  member rt.Principals =
    [ for kv in principalAddresses -> kv.Key ]

  member rt.AddPrincipal (name: string) (endpointAddress: string) = 
    principalAddresses.[name] <- endpointAddress

  member rt.GetPrincipalAddress (name: string) =
    let found, endpointAddress = principalAddresses.TryGetValue name
    if found then
      endpointAddress
    else
      failwith <| "Principal not known: " + name

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
    
