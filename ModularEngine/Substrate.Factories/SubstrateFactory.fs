namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate.Basic
open Microsoft.Research.Dkal.Substrate.Sql
open Microsoft.Research.Dkal.Substrate.Xml
open Microsoft.Research.Dkal.Substrate.Crypto

open System.Xml.Linq
open System.IO

type SubstrateFactory() =
  
  static member Substrate (kind: string) (args: string list) (namespaces: string list) = 
    match kind, args with
    | "basic", [] -> new BasicSubstrate() :> ISubstrate
    | "sql", [cs; schema] -> new SqlSubstrate(cs, schema, namespaces) :> ISubstrate
    | "xml", [xml] -> 
      if xml.Contains "<" then
        new XmlSubstrate(XDocument.Parse(xml), namespaces) :> ISubstrate
      else
        new XmlSubstrate(xml, namespaces) :> ISubstrate
    | "crypto", [] -> CryptoSubstrate() :> ISubstrate
    | _ -> failwith <| "Unknown substrate kind/params for " + kind
