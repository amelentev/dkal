namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate.Sql
open Microsoft.Research.Dkal.Substrate.Xml
open Microsoft.Research.Dkal.Substrate.Crypto

type SubstrateFactory() =
  
  static member Substrate (kind: string) (args: string list) (namespaces: string list) = 
    match kind, args with
    | "sql", [cs; schema] -> new SqlSubstrate(cs, schema, namespaces) :> ISubstrate
    | "xml", [xmlFile] -> new XmlSubstrate(xmlFile, namespaces) :> ISubstrate
    | "crypto", [] -> CryptoSubstrate() :> ISubstrate
    | _ -> failwith <| "Unknown substrate kind/params for " + kind
