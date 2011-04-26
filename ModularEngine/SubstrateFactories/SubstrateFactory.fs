namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SqlSubstrate

type SubstrateFactory() =
  
  static member Substrate (kind: string) (args: string list) (namespaces: string list) = 
    match kind, args with
    | "sql", [cs; schema] -> new SqlSubstrate(cs, schema, namespaces) :> ISubstrate
    | _ -> failwith <| "Unknown substrate kind/params for " + kind
