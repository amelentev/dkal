namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces

type SubstrateFactory() =
  
  static member Substrate (kind: string) (args: string list) (namespaces: string list) = 
    match kind, args with
    | "sql", [cs] -> () // TODO: implement
    | _ -> failwith <| "Unknown substrate kind/params for " + kind
    new DummySubstrate() :> ISubstrate // TODO: erase this line