namespace Microsoft.Research.Dkal.Infostrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Infostrate.Simple

/// The InfostrateFactory provides a factory to construct different infostrates.
/// An infostrate kind must be provided. 
type InfostrateFactory() =
  static member Infostrate (kind: string) = 
    match kind with
    | "simple" -> new SimpleInfostrate() :> IInfostrate
    | k -> failwith <| "Unrecognized infostrate kind: " + k


