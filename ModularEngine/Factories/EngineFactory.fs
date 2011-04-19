namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleEngine

/// The EngineFactory provides a factory to construct different logic engines.
/// An engine kind must be provided.
type EngineFactory() =
  static member Engine (kind: string) = 
    match kind with
    | "simple" -> new SimpleEngine() :> IEngine
    | k -> failwith <| "Unrecognized engine kind: " + k


