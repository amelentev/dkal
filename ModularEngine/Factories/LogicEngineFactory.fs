namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.LogicEngine.Simple

/// The EngineFactory provides a factory to construct different logic engines.
/// An engine kind must be provided.
type LogicEngineFactory() =
  static member Engine (kind: string) = 
    match kind with
    | "simple" -> new SimpleLogicEngine() :> ILogicEngine
    | k -> failwith <| "Unrecognized engine kind: " + k


