namespace Microsoft.Research.Dkal.LogicEngine.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.LogicEngine.Simple

/// The EngineFactory provides a factory to construct different logic engines.
/// A logic engine kind must be provided.
type LogicEngineFactory() =
  static member LogicEngine (kind: string) = 
    match kind with
    | "simple" -> new SimpleLogicEngine() :> ILogicEngine
    | k -> failwith <| "Unrecognized logic engine kind: " + k


