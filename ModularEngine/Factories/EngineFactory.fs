namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleEngine

type EngineFactory() =
  static member Engine (kind: string) = 
    match kind with
    | "simple" -> new SimpleEngine() :> IEngine
    | k -> failwith <| "Unrecognized engine kind: " + k


