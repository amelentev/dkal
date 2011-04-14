namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SimpleExecutor

type ExecutorFactory() =
  static member Executor (kind: string) (router: IRouter) (engine: IEngine) = 
    match kind with
    | "simple" -> new SimpleExecutor(router, engine) :> IExecutor
    | k -> failwith <| "Unrecognized executor kind: " + k


