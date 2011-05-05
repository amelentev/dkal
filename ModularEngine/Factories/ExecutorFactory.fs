namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Executor.Simple

/// The ExecutorFactory provides a factory to construct different executors.
/// An executor kind, a router and a logic engine must all be provided. 
type ExecutorFactory() =
  static member Executor (kind: string, router: IRouter, engine: ILogicEngine, ?stepbystep: bool) = 
    let stepbystep = 
      match stepbystep with
      | Some v -> v
      | _ -> true
    match kind with
    | "simple" -> new SimpleExecutor(router, engine, stepbystep) :> IExecutor
    | k -> failwith <| "Unrecognized executor kind: " + k


