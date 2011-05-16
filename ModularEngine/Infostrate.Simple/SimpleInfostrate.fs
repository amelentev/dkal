namespace Microsoft.Research.Dkal.Infostrate.Simple

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

/// The SimpleInfostrate accumulates knowledge as a set of facts
type SimpleInfostrate() = 

  /// Stores the known facts
  let knowledge = new HashSet<ITerm>()

  interface IInfostrate with

    /// Split the infon into conjunctions and learn these recursively
    member si.Learn (infon: ITerm) = 
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to learn asInfon(...)"
      | AndInfon(infons) ->
        List.fold (fun ch i -> 
                    let ch' = (si :> IInfostrate).Learn i
                    ch' || ch) false infons 
      | infon -> 
        knowledge.Add infon

    /// Split the infon into conjunctions and forget these recursively
    member si.Forget (infon: ITerm) =
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to forget asInfon(...)"
      | AndInfon(infons) ->
        List.fold (fun ch i -> 
                    let ch' = (si :> IInfostrate).Forget i
                    ch' || ch) false infons 
      | infon -> 
        knowledge.Remove infon

    member si.Knowledge = seq knowledge 
