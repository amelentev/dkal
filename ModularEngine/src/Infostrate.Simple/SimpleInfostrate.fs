// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Infostrate.Simple

open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

/// The SimpleInfostrate accumulates knowledge as a set of facts
type SimpleInfostrate() = 
  let log = LogManager.GetLogger("Infostrate.Simple")

  /// Stores the known facts
  let knowledge = new HashSet<ITerm>()

  interface IInfostrate with

    // TODO: throw exception if trying to forget something more concrete than we know

    /// Split the infon into conjunctions and learn these recursively
    member si.Learn (infon: ITerm) = 
      log.Debug("Learn {0}", infon)
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to learn asInfon(...)"
      | Forall(v, t) -> (si :> IInfostrate).Learn t
      | infon -> 
        knowledge.Add infon

    /// Split the infon into conjunctions and forget these recursively
    member si.Forget (infon: ITerm) =
      log.Debug("Forget {0}", infon)
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to forget asInfon(...)"
      | Forall(v, t) -> (si :> IInfostrate).Forget t
      | infon ->
        knowledge.Remove infon

    member si.Knowledge =
      // variables in knowledges should not intersect
      let ind = ref 0
      let renamevars (h:ITerm) =
        let vsubst = h.Vars |> List.fold (fun (vsubst:ISubstitution) v ->
          let v' = {Name="H"+string(!ind) + "_" + v.Name; Type=v.Type}
          vsubst.Extend(v, v')) Substitution.Id
        incr ind
        h.Apply(vsubst)
      knowledge |> Seq.map renamevars