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

namespace Microsoft.Research.Dkal.Infostrate.Z3

open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Z3

/// The Z3 infostrate accumulates facts as Z3 assertions to the engine
type Z3Infostrate() = 
  let log = LogManager.GetLogger("Infostrate.Z3")

  /// Stores the known facts since it will be necessary to reset the Z3 solver
  let knowledge = new HashSet<ITerm>()
  let mutable _z3translator: ITranslator option= None
  let mutable _z3solver : Solver option = None
  let mutable _z3context: Context option = None
  
  member z3is.setTranslator(translator: ITranslator) =
    _z3translator <- Some translator

  member z3is.getTranslator() =
    _z3translator.Value

  member z3is.hasTranslator() =
    not _z3translator.IsNone

  member z3is.setSolver(solver: Solver) =
    _z3solver <- Some solver

  member z3is.setContext(context: Context) =
    _z3context <- Some context

  interface IInfostrate with
    /// Split the infon into conjunctions and learn these recursively
    member is.Learn (infon: ITerm) = 
      log.Debug("Learn {0}", infon)
      // solver only knows false at start
      if knowledge.Count = 0 then
        _z3solver.Value.Reset()

      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to learn asInfon(...)"
      | Forall(v, t) -> (is :> IInfostrate).Learn t
      | infon -> 
          let z3Infon= _z3translator.Value.translate(infon)
          log.Debug("Asserting to Z3 {0}", ((z3Infon.getUnderlyingExpr() :?> Expr)))
          _z3solver.Value.Assert([|z3Infon.getUnderlyingExpr() :?> BoolExpr|])
          knowledge.Add infon

    /// Split the infon into conjunctions and forget these recursively
    member is.Forget (infon: ITerm) =
      log.Debug("Forget {0}", infon)
      log.Debug("Forgetting everything")
      _z3solver.Value.Reset()
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to forget asInfon(...)"
      | Forall(v, t) -> (is :> IInfostrate).Forget t
      | infon ->
        let todel = knowledge |> Seq.filter (fun h -> infon.Unify(h) |> Option.exists (fun s -> s.IsVariableRenaming))
        todel |> List.ofSeq |> List.iter (fun x -> knowledge.Remove(x) |> ignore)
        log.Debug("Relearning everything except {0}", infon) // TODO quite inefficient as we have to translate again. Better to keep the associated translations
        knowledge |>
          Seq.iter (fun x -> ignore ((is :> IInfostrate).Learn(x)))
        not(todel |> Seq.isEmpty)

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