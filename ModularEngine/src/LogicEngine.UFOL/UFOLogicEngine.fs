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

namespace Microsoft.Research.Dkal.LogicEngine.UFOL

open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Translations.Z3Translator
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Infostrate.Z3
open Microsoft.Research.Dkal.Substrate.Basic
open Microsoft.Z3

/// The UFOL engine uses Z3 deducing engine to infer the condition formulae based on
/// the principals' substrate and infostrate information.
/// It provides support to formulae on the universal fragment of FOL
/// Initially does not support quotations
type UFOLogicEngine(assemblyInfo: MultiAssembly) = 

  let log = LogManager.GetLogger("LogicEngine.UFOL")

  // TODO check Z3 documentation for possible parameters for context construction
  let _z3context : Context = new Context()
  let mutable _z3solver: Solver option = None

  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let mutable _z3translator: Z3Translator option = None

  /// Information about declared relations for the DKAL script being analyzed
  let _assemblyInformation: MultiAssembly = assemblyInfo
  let _dkalRelationDeclarations = Dictionary<string, RelationDeclaration>()

  do
    _z3solver <- Some (_z3context.MkSimpleSolver())

  /// Lift all queries from infon that need to be forwarded to the substrates
  member private ufolengine.substrateQueries (infon: ITerm) = 
    seq {
        match infon with
        | AndInfon(infons) -> 
          yield! infons |>
                        Seq.fold (fun acc inf -> Seq.append acc (ufolengine.substrateQueries inf)) (seq [])
        | AsInfon(exp) -> yield exp
        | _ -> yield! []
    }

  /// Tries to derive the relation for every known domain object. Those that are derivable and saved and known to be true
  /// The rest will from now on be known as false
  member private ufolengine.doFreezeRelation (relationName: string) =
    let relDecl= _dkalRelationDeclarations.[relationName]
    let relArgs= relDecl.Args |> List.map(fun var -> var :> ITerm)
    let relDeclTypes = relDecl.Args |> List.map(fun var -> var.Type)
    let relAppInfon= App({Name= relationName; RetType = Type.Infon ; ArgsType = relDeclTypes; Identity=None }, relArgs)
    let substs = (ufolengine :> ILogicEngine).Derive relAppInfon [Substitution.Id]

    (_infostrate.Value :?> Z3Infostrate).learnRelationCompleteDomain(relAppInfon, substs)

  interface INegLogicEngine with
    member engine.Start() = 
      ()

    member engine.Stop() =
      /// TODO should cleanup and shutdown Z3 (?)
      ()

    member mle.Freeze(infon: ITerm) =
      match infon with
      | App(f, args) -> mle.doFreezeRelation(f.Name)
      | _ -> log.Error("Engine tried to freeze {0}", infon)
             failwith("Engine tried to freeze " + infon.ToString())
      true

    /// Given an infon ITerm with (possibly) free variables and an initial sequence
    /// of substitutions it returns all those (possibly specialized) substitutions
    /// that make the infon hold
    member engine.Derive (infon: ITerm) (substs: ISubstitution seq) =
      log.Debug("Derive {0}", infon)
      let mutable newlearned = true
      let mutable retSubsts = substs
      let originalSubsts= substs
      while newlearned do
          retSubsts <- originalSubsts
          retSubsts <- retSubsts |>
                             Z3TranslatorUtil.completeSubstitutions (infon) ((_infostrate.Value :?> Z3Infostrate).getDomains()) |>
                             SubstrateDispatcher.Solve (engine.substrateQueries infon)

          // learn new constants that cropped up from solving substrates to fixpoint
          newlearned <- (_infostrate.Value :?> Z3Infostrate).learnConstantsFromSubstitutions (Seq.toList retSubsts)

      let substs= retSubsts
      substs |>
        Z3TranslatorUtil.completeSubstitutions (infon) ((_infostrate.Value :?> Z3Infostrate).getDomains()) |>
        Seq.filter  (fun sub -> 
                     log.Debug("Deriving {0} through Z3", (_z3translator.Value :> ITranslator).translate(infon.Apply sub).getUnderlyingExpr())
                     _z3solver.Value.Push()
                     let notGuard= _z3context.MkNot((_z3translator.Value :> ITranslator).translate(infon.Apply sub).getUnderlyingExpr() :?> BoolExpr)
                     _z3solver.Value.Assert( notGuard )
                     let hasModel= _z3solver.Value.Check([||])
                     _z3solver.Value.Pop()
                     if hasModel = Status.UNSATISFIABLE then
                       log.Debug("Assertion is valid")
                     else if hasModel = Status.SATISFIABLE then
                       log.Debug("Assertion is invalid")
                       log.Debug("--- KNOWLEDGE ---\n{0}", (Printf.sprintf "%A" (Array.toList(_z3solver.Value.Assertions))))
  //                     log.Debug("--- MODEL ---\n{0}", _z3solver.Value.Model)
                     else
                       log.Debug("Assertion is UNKNOWN")
                       log.Debug(Printf.sprintf "--- KNOWLEDGE ---\n%A" (Array.toList(_z3solver.Value.Assertions)))
                     hasModel = Status.UNSATISFIABLE
                     )

    /// Constructs evidence for the given infon ITerm that matches the given 
    /// proofTemplate, if possible. Works under the given substitutions, returning
    /// more concrete ones (to instantiate the proofTemplate when successfull)
    member engine.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
      /// TODO not sure yet what this is supposed to do
      failwith "Not implemented"

    /// Checks if the given evidence is a well-formed justification, if it succeeds
    /// it returns the infon that is justified by the evidence; it it does not 
    /// suceed it returns None
    member engine.CheckJustification (evidence: ITerm) = // -> ITerm option
      /// TODO not sure yet what this is supposed to do
      failwith "Not implemented"

    member engine.set_Infostrate (infostrate: IInfostrate) =
      _infostrate <- Some infostrate
      let z3Infostrate= _infostrate.Value :?> Z3Infostrate
      // set up the Z3 infostrate
      z3Infostrate.setup(_z3context, _z3solver.Value, _assemblyInformation)
      _z3translator <- Some (z3Infostrate.getTranslator() :?> Z3Translator) 

    member engine.get_Infostrate () =
      _infostrate.Value

    member engine.set_SignatureProvider (sigProvider: ISignatureProvider) =
      _signatureProvider <- Some sigProvider
  
    /// The signature checking implementation for this logic engine
    member engine.get_SignatureProvider () =
      _signatureProvider.Value
