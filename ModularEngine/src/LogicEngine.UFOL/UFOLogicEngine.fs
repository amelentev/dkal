﻿// *********************************************************
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

open Microsoft.Z3

/// The UFOL engine uses Z3 deducing engine to infer the condition formulae based on
/// the principals' substrate and infostrate information.
/// It provides support to formulae on the universal fragment of FOL
/// Initially does not support quotations
type UFOLogicEngine(assemblyInfo: MultiAssembly) as this = 

  let log = LogManager.GetLogger("LogicEngine.UFOL")

  // TODO check Z3 documentation for possible parameters for context construction
  let _z3context : Context = new Context()
  let mutable _z3solver: Solver option = None
  
  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let mutable _z3translator: Z3Translator option = None

  /// Information about declared relations for the DKAL script being analyzed
  let _assemblyInformation: MultiAssembly = assemblyInfo
  let _z3TypeDefinitions= Z3TypeDefinition()
  let _z3RelationDefinitions= Dictionary<string, FuncDecl>()

  do
    /// Feed Z3 relation declarations and save them for when we have to reset Z3
    List.iter (fun rel -> ignore (this.makeZ3FunDecl rel.Name rel.Args)) assemblyInfo.Relations
    _z3solver <- Some (_z3context.MkSimpleSolver())
    _z3translator <- Some (Z3Translator(_z3context, _z3TypeDefinitions, _z3RelationDefinitions))

  /// The signature checking implementation for this logic engine
  member ufolengine.get_AssemblyInformation () =
    _assemblyInformation

  member private ufolengine.getZ3TypesArray(args: IVar list) =
    args |>
      List.fold (fun acc var -> acc @ [Z3TypesUtil.getZ3TypeSort(_z3TypeDefinitions.getZ3TypeForDkalType(var.Type.FullName), _z3context)]) [] |>
      List.toArray

  member private ufolengine.makeZ3FunDecl (name: string) (args: IVar list) =
    let rangeSort= _z3context.MkBoolSort();
    let domainSort= ufolengine.getZ3TypesArray(args)
    _z3RelationDefinitions.Add(name, _z3context.MkFuncDecl(name, domainSort, rangeSort))


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

  member private ufolengine.substsFromKnowledge (infon: ITerm) (subst: ISubstitution) (knowledge: ITerm seq) =
    knowledge |>
      Seq.fold (fun acc x -> match infon.UnifyFrom subst x with
                                | None -> acc
                                | Some sub -> sub::acc) []

  interface ILogicEngine with
    member engine.Start() = 
      ()

    member engine.Stop() =
      /// TODO should cleanup and shutdown Z3 (?)
      ()

    /// Given an infon ITerm with (possibly) free variables and an initial sequence
    /// of substitutions it returns all those (possibly specialized) substitutions
    /// that make the infon hold
    member engine.Derive (infon: ITerm) (substs: ISubstitution seq) =
      log.Debug("Derive {0}", infon)
      // TODO solve asInfon via substrates and assert substitutions
      seq {
          for subst in substs do
            // recursively solve substitutions for asInfon
            let infon= infon.Apply subst
            let knowledgeBasedSubsts = 
                  if not infon.Vars.IsEmpty then     // check the infostrate knowledge for possible substitutions
                    engine.substsFromKnowledge infon subst _infostrate.Value.Knowledge
                  else
                    [subst]
            for subst2 in knowledgeBasedSubsts do
              let specSubsts= SubstrateDispatcher.Solve (engine.substrateQueries infon) [subst2]
              for specSubst in specSubsts do
                let substInfon= infon.Apply specSubst
                _z3solver.Value.Push()
                _z3solver.Value.Assert( _z3context.MkNot((_z3translator.Value :> ITranslator).translate(substInfon.Apply specSubst).getUnderlyingExpr() :?> BoolExpr) )
                let hasModel= _z3solver.Value.Check([||])
                _z3solver.Value.Pop()
                match hasModel with
                  | Status.UNSATISFIABLE -> yield specSubst
                  | _ -> ()
      }

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
      z3Infostrate.setTranslator (_z3translator.Value)
      z3Infostrate.setContext(_z3context)
      z3Infostrate.setSolver(_z3solver.Value)

    member engine.get_Infostrate () =
      _infostrate.Value

    member engine.set_SignatureProvider (sigProvider: ISignatureProvider) =
      _signatureProvider <- Some sigProvider
  
    /// The signature checking implementation for this logic engine
    member engine.get_SignatureProvider () =
      _signatureProvider.Value
