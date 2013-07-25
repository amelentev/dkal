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
  let _dkalRelationDeclarations = Dictionary<string, RelationDeclaration>()

  do
    /// Feed Z3 relation declarations and save them for when we have to reset Z3
    List.iter (fun rel ->
                         ignore (this.makeZ3FunDecl rel.Name rel.Args)
                         _dkalRelationDeclarations.Add(rel.Name, rel)) assemblyInfo.Relations
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
    // relations need world knowledge for "said"
    // the world goes first for later convenience
    let domainSort= Array.append ([|Z3TypesUtil.getZ3TypeSort(_z3TypeDefinitions.getZ3WorldSort(), _z3context)|]) (domainSort)
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


  /// Tries to derive the relation for every known domain object. Those that are derivable and saved and known to be true
  /// The rest will from now on be known as false
  member private ufolengine.doFreezeRelation (relationName: string) =
    let relDecl= _dkalRelationDeclarations.[relationName]
    let relArgs= relDecl.Args |> List.map(fun var -> var :> ITerm)
    let relDeclTypes = relDecl.Args |> List.map(fun var -> var.Type)
    let relAppInfon= App({Name= relationName; RetType = Type.Infon ; ArgsType = relDeclTypes; Identity=None }, relArgs)
    let substs = (ufolengine :> ILogicEngine).Derive relAppInfon [Substitution.Id]

    (_infostrate.Value :?> Z3Infostrate).learnRelationCompleteDomain(relAppInfon, substs)

  interface ILogicEngine with
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
      let substs= substs |>
                         Z3TranslatorUtil.completeSubstitutions (infon) ((_infostrate.Value :?> Z3Infostrate).getDomains()) |>
                         SubstrateDispatcher.Solve (engine.substrateQueries infon)

      // TODO probably needs to be done to fixpoint
      // learn new constants that cropped up from solving substrates
      (_infostrate.Value :?> Z3Infostrate).learnConstantsFromSubstitutions (Seq.toList substs)

      substs |>
        Z3TranslatorUtil.completeSubstitutions (infon) ((_infostrate.Value :?> Z3Infostrate).getDomains()) |>
        Seq.filter  (fun sub -> 
                     log.Debug("Deriving {0} through Z3", (_z3translator.Value :> ITranslator).translate(infon.Apply sub).getUnderlyingExpr())
                     _z3solver.Value.Push()
                     let notGuard= _z3context.MkNot((_z3translator.Value :> ITranslator).translate(infon.Apply sub).getUnderlyingExpr() :?> BoolExpr)
                     _z3solver.Value.Assert( notGuard )
                     let hasModel= _z3solver.Value.Check([||])
                     _z3solver.Value.Pop()
                     log.Debug("{0} valid", (if hasModel = Status.UNSATISFIABLE then "Is" else "Is NOT"))
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
      z3Infostrate.setTranslator (_z3translator.Value)
      _z3translator.Value.setVariableDomains (z3Infostrate.getDomains())
      z3Infostrate.setContext(_z3context)
      z3Infostrate.setSolver(_z3solver.Value)
      // learn principals
      _assemblyInformation.PrincipalPolicies.Keys |> z3Infostrate.learnPrincipals

    member engine.get_Infostrate () =
      _infostrate.Value

    member engine.set_SignatureProvider (sigProvider: ISignatureProvider) =
      _signatureProvider <- Some sigProvider
  
    /// The signature checking implementation for this logic engine
    member engine.get_SignatureProvider () =
      _signatureProvider.Value
