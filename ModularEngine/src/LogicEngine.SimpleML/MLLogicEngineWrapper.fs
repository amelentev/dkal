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

namespace Microsoft.Research.Dkal.LogicEngine.ML

open System.Collections.Generic
//open NLog
open Microsoft.Research.Dkal.LogicEngine
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

// The ML Engine is a translation to pure ML
// of the simple logic engine
type MLLogicEngine() =
 
  //let log = LogManager.GetLogger("LogicEngine.ML") 
  
  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let mutable _freshVarId = 0

  interface ILogicEngine with
    member mle.Start () = ()
    member mle.Stop () = ()

    member mle.set_Infostrate (infostrate: IInfostrate) =
      _infostrate <- Some infostrate

    member mle.get_Infostrate () =
      _infostrate.Value

    member mle.set_SignatureProvider (signatureProvider: ISignatureProvider) =
      _signatureProvider <- Some signatureProvider

    member mle.get_SignatureProvider () =
      _signatureProvider.Value

    member mle.Derive (target: ITerm) (substs: ISubstitution seq) = 
      substs |> List.ofSeq |> List.map Translation.MLsubstitutionOfISubstitution |>
      MLLogicEngineImpl.derive (Translation.MLtermOfITerm target) |>
      List.map Translation.ISubstitutionOfMLsubstitution |>
      Seq.ofList

    member mle.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
      substs |> List.ofSeq |> List.map Translation.MLsubstitutionOfISubstitution |>
      MLLogicEngineImpl.deriveJustification 
        (Translation.MLtermOfITerm infon) (Translation.MLtermOfITerm proofTemplate) |>
      List.map Translation.ISubstitutionOfMLsubstitution |>
      Seq.ofList

    member mle.CheckJustification (evidence: ITerm) =
      evidence  |> Translation.MLtermOfITerm |> MLLogicEngineImpl.checkJustification |>
       Option.map Translation.ITermOfMLterm

