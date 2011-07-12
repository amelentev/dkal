(*
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
*)

namespace Microsoft.Research.Dkal.LogicEngine.ML

open Microsoft.Research.Dkal.Interfaces // for ISignatureProvider and IInfostrate
open Microsoft.Research.Dkal.Substrate // for SubstrateDispatcher

module Utilities =

  let int_to_string (i: int) =
    i.ToString()

  let value_knowledge (_infostrate : IInfostrate option) =
    _infostrate.Value.Knowledge |> Seq.toList |>
    List.map TranslationtoML.MLtermOfITerm

  let value_checkSignature (_signatureProvider : ISignatureProvider option) (infon : MLType.term) (principal : MLType.principal) (sign : obj) =
    _signatureProvider.Value.CheckSignature (TranslationfromML.ITermOfMLterm infon) principal (sign :?> int)

  let substrateDispatcher_solve (q : MLType.substrateQueryTerm list) s =
    let q' = Seq.ofList (List.map (fun q0 -> q0 :> ISubstrateQueryTerm) q)
    let s' = Seq.ofList (List.map TranslationfromML.ISubstitutionOfMLsubstitution s)
    SubstrateDispatcher.Solve q' s' |> Seq.toList |>
    List.map TranslationtoML.MLsubstitutionOfISubstitution

