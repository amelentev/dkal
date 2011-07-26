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

namespace Microsoft.Research.Dkal.LogicEngine.FStar.Wrapper

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

module Utilities =
  type principal = string

  let _value_knowledge (_infostrate : IInfostrate option) =
    _infostrate.Value.Knowledge |> Seq.toList |>
    List.map TranslationToFStar.FStarTermOfITerm

  let _value_checkSignature (_signatureProvider : ISignatureProvider option) (infon : Types.term) (principal : principal) (sign : obj) =
    _signatureProvider.Value.CheckSignature (TranslationFromFStar.ITermOfFStarTerm infon) principal (sign :?> int)

  let _substrateDispatcher_solve (q : list<ISubstrateQueryTerm>) s =
    let q' = Seq.ofList (List.map (fun q0 -> q0 :> ISubstrateQueryTerm) q)
    let s' = Seq.ofList (List.map TranslationFromFStar.ISubstitutionOfFStarSubstitution s)
    SubstrateDispatcher.Solve q' s' |> Seq.toList |>
    List.map TranslationToFStar.FStarSubstitutionOfISubstitution

