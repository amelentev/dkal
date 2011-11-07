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

open System.Collections.Generic
//open NLog
open Microsoft.Research.Dkal.LogicEngine
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.LogicEngine.FStar.Deps

type substitution = Dictionary<Types.var, Types.term>

type Wrapper() =
  
  (*let rec PrimsListofList (l:list<'a>) : Prims.list<'a> = 
    match l with
    | [] -> new Prims.Nil<'a>() :> Prims.list<'a>
    | h::t -> new Prims.Cons<'a>(h, PrimsListofList t) :> Prims.list<'a>

  let PrimsOptionofOption (o:option<'a>) : Prims.option<'a> = 
    match o with
    | None -> new Prims.None<'a>() :> Prims.option<'a>
    | Some(a) -> new Prims.Some<'a>(a) :> Prims.option<'a>
  *)
  let _signatureProvider: ISignatureProvider option ref = ref None
  let _infostrate: IInfostrate option ref = ref None

  interface ILogicEngine with

    member le.Start () = ()
    member le.Stop () = ()

    member le.set_Infostrate (infostrate: IInfostrate) =
      _infostrate := Some infostrate

    member le.get_Infostrate () =
      (!_infostrate).Value

    member le.set_SignatureProvider (signatureProvider: ISignatureProvider) =
      _signatureProvider := Some signatureProvider

    member le.get_SignatureProvider () =
      (!_signatureProvider).Value

    member le.Derive (target: ITerm) (substs: ISubstitution seq) : ISubstitution seq = 
      substs |> List.ofSeq |>
      List.map TranslationToFStar.FStarSubstitutionOfISubstitution |>
      Builders.PrimsListOfList |> 
      InfonLogic.deriveWrapper 
        (!_infostrate |> Option.map (fun i -> i.Knowledge |> Seq.toList 
                                              |> List.map TranslationToFStar.FStarPolyTermOfITerm
                                              |> Builders.PrimsListOfList)
                      |> Builders.PrimsOptionOfOption)
        (TranslationToFStar.FStarTermOfITerm target) |>
      List.map TranslationFromFStar.ISubstitutionOfFStarSubstitution |>
      Seq.ofList

    member le.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
      substs |> List.ofSeq |> List.map TranslationToFStar.FStarSubstitutionOfISubstitution |>
      Builders.PrimsListOfList |>
      InfonLogic.deriveJustificationWrapper
        (Builders.PrimsOptionOfOption !_infostrate) (TranslationToFStar.FStarTermOfITerm infon) (TranslationToFStar.FStarTermOfITerm proofTemplate) |>
      List.map TranslationFromFStar.ISubstitutionOfFStarSubstitution |>
      Seq.ofList

    member le.CheckJustification (evidence: ITerm) =
      (*evidence  |> TranslationToFStar.FStarTermOfITerm |>
      InfonLogic.checkJustificationWrapper 
        (Builders.PrimsOptionOfOption !_signatureProvider) |>
      Option.map TranslationFromFStar.ITermOfFStarTerm*)
      failwith "TODO checkJustification"

