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

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.LogicEngine.FStar.Deps

module Utilities =
  type principal = string
  type substitution = Dictionary<Types.var, Types.term>

  let _value_knowledge (_infostrate : IInfostrate option) =
    _infostrate.Value.Knowledge |> Seq.toList |>
    List.map TranslationToFStar.FStarTermOfITerm

  let _value_checkSignature (_signatureProvider : ISignatureProvider option) (infon : Types.term) (principal : principal) (sign : obj) =
    _signatureProvider.Value.CheckSignature (TranslationFromFStar.ITermOfFStarTerm infon) principal (sign :?> int)

  (* Solve the given queries, invoking the necessary substrates, considering all   *)
  (* possible substitutions. Return more specialized substitutions (if successful) *)
  let _substrateDispatcher_solve (q : list<ISubstrateQueryTerm>) (s : list<TranslationToFStar.substitution>) 
    : list<TranslationToFStar.substitution> =
    let q' = Seq.ofList (List.map (fun q0 -> q0 :> ISubstrateQueryTerm) q)
    let s' = Seq.ofList (List.map TranslationFromFStar.ISubstitutionOfFStarSubstitution s)
    SubstrateDispatcher.Solve q' s' |> Seq.toList |>
    List.map TranslationToFStar.FStarSubstitutionOfISubstitution

  (* For CoreDKAL2 only *)
  let substrateQueryTerm_Vars (s:Builders.substrateQueryTerm) : Prims.list<Types.var> =
    Builders.PrimsListOfList (List.map TranslationToFStar.FStarVarOfIVar s.Vars)
  let substrateUpdateTerm_Vars (s:Builders.substrateUpdateTerm) : Prims.list<Types.var> =
    Builders.PrimsListOfList (List.map TranslationToFStar.FStarVarOfIVar s.Vars)
  let substrateQueryTerm_boundVars (s:Builders.substrateQueryTerm) : Prims.list<Types.var> =
    Builders.PrimsListOfList (List.map TranslationToFStar.FStarVarOfIVar s.BoundVars)
  let substrateUpdateTerm_boundVars (s:Builders.substrateUpdateTerm)  : Prims.list<Types.var> =
    Builders.PrimsListOfList (List.map TranslationToFStar.FStarVarOfIVar s.BoundVars)
  (*let substrateQueryTerm_apply (s:Builders.substrateQueryTerm) (s2:ISubstitution) : ITerm = s.Apply s2
  let substrateUpdateTerm_apply (s:Builders.substrateUpdateTerm) (s2:ISubstitution) : ITerm = s.Apply s2
  let _substrateQueryTerm_unifyFrom (s:Builders.substrateQueryTerm) (s2:ISubstitution) (t:ITerm) : ISubstitution option = s.UnifyFrom s2 t
  let _substrateUpdateTerm_unifyFrom (s:Builders.substrateUpdateTerm) (s2:ISubstitution) (t:ITerm) : ISubstitution option = s.UnifyFrom s2 t
  *)

  (* solution 1 (preferred but does not compile *)
  let mkSubst (al:Prims.list<Types.var>) (bl:Prims.list<Types.term>) : substitution =
    let res = new Dictionary<Types.var, Types.term>() in
    List.iter2 (fun a b -> res.[a] <- b)
      ((TypeHeaders.ListOfPrimsList.TyApp<Types.var>() 
          :?> (Prims.list<Types.var> -> list<Types.var>)) al)
      ((TypeHeaders.ListOfPrimsList.TyApp<Types.term>() 
          :?> (Prims.list<Types.term> -> list<Types.term>)) bl);
    res
  (* solution 2 (but can only compile the subst.fst with --UNSAFE) *)
  (*
  let mkSubst_aux (al:list<Types.var>) (bl:list<Types.term>) : substitution =
    let res = new Dictionary<Types.var, Types.term>() in
    List.iter2 (fun a b -> res.[a] <- b) al bl;
    res
  *)
  let substQuery (q:ISubstrateQueryTerm) (s:substitution) : ISubstrateQueryTerm =
    q.Apply(TranslationFromFStar.ISubstitutionOfFStarSubstitution s)
      :?> ISubstrateQueryTerm

  let emptySubst (b:bool) : substitution = 
    new Dictionary<Types.var, Types.term>()

  let extendSubst (s:substitution) (x:Types.var) (t:Types.term) =
      (* simple extension for now; does not replace x in all the images of s *)
      let newSubst = new Dictionary<_, _>(s)
      newSubst.[x] <- t
      newSubst
  
  let extends (s2:substitution) (s1:substitution) : bool =
    true (* TODO: dummy, need to get rid of it eventually *)

  let lookupVar (s:substitution) (x:Types.var) : Prims.option<Types.term> = 
    let found, ret = s.TryGetValue x in
    (if found then Some(ret) else None)
    |> Builders.PrimsOptionOfOption

  let domain (s:substitution) : Prims.list<Types.var> =
    [ for kvp in s -> kvp.Key ] |> Builders.PrimsListOfList