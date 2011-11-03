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

open System.Collections.Generic
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

module Term =
  let rec vars (t: MLType.term) : MLType.var list =
    match t with
    | MLType.Forall(v, t) ->       
      let termVars = new HashSet<_>(vars t)
      termVars.Remove(v) |> ignore
      termVars |> Seq.toList
    | MLType.App(_, tl) -> new HashSet<_>(List.collect (fun (a: MLType.term) -> vars a) tl) |> Seq.toList
    | MLType.Var v -> [v]
    | MLType.Const _ -> []
    | MLType.ConcretizationEvidence(t, s) -> 
      let ret = new HashSet<_>(vars t)
      for v in Subst.domain s do  
        if ret.Contains(v) then
          ret.Remove v |> ignore
          ret.UnionWith <| vars (Subst.subst_apply s v)
      ret |> Seq.toList
    | MLType.SubstrateQueryTerm(t0) ->
      t0.Vars |> Seq.toList |> List.map TranslationtoML.MLvarOfIVar
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.Vars |> Seq.toList |> List.map TranslationtoML.MLvarOfIVar

  let rec boundVars (t: MLType.term) : MLType.var list =
    match t with
    | MLType.Forall(v, t) ->
      new HashSet<_>(v :: boundVars t) |> Seq.toList
    | MLType.App(_, tl) -> new HashSet<_>(List.collect (fun (a: MLType.term) -> vars a) tl) |> Seq.toList
    | MLType.Var _ -> []
    | MLType.Const _ -> []
    | MLType.ConcretizationEvidence(t, s) -> 
      let ret = new HashSet<_>(boundVars t)
      for v in Subst.domain s do  
        if ret.Contains(v) then
          ret.Remove v |> ignore
          ret.UnionWith <| boundVars (Subst.subst_apply s v)
      ret |> Seq.toList
    | MLType.SubstrateQueryTerm(t0) ->
      t0.BoundVars |> Seq.toList |> List.map TranslationtoML.MLvarOfIVar
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.BoundVars |> Seq.toList |> List.map TranslationtoML.MLvarOfIVar

  let freshVar (var: MLType.var) (otherVars: MLType.var list) =
  // from ForallTerm.fs
  // TODO: uses objects on strings
    let prefix = "FreshVar#"
    let freshVars = List.filter (fun (v: MLType.var) -> (v.name).StartsWith(prefix)) (var::otherVars)
    let freshNumbers = List.map (fun (v: MLType.var) -> System.Int32.Parse((v.name).Substring(prefix.Length))) freshVars
    let freshVarId = if List.isEmpty freshNumbers then 0 else (List.max freshNumbers) + 1
    let freshVar = { MLType.name = prefix + freshVarId.ToString(); MLType.typ = var.typ }
    let subst = Subst.extend Subst.id (var, MLType.Var(freshVar))
    freshVar, subst

  let rec composeWith (s: MLType.substitution) (s': MLType.substitution) : MLType.substitution =
  // from Subst; moved here because of mutual recursions
    let (newSubst : MLType.substitution) = new Dictionary<_, _>(s)
    for v in Subst.domain s' do
      newSubst.[v] <- term_apply (Subst.subst_apply s' v) s
    for v in Subst.domain s do
      if not <| Subst.domainContains s' v then
        newSubst.[v] <- Subst.subst_apply s v
    newSubst

  // Subst.composeWith (above) and Term.term_apply (below) are mutually recursive!!
  and term_apply (t: MLType.term) (s: MLType.substitution) : MLType.term =
    match t with
    | MLType.Var(v) -> Subst.subst_apply s v // from Variable.fs
    | MLType.Const _ -> t // from Constants.fs
    | MLType.Forall(v0, t0) -> // from ForallTerm.fs
      // the substitution is not applied to the quantified variable
      let s = Subst.forget s [v0]
      // check that there will be no variable capture
      let varsToCheck = new HashSet<_>(vars t0)
      varsToCheck.IntersectWith (Subst.domain s)
      let mappedVars = Seq.collect (fun (v': MLType.var) -> vars (Subst.subst_apply s v')) varsToCheck
      if Seq.exists (fun v -> v = v0) mappedVars 
      then
        let newVar, newVarSubst = freshVar v0 ((vars t0) @ (mappedVars |> Seq.toList))
        MLType.Forall(newVar, term_apply (term_apply t0 newVarSubst) s)
      else
        MLType.Forall(v0, term_apply t0 s)
    | MLType.App(f, tl) -> MLType.App(f, List.map (fun t0 -> term_apply t0 s) tl) // from TreeTerm.fs
    | MLType.ConcretizationEvidence(t1, s1) -> // from ExplicitSubstitutionTerm.fs
      MLType.ConcretizationEvidence(t1, composeWith s s1)
    | MLType.SubstrateQueryTerm(t0) -> 
      t0.Apply (TranslationfromML.ISubstitutionOfMLsubstitution s) |>
      TranslationtoML.MLtermOfITerm
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.Apply (TranslationfromML.ISubstitutionOfMLsubstitution s) |>
      TranslationtoML.MLtermOfITerm

  let rec innerTerm (ft: MLType.term) = // from ForallTerm.fs
    match ft with 
    | MLType.Forall(v, t) -> innerTerm t
    | _ -> ft

  let instantiate (ft: MLType.term) (s: MLType.substitution) = // from ForallTerm.fs
    let remainingVars = new HashSet<_>(boundVars ft) // TODO HashSet
    remainingVars.ExceptWith (Subst.domain s) // TODO HashSet
    let innerSubst = term_apply (innerTerm ft) s
    List.fold (fun t v -> MLType.Forall(v, t)) 
      innerSubst (Seq.toList remainingVars) // TODO HashSet

  let changeVarName (ft: MLType.term) (s:MLType.substitution) = // from ForallTerm.fs
    match ft with
    | MLType.Forall(v, t) ->
      let v', s' = 
        freshVar
          v 
          ((List.foldBack 
              (fun v acc -> (vars (Subst.subst_apply s v)) @ acc)
              (Subst.domain s) [])
            @ (Subst.domain s) @ (vars t))
      MLType.Forall(v, term_apply t s'), s'
    | _ -> failwith "changeVarName can only be called on a ForallTerm"

  let rec unifyFrom (t1: MLType.term) (s: MLType.substitution) (t2: MLType.term) : MLType.substitution option =
    match t1 with
    | MLType.Var(v1) -> // from Variable.fs
      match t2 with
      | _ when term_apply t1 s = term_apply t2 s -> Some s
      | _ when not(List.exists (fun v' -> v1 = v') (vars (term_apply t2 s))) -> 
        if Subst.domainContains s v1 then
          unifyFrom (term_apply t1 s) s t2
        else
          Some <| composeWith (Subst.extend Subst.id (v1, term_apply t2 s)) s
      | _ -> None
    | MLType.Const(c1) -> // from Constants.fs
      match t2 with
      | _ when t1 = term_apply t2 s -> Some s
      | MLType.Var(v2) -> unifyFrom t2 s t1
      | _ -> None
    | MLType.Forall(v1, t1') -> // from ForallTerm.fs
      match t2 with
      | MLType.Forall(v2, t2') -> 
        match unifyFrom (innerTerm t1) s (innerTerm t2) with
        | Some s -> 
          if List.forall 
                (fun v -> 
                  if List.exists (fun v' -> v' = v) (boundVars t1) then
                    match Subst.subst_apply s v with
                    | MLType.Var(v0) -> List.exists (fun v' -> v' = v0) (boundVars t2)
                    | _ -> false
                  elif List.exists (fun v' -> v' = v) (boundVars t2) then
                    match Subst.subst_apply s v with
                    | MLType.Var(v0) -> List.exists (fun v' -> v' = v0) (boundVars t1)
                    | _ -> false
                  else 
                    true) 
                (Subst.domain s) then
            Some s
          else 
            None
        | _ -> None
      | _ -> 
        unifyFrom t1' s t2
    | MLType.App(f1, tlist1) ->
      match t2 with
      | MLType.Var(v) -> unifyFrom t2 s t1
      | MLType.App(f2, tlist2)
        when f1 = f2
          && List.length tlist1 = List.length tlist2 -> 
        let mutable okSoFar = true
        let mutable ret = s
        let mutable i = 0
        while okSoFar && i < List.length tlist1 do
          match unifyFrom (term_apply tlist1.[i] ret) ret (term_apply tlist2.[i] ret) with
          | Some s ->
            ret <- s
          | None -> 
            okSoFar <- false
          i <- i + 1
        if okSoFar then
          Some ret
        else
          None
      | _ -> None
    | MLType.ConcretizationEvidence(et1, s1) ->
      match t2 with
      | MLType.ConcretizationEvidence(et2, s2) -> 
        unifyFrom (term_apply et1 s1) s2 (term_apply et2 s2)
      | _ -> unifyFrom t2 s t1
    | MLType.SubstrateQueryTerm(t0) ->
      t0.UnifyFrom (TranslationfromML.ISubstitutionOfMLsubstitution s) (TranslationfromML.ITermOfMLterm t2) |>
      Option.map TranslationtoML.MLsubstitutionOfISubstitution
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.UnifyFrom (TranslationfromML.ISubstitutionOfMLsubstitution s) (TranslationfromML.ITermOfMLterm t2) |>
      Option.map TranslationtoML.MLsubstitutionOfISubstitution

  let unify (t1: MLType.term) (t2: MLType.term) : MLType.substitution option =
    unifyFrom t1 (Subst.id) t2

