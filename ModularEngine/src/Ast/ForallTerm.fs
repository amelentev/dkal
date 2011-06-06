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

namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

type ForallTerm =
  { Var: IVar; Term: ITerm }

  member ft.InnerTerm = 
    match ft.Term with :? ForallTerm as ft' -> ft'.InnerTerm | _ -> ft.Term

  member ft.Instantiate (s: ISubstitution) = 
    let remainingVars = new HashSet<_>((ft :> ITerm).BoundVars)
    remainingVars.ExceptWith s.Domain
    let innerSubst = ft.InnerTerm.Apply s
    List.fold (fun t v -> {Var = v; Term = t} :> ITerm) innerSubst (Seq.toList remainingVars)
  
  member ft.ChangeVarName (s: ISubstitution) =
    let v, s' = ForallTerm.FreshVar ft.Var ([for v in s.Domain do yield! (s.Apply v).Vars] @ s.Domain @ ft.Term.Vars)
    { Var = v; Term = ft.Term.Apply s' } :> ITerm, s'

//    
//    if s.DomainContains ft.Var then
//      ft.Term.Apply s
//    else
//      match ft.InnerTerm with
//      | :? ForallTerm as ft' -> 
//        { Var = ft.Var; Term = ft'.Instantiate s } :> ITerm
//      | _ ->
//        { Var = ft.Var; Term = ft.Term.Apply s } :> ITerm

  interface ITerm with
    member fit.BoundVars = new HashSet<_>(fit.Var :: fit.Term.BoundVars) |> Seq.toList

    member fit.Vars =
      let termVars = new HashSet<_>(fit.Term.Vars)
      termVars.Remove(fit.Var) |> ignore
      termVars |> Seq.toList
    member fit.Type = fit.Term.Type
    member fit.Apply s = 
      // the substitution is not applied to the quantified variable
      let s = s.Forget [fit.Var]
      // check that there will be no variable capture
      let varsToCheck = new HashSet<_>(fit.Term.Vars)
      varsToCheck.IntersectWith s.Domain
      let mappedVars = Seq.collect (fun (v': IVar) -> s.Apply(v').Vars) varsToCheck
      if Seq.exists (fun v -> v = fit.Var) mappedVars then
        let newVar, newVarSubst = ForallTerm.FreshVar fit.Var (fit.Term.Vars @ (mappedVars |> Seq.toList))
        { Var = newVar; Term = (fit.Term.Apply newVarSubst).Apply(s) } :> ITerm
      else
        { Var = fit.Var; Term = fit.Term.Apply s } :> ITerm
    member fit.Normalize () = { Var = fit.Var; Term = fit.Term.Normalize() } :> ITerm
    member fit.UnifyFrom s t = 
      match t with
      | :? ForallTerm as fit' -> 
        match fit.InnerTerm.UnifyFrom s fit'.InnerTerm with
        | Some s -> 
          if List.forall 
                (fun v -> 
                  if List.exists (fun v' -> v' = v) (fit :> ITerm).BoundVars then
                    match s.Apply v with
                    | :? IVar as v -> List.exists (fun v' -> v' = v) (fit' :> ITerm).BoundVars
                    | _ -> false
                  elif List.exists (fun v' -> v' = v) (fit' :> ITerm).BoundVars then
                    match s.Apply v with
                    | :? IVar as v -> List.exists (fun v' -> v' = v) (fit :> ITerm).BoundVars
                    | _ -> false
                  else 
                    true) 
                s.Domain then
            Some s
          else 
            None
        | _ -> None
      | _ -> 
        fit.Term.UnifyFrom s t
    member fit.Unify t = 
      (fit :> ITerm).UnifyFrom (Substitution.Id) t

  static member private FreshVar (var: IVar) (otherVars: IVar list) =
    let prefix = "FreshVar#"
    let freshVars = List.filter (fun (v: IVar) -> v.Name.StartsWith(prefix)) (var::otherVars)
    let freshNumbers = List.map (fun (v: IVar) -> System.Int32.Parse(v.Name.Substring(prefix.Length))) freshVars
    let freshVarId = if freshNumbers.IsEmpty then 0 else (List.max freshNumbers) + 1
    let freshVar = { Name = prefix + freshVarId.ToString(); Type = var.Type } :> IVar
    let subst = Substitution.Id.Extend (var, freshVar)
    freshVar, subst

  override fit.ToString() = 
    let vars = String.concat ", " [for v in (fit :> ITerm).BoundVars -> v.Name + ": " + v.Type.FullName]
    "with " + vars + " " + fit.InnerTerm.ToString()
    
