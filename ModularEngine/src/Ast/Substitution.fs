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

/// Implementation of the ISubstitution interface using a Dictionary
type Substitution(subst : Dictionary<IVar, ITerm>) = 
    
  /// Returns a new Substitution that behaves like the identity
  static member Id = 
    new Substitution(new Dictionary<_, _>()) :> ISubstitution

  interface ISubstitution with
      
    /// Returns a new Substitution that results from extending the current 
    /// Substitution so that it maps v to t and leaves the rest unchanged
    member s.Extend (v: IVar, t: ITerm) = 
      if t = (v :> ITerm) then 
        s :> ISubstitution
      else
        let newSubst = new Dictionary<_, _>(subst)
        newSubst.[v] <- t
        new Substitution(newSubst) :> ISubstitution
       
    /// Applies this Substitution to the given IVar
    member s.Apply (v: IVar) = 
      match subst.TryGetValue v with
      | true, (:? IVar as ret) -> (s:>ISubstitution).Apply(ret) // var -> another var
      | true, (:? IConst as ret) -> match ret.Value with       // TODO take this out, shouldn't happen
                                    | :? IConst as value -> value :> ITerm
                                    | _ -> ret :> ITerm
      | true, ret -> ret
      | _ -> v :> ITerm

    /// Returns a new Substitution that results from first applying s' and 
    /// then applying the current Substitution
    member s.ComposeWith (s': ISubstitution) =
      let newSubst = new Dictionary<_, _>(subst)
      for v in s'.Domain do
        newSubst.[v] <- v.Apply(s').Apply(s)
      for v in (s :> ISubstitution).Domain do
        if not <| s'.DomainContains v then
          newSubst.[v] <- v.Apply(s)
      new Substitution(newSubst) :> ISubstitution

    /// Returns true iff v is affected by this Substitution
    member s.DomainContains (v: IVar) =
      subst.ContainsKey v

    /// Returns the vars affected by this Substitution
    member s.Domain =
      [ for kvp in subst -> kvp.Key ]

    /// Returns a new Substitution that results from restricting the current 
    /// one to only modify the variables given in the list
    member s.RestrictTo (vars: IVar list) = 
      let newSubst = new Dictionary<_, _>(subst)
      for v in subst.Keys do
        if not <| List.exists (fun v' -> v' = v) vars then
          newSubst.Remove(v) |> ignore
      new Substitution(newSubst) :> ISubstitution

    /// Returns a new Substitution that results from forgetting the current
    /// mapping to the variables given in the list
    member s.Forget (vars: IVar list) = 
      let relevantVars = new HashSet<_>((s :> ISubstitution).Domain)
      relevantVars.ExceptWith vars
      (s :> ISubstitution).RestrictTo (relevantVars |> Seq.toList)

    /// Returns true if this Substitution only renames variables
    member s.IsVariableRenaming = 
      List.forall 
        (fun (t: ITerm) -> match t with :? IVar -> true | _ -> false) 
        [for t in subst.Values -> t]

  override s.Equals (s': obj) =
    let s = (s :> ISubstitution)
    match s' with
    | :? ISubstitution as s' -> 
      s.Domain.Equals(s'.Domain) &&
        List.forall (fun v -> s.Apply(v).Equals(s'.Apply(v))) s.Domain
    | _ -> false

  override s.GetHashCode() =
    let s = (s :> ISubstitution)
    [ for v in s.Domain -> (v, s.Apply(v)) ].GetHashCode()

  override s.ToString() = 
    let s = (s :> ISubstitution)
    "{" + (String.concat ", " [for v in s.Domain -> v.ToString() + " -> " + s.Apply(v).ToString()]) + "}"
