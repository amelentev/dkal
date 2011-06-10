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

namespace Microsoft.Research.Dkal.Substrate.Basic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic

/// A BasicSubstrateTerm is a query term for the BasicSubstrate. It has a left
/// side term, which can only be a variable (used for output) or a constant 
/// (used for comparison), and a right side term which can be a complex 
/// expression.
type BasicSubstrateTerm(left: ITerm, right: ITerm) as bst =

  do 
    if left.Type <> right.Type then
      failwithf "Error while constructing basic substrate term. Left type %O does not match right type %O at %O" left.Type right.Type bst

  /// The left side of the BasicSubstrateTerm. It can contain a variable (for 
  /// output) or a constant (for comparison)
  member bst.Left = left

  /// The right side of the BasicSubstrateTerm. It can contain a complex 
  /// expression
  member bst.Right = right

  override bst.ToString() = 
    "{| \"" + (bst :> ISubstrateTerm).Namespace + "\" | " 
      + bst.Left.ToString() + " := " + bst.Right.ToString() + " |}"

  override bst.Equals(o: obj) = 
    match o with
    | :? BasicSubstrateTerm as bst' ->
      (bst.Left, bst.Right).Equals((bst'.Left, bst'.Right))
    | _ -> false

  override bst.GetHashCode() =
    (bst.Left, bst.Right).GetHashCode()

  interface ISubstrateQueryTerm with
    member bst.Namespace = BasicPrimitives.BasicNamespace

    member bst.Type = Type.SubstrateQuery

    member bst.Vars = new HashSet<_>(left.Vars @ right.Vars) |> Seq.toList

    member bst.BoundVars = []

    member bst.Apply (s: ISubstitution) =
      let left =  match left.Apply s with
                  | :? IVar as v -> v :> ITerm
                  | :? IConst as c -> c :> ITerm
                  | l -> failwithf "Can't apply substitution %O to %O because it yields non atomic left side" s bst
      new BasicSubstrateTerm(left, right.Apply s) :> ITerm

    member bst.Normalize () = 
      new BasicSubstrateTerm(left.Normalize(), right.Normalize()) :> ITerm

    member bst.UnifyFrom (s: ISubstitution) (t: ITerm) = 
      match t with 
      | :? BasicSubstrateTerm as bst' -> 
        match bst.Left.UnifyFrom s bst'.Left with
        | Some s' -> bst.Right.UnifyFrom s' bst.Right
        | _ -> None
      | _ -> None

    member bst.Unify (t: ITerm) = (bst :> ITerm).UnifyFrom Substitution.Id t

