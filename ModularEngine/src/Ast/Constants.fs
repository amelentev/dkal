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

/// Constants are implicitly typed (they have the type of the wrapped
/// element)
type Constant(value: obj) = 
  interface IConst with
    member c.Vars = []
    member c.BoundVars = []
    member c.Type = Type.Substrate(value.GetType()) :> IType
    member c.Apply s = c :> ITerm
    member c.Normalize () = c :> ITerm
    member c.UnifyFrom s t =
      match (c :> ITerm), t with
      | c1, c2 when c1 = c2.Apply(s) -> Some s
      | c1, (:? IVar as v2) -> (v2 :> ITerm).UnifyFrom s c1
      | _ -> None
    member c.Unify t = 
      (c :> ITerm).UnifyFrom (Substitution.Id) t
    member c.Value = value
  member c.Value = (c :> IConst).Value
  override c.Equals (o: obj) =
    match o with
    | :? Constant as c' -> c.Value.Equals c'.Value
                                 && (c :> ITerm).Type.Equals (c' :> ITerm).Type
    | _ -> false
  override c.GetHashCode() = ((c :> ITerm).Type, c.Value).GetHashCode()
  override c.ToString() = sprintf "%A" value

/// Principal constants
type PrincipalConstant(name: string) =
  inherit Constant(name)
  interface IConst with
    override pc.Type = Type.Principal
  member pc.Name = name
  override pc.ToString() = name

type Collection(elems: IConst list) =
  inherit Constant(elems)
  member c.elems = elems
  interface IConst with
    override c.Type = 
      (if elems.IsEmpty then Type.CollectionType(Type.Nothing())
      else Type.CollectionType(elems.Head.Type)) :> IType
  override c.Equals (o: obj) =
    match o with
    | :? Collection as c' -> elems.Equals(c'.elems)
    | _ -> false
  override c.GetHashCode() = elems.GetHashCode()
  override c.ToString() = "[" + (elems |> List.map (fun e -> e.ToString()) |> String.concat "; ") + "]"