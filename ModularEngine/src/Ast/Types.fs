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

/// Defines the basic IType implementations for DKAL types
module Type = 

  type BasicType(fullName: string, name: string, ?baseType: IType) = 
    interface IType with
      member bt.FullName = fullName
      member bt.Name = name
      member bt.BaseType = baseType
      member bt.IsSubtypeOf t = 
        bt.Equals t || (match baseType with None -> false | Some bt -> bt.IsSubtypeOf t)

    override bt.GetHashCode() = (bt :> IType).FullName.GetHashCode()
    override bt.Equals (o: obj) = 
      match o with
      | :? BasicType as bt' -> (bt :> IType).FullName.Equals((bt' :> IType).FullName)
      | _ -> false
    override bt.ToString() = (bt :> IType).FullName

  /// Type for abstract infons (either justified or non justified)
  let AbstractInfon = new BasicType("Dkal.AbstractInfon", "AbstractInfon") :> IType

  /// Type for infons
  let Infon = new BasicType("Dkal.Infon", "Infon", AbstractInfon) :> IType

  /// Type for infons
  let JustifiedInfon = new BasicType("Dkal.JustifiedInfon", "JustifiedInfon", AbstractInfon) :> IType

  /// Type for principals
  let Principal = new BasicType("Dkal.Principal", "Principal") :> IType

  /// Type for substrate update terms
  let SubstrateUpdate = new BasicType("Dkal.SubstrateUpdate", "SubstrateUpdate") :> IType

  /// Type for substrate query terms
  let SubstrateQuery = new BasicType("Dkal.SubstrateQuery", "SubstrateQuery") :> IType

  /// Type for actions used in policy rules
  let Action = new BasicType("Dkal.Action", "Action") :> IType

  /// Type for conditions used in policy rules
  let Condition = new BasicType("Dkal.Condition", "Condition") :> IType

  /// Type for policy rules 
  let Rule = new BasicType("Dkal.Rule", "Rule") :> IType

  /// Type for evidence (a.k.a. justification, or proof)
  let Evidence = new BasicType("Dkal.Evidence", "Evidence") :> IType

  /// Defines an IType implementation to use .NET types as DKAL types
  type Substrate(typ: System.Type) = 
    interface IType
      with 
        member t.FullName = typ.FullName
        member t.Name = typ.Name
        member t.BaseType = None
        member t.IsSubtypeOf t' = t.Equals t'
    /// The .NET type wrapped by this Substrate type
    member s.Type = typ
    override s.Equals t' = match t' with
                           | :? Substrate as t' -> typ.Equals(t'.Type)
                           | _ -> false
    override s.GetHashCode() = typ.GetHashCode()
    override s.ToString() = (s :> IType).FullName

  // type shortcuts
  let Boolean = Substrate(typeof<bool>) :> IType
  let Int32 = Substrate(typeof<int32>) :> IType
  let Double = Substrate(typeof<double>) :> IType
  let String = Substrate(typeof<string>) :> IType

  /// Collection type
  type CollectionType(elemType: IType) =
    member x.elemType = elemType
    interface IType with
      member x.FullName = elemType.FullName + "[]"
      member x.Name = elemType.Name + "[]"
      member x.BaseType = None
      member x.IsSubtypeOf t = 
        match t with
        | :? CollectionType as ct -> elemType.IsSubtypeOf ct.elemType // Collections are covariant because immutability
        | _ -> false
    override x.Equals x' = 
      match x' with
      | :? CollectionType as x' -> elemType.Equals(x'.elemType)
      | _ -> false
    override x.GetHashCode() = (typeof<CollectionType>, elemType).GetHashCode()
    override x.ToString() = elemType.ToString() + "[]"

  /// Bottom type. There are no instances of Nothing and Nothing is subtype of every type.
  /// Empty collection has type  CollectionType(Nothing()).
  type Nothing() =
    inherit BasicType("Dkal.Nothing", "Nothing")
    interface IType with
      override t.IsSubtypeOf t' = true

  let FromFullName fn = 
    match fn with
    | "Dkal.Infon" -> Infon
    | "Dkal.JustifiedInfon" -> JustifiedInfon
    | "Dkal.Principal" -> Principal
    | "Dkal.SubstrateUpdate" -> SubstrateUpdate
    | "Dkal.SubstrateQuery" -> SubstrateQuery
    | "Dkal.Condition" -> Condition
    | "Dkal.Action" -> Action
    | "Dkal.Rule" -> Rule
    | "Dkal.Evidence" -> Evidence
    | fn -> 
      let t = System.Type.GetType(fn)
      if t <> null then
        Substrate(t) :> IType
      else
        failwithf "Unknown type: %O, check spelling and make sure to use fully qualified names (e.g., Dkal.Principal, System.Int32)" fn