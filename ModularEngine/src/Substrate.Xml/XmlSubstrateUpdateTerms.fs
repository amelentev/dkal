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
namespace Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

/// base for XmlSubstrateUpdate terms
[<AbstractClass>]
type AXmlSubstrateUpdateTerm(ns: string, xpath: string, vars: IVar list) =
  member x.XPath = xpath
  interface ISubstrateUpdateTerm with
    member x.Namespace = ns
  interface ITerm with
    member x.Type = Type.SubstrateUpdate
    member x.Unify t = (x:>ITerm).UnifyFrom Substitution.Id t
    member x.Vars = vars
    member x.Apply subst = x :> ITerm
    member x.BoundVars = []
    member x.Normalize() = x :> ITerm
    member x.UnifyFrom s t = None
  override x.Equals y =
    match y with
    | :? AXmlSubstrateUpdateTerm as y ->
      ns = (y :> ISubstrateUpdateTerm).Namespace
        && xpath = y.XPath
        && HashSet(vars).SetEquals((y:>ITerm).Vars)
    | _ -> false
  override x.GetHashCode() =
    (ns, xpath, vars).GetHashCode()

/// Modify xml nodes
///  for each (attr,value) in attributes dict
///   if attr<>"" & value<>None then modify attribute attr to value
///   if attr<>"" & value=None then remove attribute attr from xml node
///   if attr="" & value<>None then rename node to value
///   if attr="" & value=None then remove node
type XmlSubstrateModifyTerm(ns: string, xpath: string, vars: IVar list,  attrs: Map<string, ITerm option>) =
  inherit AXmlSubstrateUpdateTerm(ns, xpath, vars)
  member x.Attrs = attrs
  interface ITerm with
    override x.Vars =
      let varsInAttrs = attrs |> Map.toList |> Seq.map snd |> Seq.choose (fun x -> x) |> Seq.collect (fun x -> x.Vars)
      HashSet(Seq.concat [seq vars; varsInAttrs]) |> Seq.toList
    override x.Apply subst =
      let xpath, vars = XSUtil.xpathApply xpath vars subst
      XmlSubstrateModifyTerm(ns, xpath, vars,
        (attrs |> Map.map (fun k -> Option.map (XSUtil.termApply x subst))) ) :> ITerm
  override x.Equals y =
    match y with
    | :? XmlSubstrateModifyTerm as y -> base.Equals y && attrs = y.Attrs
    | _ -> false
  override x.GetHashCode() = (base.GetHashCode(), attrs).GetHashCode()
  override x.ToString() =
    if (attrs |> Map.exists (fun attr term -> attr="" && term=None)) then
      sprintf "{| \"%s\" | delete \"%s\" |}" ns xpath
    else
      sprintf "{| \"%s\" | update \"%s\" | %s |}" ns xpath (XSUtil.mappingToStringO attrs)

/// Insert xml node with attributes.
///   NodeName = attrs.[""]
type XmlSubstrateInsertTerm(ns: string,  xpath: string, vars: IVar list, attrs: Map<string, ITerm>) =
  inherit AXmlSubstrateUpdateTerm(ns, xpath, vars)
  member x.NodeName = attrs.[""]
  member x.Attrs = attrs
  interface ITerm with
    override x.Vars =
      let varsInAttrs = attrs |> Map.toSeq |> Seq.map snd |> Seq.collect (fun x -> x.Vars)
      HashSet(Seq.concat [seq vars; varsInAttrs]) |> Seq.toList
    override x.Apply subst =
      let xpath, vars = XSUtil.xpathApply xpath vars subst
      XmlSubstrateInsertTerm(ns, xpath, vars,
        (attrs |> Map.map (fun k v -> v.Apply subst)) ) :> ITerm
  override x.Equals y = 
    match y with
    | :? XmlSubstrateInsertTerm as y ->
      base.Equals y && attrs = y.Attrs
    | _ -> false
  override x.GetHashCode() = (base.GetHashCode(), attrs).GetHashCode()
  override x.ToString() =
    sprintf "{| \"%s\" | insert \"%s\" | %s |}" ns xpath (XSUtil.mappingToString attrs)