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

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open System.Linq
open NLog
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

/// An XML Substrate is an abstraction of an XML document that can be queried
/// by means of XPath expressions encoded in XmlSubstrateQueryTerms
type XmlSubstrate(xmldoc: XDocument, namespaces: string list) = 
  let log = LogManager.GetLogger("Substrate.Xml")

  let xn s = XName.Get s

  let bind (subst: ISubstitution option) (output: ITerm) value =
    match subst with
    | None -> None
    | Some subst ->
      let sc : ITerm = 
        if output.Type=Type.Int32 then
          Constant (System.Int32.Parse(value)) :> ITerm
        else if output.Type=Type.Principal then
          PrincipalConstant value :> ITerm
        else
          Constant value :> ITerm
      output.UnifyFrom subst sc

  let getValue (elem: XElement) attr =
    match attr with
    | "" -> elem.Name.ToString()
    | _ -> elem.Attribute(xn attr).Value

  let bindXE (subst: ISubstitution) (output: IDictionary<string, ITerm>) (elem: XElement) =
    output.Keys |> Seq.fold (fun subst attr ->
      bind subst output.[attr] (getValue elem attr)) (Some subst)

  let solve11 (query: XmlSubstrateQueryTerm) (subst: ISubstitution) =
    let xpath = ((query :> ITerm).Apply subst :?> XmlSubstrateQueryTerm).XPath
    log.Debug("xpath: {0} under {1}", xpath, subst)
    let res = xmldoc.Root.XPathEvaluate(xpath) :?> IEnumerable<obj>
    seq {
      for elem in res do
        match elem with
        | :? XElement as xe ->
          match bindXE subst query.Output xe with
          | Some subst -> yield subst
          | None -> ()
        | :? XAttribute as xa when query.Output.Count=1 ->
          let output = query.Output |> Map.pick (fun k v -> Some(v))
          match bind (Some subst) output xa.Value with
          | Some subst -> yield subst
          | None -> ()
        | _ ->
          failwithf "unknown xpath result: %A" elem
    }

  let getTermValue (t:ITerm) = 
    match t with
      | :? IConst as c -> c.Value.ToString()
      | _ as v -> failwithf "unknown attribute value %A" v
  
  member x.modify (term: XmlSubstrateModifyTerm) =
    let nodes = xmldoc.Root.XPathEvaluate(term.XPath) :?> IEnumerable<obj>
    let mutable found = false
    for elem in nodes do
      let xe = elem :?> XElement
      for kvp in term.Attrs do
        let v = match kvp.Value with
                | None -> null
                | Some(c) -> getTermValue c
        match kvp.Key, v with
        | "", null -> xe.Remove()
        | "", v -> xe.Name <- xn v
        | _ -> xe.SetAttributeValue(xn kvp.Key, v)
        found <- true
    found
  
  member x.insert (term: XmlSubstrateInsertTerm) =
    let nodes = xmldoc.Root.XPathEvaluate(term.XPath) :?> IEnumerable<obj>
    let mutable found = false
    for elem in nodes do
      let xe = elem :?> XElement
      let newnode = XElement(xn (getTermValue term.NodeName))
      for attr in (term.Attrs |> Seq.filter (fun a -> a.Key<>"")) do
        newnode.SetAttributeValue(xn attr.Key, getTermValue attr.Value)
      xe.Add(newnode)
      found <- true
    found

  interface ISubstrate with
    member xs.RequiredVars st = 
      st.Vars

    member xs.Namespaces = new HashSet<_>(namespaces)

    member xs.Solve queries substs =
      let queries: XmlSubstrateQueryTerm seq = queries |> Seq.cast
      let solve1Many substs query =
        substs |> Seq.collect (solve11 query)
      queries |> Seq.fold solve1Many substs

    /// xml updates are in-memory only. no files affected.
    member xs.Update terms =
      let update(term : ISubstrateUpdateTerm) =
        match term with
        | :? XmlSubstrateModifyTerm as t -> xs.modify t
        | :? XmlSubstrateInsertTerm as t -> xs.insert t
        | _ -> failwithf "unknown SubstrateUpdateTerm: %A" term
      terms |> Seq.map update |> List.ofSeq |> List.exists (fun x -> x)

    member xs.AreConsistentUpdates _ = failwith "XML substrate does not support updates"

  new(xmlFile: string, namespaces: string list) = new XmlSubstrate(XDocument.Load(xmlFile), namespaces)