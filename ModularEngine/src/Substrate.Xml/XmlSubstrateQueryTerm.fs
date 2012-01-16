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
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic
open System.Text

module XSUtil =
  let setEquals (a : 'a seq) b = HashSet<'a>(a).SetEquals(b)

  let xpathApply xpath (vars:IVar seq) (subst:ISubstitution) =
    let quote (t:ITerm) = match t with
                          | :? IConst as c ->
                            match c.Type with
                            | Substrate(s) when s = typeof<string> -> c.Value.ToString()
                            | _ -> c.ToString()
                          | :? IVar as v -> "$" + v.Name
                          | _ -> failwithf "Can't apply substitution %O in XML query term, it would yield a non-atomic term %O" subst t
    let xpath = vars |> Seq.fold (fun (xpath:string) (v:IVar) -> 
                                     let value = (subst.Apply v)
                                     xpath.Replace("$"+v.Name, quote value)) xpath
    let vars = new HashSet<_>(vars |> Seq.collect (fun (v:IVar) -> subst.Apply(v).Vars)) |> Seq.toList
    (xpath, vars)

  let termApply this subst (t: ITerm) =
    match t.Apply subst with
    | :? IVar as v -> v :> ITerm
    | :? IConst as c -> c :> ITerm
    | o -> failwithf "Can't apply substitution %O to %O because it yields a non atomic output binding" subst this

  let mappingToStringO (map: Map<string, ITerm option>) =
    map |> Seq.map (fun kvp ->
                      match kvp.Value, kvp.Key with
                      | Some(t), ""   -> t.ToString()
                      | Some(t), attr -> t.ToString() + "<->\""+attr+"\""
                      | None, attr    -> "delete \""+attr+"\"")
        |> String.concat ", "
  let mappingToString (map: Map<string, ITerm>) = map |> Map.map (fun attr term -> Some term) |> mappingToStringO


/// An XPath expression used to query XmlSubstrates
type XmlSubstrateQueryTerm(ns: string, xpath: string, vars: IVar list, output: Map<string, ITerm>) =

  /// XPath expression with (optional) input variables encoded as "$VARNAME"
  member x.XPath = xpath

  /// Input variables used in the XPath expression
  member x.Vars = vars

  /// Map from result nodes attribute names (or node name in case of "") to 
  /// output ITerms. If the output term is a variable it gets instantiated; 
  /// if it is a constant it is compared
  member x.Output = output

  override x.Equals (o: obj) =
    match o with
    | :? XmlSubstrateQueryTerm as x' ->
      x.XPath = x'.XPath && XSUtil.setEquals x.Vars x'.Vars && x.Vars = x'.Vars
    | _ -> false

  override x.GetHashCode() =
    let vars = new HashSet<_>(vars) |> Seq.toList
    (xpath, vars, output).GetHashCode()

  override x.ToString() =
    sprintf "{| \"%s\" | \"%s\" | %s |}" ns xpath (XSUtil.mappingToString output)

  interface ISubstrateQueryTerm with
    member x.Type = Type.SubstrateQuery
    member x.Vars = vars
    member x.BoundVars = HashSet([for t in output do yield! t.Value.Vars]) |> Seq.toList
    member x.Apply subst =
      let xpath, vars = XSUtil.xpathApply xpath vars subst
      let newOutput = output |> Map.map (fun key -> XSUtil.termApply x subst)
      new XmlSubstrateQueryTerm(ns, xpath, vars, newOutput) :> ITerm
    member x.Normalize() = x :> ITerm
    member x.UnifyFrom s t = 
      match t with
      | Var(_) -> t.UnifyFrom s x
      | :? XmlSubstrateQueryTerm as x' -> 
        if x.Equals(x') then
          Some s
        else
          failwithf "Operation not supported (yet): attempt to unify XML substrate terms %O and %O" x x'
      | _ -> None

    member x.Unify t = (x :> ITerm).UnifyFrom Substitution.Id t

    member x.Namespace = ns