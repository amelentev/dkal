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

type XmlSubstrate(xmldoc: XDocument, namespaces: string list) = 
  let log = LogManager.GetLogger("Substrate.Xml")

  let xn s = XName.Get s

  let bind (subst: ISubstitution option) (var: IVar) value =
    match subst with
    | None -> None
    | Some subst ->
      let sc : ITerm = 
        if var.Type=Type.Int32 then
          Constant (System.Int32.Parse(value)) :> ITerm
        else if var.Type=Type.Principal then
          PrincipalConstant value :> ITerm
        else
          Constant value :> ITerm
      if subst.Contains var && subst.Apply var <> sc then
        None
      else
        Some <| subst.Extend(var, sc)

  let getValue (elem: XElement) attr =
    match attr with
    | "" -> elem.Name.ToString()
    | _ -> elem.Attribute(xn attr).Value

  let bindXE (subst: ISubstitution) (vars: IDictionary<string, IVar>) (elem: XElement) =
    vars.Keys |> Seq.fold (fun subst attr ->
      bind subst vars.[attr] (getValue elem attr)) (Some subst)

  let solve11 (query: XmlSubstrateQueryTerm) (subst: ISubstitution) =
    let xpath = ((query :> ITerm).Apply subst :?> XmlSubstrateQueryTerm).XPath
    log.Debug("xpath: {0}", xpath)
    let res = xmldoc.Root.XPathEvaluate(xpath) :?> IEnumerable<obj>
    seq {
      for elem in res do
        match elem with
        | :? XElement as xe ->
          match bindXE subst query.OutputVars xe with
          | Some subst -> yield subst
          | None -> ()
        | :? XAttribute as xa when query.OutputVars.Count=1 ->
          let var = query.OutputVars.Values.First()
          match bind (Some subst) var xa.Value with
          | Some subst -> yield subst
          | None -> ()
        | _ ->
          failwithf "unknown xpath result: %A" elem
    }

  interface ISubstrate with
    member xs.RequiredVars st = 
      st.Vars

    member xs.Namespaces = new HashSet<_>(namespaces)

    member xs.Solve queries substs =
      let queries: XmlSubstrateQueryTerm seq = queries |> Seq.cast
      let solve1Many substs query =
        substs |> Seq.collect (solve11 query)
      queries |> Seq.fold solve1Many substs

    member xs.Update _ = failwith "XML substrate does not support updates"

    member xs.AreConsistentUpdates _ = failwith "XML substrate does not support updates"

  new(xmlFile: string, namespaces: string list) = new XmlSubstrate(XDocument.Load(xmlFile), namespaces)