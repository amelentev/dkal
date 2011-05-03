namespace Microsoft.Research.Dkal.Substrate.Xml

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open System.Linq
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type XmlSubstrate(xmldoc: XDocument, namespaces: string list) = 
  
  let quote s =
    s // XXX: full quote

  let quoteConstant = function
    | SubstrateConstant(s) -> quote( s.ToString() )
    | _ as s -> quote( s.ToString() )

  let xn s = XName.Get s

  let bind (subst: ISubstitution) (var: IVar) value =
    let sc : ITerm = 
      if var.Type=Type.Int32 then
        Constant (System.Int32.Parse(value)) :> ITerm
      else if var.Type=Type.Principal then
        PrincipalConstant value :> ITerm
      else
        Constant value :> ITerm
    subst.Extend(var, sc)

  let getValue (elem: XElement) attr =
    match attr with
    | "" -> elem.Name.ToString()
    | _ -> elem.Attribute(xn attr).Value

  let bindXE (subst: ISubstitution) (vars: IDictionary<string, IVar>) (elem: XElement) =
    vars.Keys |> Seq.fold (fun subst attr ->
      bind subst vars.[attr] (getValue elem attr)) subst

  let solve11 (query: XmlSubstrateTerm) (subst: ISubstitution) =
    let xpath = query.Vars |> List.fold (fun (s:string) (x:IVar) -> 
      let value = (subst.Apply x)
      s.Replace("$"+x.Name, quoteConstant value)) query.XPath
    printfn "xpath: %A" xpath
    let res = xmldoc.Root.XPathEvaluate(xpath) :?> IEnumerable<obj>
    seq {
      for elem in res do
        match elem with
        | :? XElement as xe ->
          yield bindXE subst query.OutputVars xe
        | :? XAttribute as xa when query.OutputVars.Count=1 ->
          let var = query.OutputVars.Values.First()
          yield bind subst var xa.Value
        | _ ->
          failwithf "unknown xpath result: %A" elem
    }

  interface ISubstrate with
    member xs.RequiredVars st = 
      st.Vars

    member xs.Namespaces = new HashSet<_>(namespaces)

    member xs.Solve queries substs =
      let queries: XmlSubstrateTerm seq = queries |> Seq.cast
      let solve1Many substs query =
        substs |> Seq.collect (solve11 query)
      queries |> Seq.fold solve1Many substs

  new(xmlFile: string, namespaces: string list) = new XmlSubstrate(XDocument.Load(xmlFile), namespaces)