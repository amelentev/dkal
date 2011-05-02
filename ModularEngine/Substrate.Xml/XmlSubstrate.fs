namespace Microsoft.Research.Dkal.Substrate.Xml

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Interfaces

type XmlSubstrate(xmldoc: XDocument, xsdFile: string, namespaces: string list) = 
  
  let quote s =
    s // XXX: full quote

  let quoteConstant = function
    | SubstrateConstant(s) -> quote( s.ToString() )
    | _ as s -> quote( s.ToString() )

  let xn s = XName.Get s

  let bind (subst: ISubstitution) (var: IVar) value =
    let sc : Constant = 
      if var.Type=Type.Int32 then
        SubstrateConstant (System.Int32.Parse(value)) :> Constant
      else if var.Type=Type.Principal then
        PrincipalConstant value :> Constant
      else
        SubstrateConstant value :> Constant
    subst.Extend(var, sc)

  let solve11 (query: XmlSubstrateTerm) (subst: ISubstitution) =
    let xpath = query.Vars |> List.fold (fun (s:string) (x:IVar) -> 
      let value = (subst.Apply x)
      s.Replace("$"+x.Name, quoteConstant value)) query.XPath
    printfn "xpath: %A" xpath
    let res = xmldoc.Root.XPathEvaluate(xpath) :?> IEnumerable<obj>
    seq {
      for a in res do
        match a with
        | :? XElement as xe ->
          let res = bind subst query.OutputVars.Head (xe.Name.ToString())
          yield query.OutputVars.Tail |>
            List.fold (fun acc var ->
              let value = xe.Attribute(xn var.Name).Value
              bind acc var value) res
        | :? XAttribute as xa ->
          yield bind subst query.OutputVars.Head xa.Value
        | _ ->
          failwithf "unknown xpath result: %A" a
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

  new(xmlFile: string, xdsFile: string, namespaces: string list) = XmlSubstrate(XDocument.Load(xmlFile), xdsFile, namespaces)