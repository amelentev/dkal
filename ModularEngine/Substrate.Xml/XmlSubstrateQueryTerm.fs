namespace Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open System.Text

/// xpath - xpath expression with (optional) input variables encoded as "$VARNAME"
/// vars - input variables
/// outputVars - map from result nodes attribute names (or node name in case of "") to output variable
type XmlSubstrateQueryTerm(ns: string, xpath: string, vars: IVar list, outputVars: IDictionary<string, IVar>) =

  member x.XPath = xpath
  member x.Vars = vars
  member x.OutputVars = outputVars

  override x.Equals (o: obj) =
    match o with
    | :? XmlSubstrateQueryTerm as x' ->
      x.XPath.Equals(x'.XPath) && x.Vars.Equals(x'.Vars) && x.OutputVars.Equals(x'.OutputVars)
    | _ -> false

  override x.GetHashCode() =
    (x.XPath, x.Vars, x.OutputVars).GetHashCode()

  override x.ToString() =
    let sout = outputVars.Keys |> Seq.map (function
        | "" -> outputVars.[""].Name
        | attr -> outputVars.[attr].Name + "<->\"" + attr+"\"") |> String.concat ", "
    "{| \""+ns+"\" | " + xpath + " | " + sout + " |}"

  interface ISubstrateQueryTerm with
    
    member x.Type = Type.SubstrateQuery
    member x.Vars = vars
    member x.Apply subst =
      let quote s =
        s // XXX: full quote
      let quoteConstant = function
        | SubstrateConstant(s) -> quote( s.ToString() )
        | _ as s -> quote( s.ToString() )
      let xpath = x.Vars |> List.fold (fun (s:string) (v:IVar) -> 
        let value = (subst.Apply v)
        s.Replace("$"+v.Name, quoteConstant value)) x.XPath
      let vars = 
        new HashSet<_>(
          List.collect (fun (v:IVar) -> if subst.Contains v then subst.Apply(v).Vars else [v] ) x.Vars) |> Seq.toList
      new XmlSubstrateQueryTerm(ns, xpath, vars, outputVars) :> ITerm
    member x.Normalize() = x :> ITerm
    member x.UnifyFrom _ _ = None
    member x.Unify _ = None

    member x.Namespace = ns