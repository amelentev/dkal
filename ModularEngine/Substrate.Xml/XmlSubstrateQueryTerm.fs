namespace Microsoft.Research.Dkal.Substrate.Xml

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic
open System.Text

/// xpath - xpath expression with (optional) input variables encoded as "$VARNAME"
/// vars - input variables
/// output - map from result nodes attribute names (or node name in case of "") to output term 
/// (if it is a variable it gets instantiated)
type XmlSubstrateQueryTerm(ns: string, xpath: string, vars: IVar list, output: IDictionary<string, ITerm>) =

  member x.XPath = xpath
  member x.Vars = vars
  member x.Output = output

  override x.Equals (o: obj) =
    match o with
    | :? XmlSubstrateQueryTerm as x' ->
      let equalVars = (new HashSet<_>(x.Vars)).SetEquals(new HashSet<_>(x'.Vars))
      let equalOutput = (new HashSet<_>([for kvp in x.Output -> (kvp.Key, kvp.Value)]))
                           .SetEquals(new HashSet<_>([for kvp in x'.Output -> (kvp.Key, kvp.Value)]))
      x.XPath.Equals(x'.XPath) && equalVars && equalOutput
    | _ -> false

  override x.GetHashCode() =
    let vars = new HashSet<_>(vars) |> Seq.toList
    let output = [for kvp in x.Output -> (kvp.Key, kvp.Value)]
    (x.XPath, vars, output).GetHashCode()

  override x.ToString() =
    let sout = output.Keys |> Seq.map (function
        | "" -> output.[""].ToString()
        | attr -> output.[attr].ToString() + "<->\"" + attr+"\"") |> String.concat ", "
    "{| \""+ns+"\" | " + xpath + " | " + sout + " |}"

  interface ISubstrateQueryTerm with
    
    member x.Type = Type.SubstrateQuery
    member x.Vars = new HashSet<_>(vars @ [for t in output.Values do yield! t.Vars]) |> Seq.toList
    member x.BoundVars = []
    member x.Apply subst =
      let quote (t: ITerm) = 
        match t with
        | :? IConst as c -> c.ToString()
        | :? IVar as v -> "$" + v.Name
        | _ -> failwithf "Can't apply substitution %O in XML query term, it would yield a non-atomic term %O" subst t
      let xpath = x.Vars |> List.fold (fun (s:string) (v:IVar) -> 
        let value = (subst.Apply v)
        s.Replace("$"+v.Name, quote value)) x.XPath
      let vars = 
        new HashSet<_>(
          List.collect (fun (v:IVar) -> if subst.DomainContains v then subst.Apply(v).Vars else [v] ) x.Vars) |> Seq.toList
      let newOutput = new Dictionary<_, _>()
      for kvp in output do
        newOutput.[kvp.Key] <- match kvp.Value.Apply(subst) with
                               | :? IVar as v -> v :> ITerm
                               | :? IConst as c -> c :> ITerm
                               | o -> failwithf "Can't apply substitution %O to %O because it yields a non atomic output binding" subst x
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