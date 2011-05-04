namespace Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic

/// The TypedXmlPrettyPrinter prints substrate elements into the typed concrete syntax
type TypedXmlPrettyPrinter() =
    
  interface ISubstratePrettyPrinter with
    member spp.PrintTerm t =
      match t with
      | :? XmlSubstrateQueryTerm as t ->
        PrettyPrinter.PrettyPrint <| spp.TokenizeTerm t
      | _ -> failwith "Expecting DummySubstrateTerm when printing SimpleSqlSyntax"

  member private spp.PrintType (t: IType) = t.FullName

  member private spp.PrintVar (v: IVar) = v.Name + ":" + spp.PrintType v.Type

  member private spp.TokenizeTerm (t: XmlSubstrateQueryTerm) =
    let outputVars = String.concat "," 
                      <| Seq.map (fun (kv: KeyValuePair<string,IVar>) -> 
                                    match kv.Key with
                                    | "" -> spp.PrintVar kv.Value
                                    | att -> spp.PrintVar kv.Value + "<->\"" + att + "\"") t.OutputVars
    let vars = String.concat "," 
                <| List.map (fun (v: IVar) -> spp.PrintVar v) t.Vars
    [ TextToken <| "\"" + t.XPath + "\"|" + outputVars + "|" + vars ]