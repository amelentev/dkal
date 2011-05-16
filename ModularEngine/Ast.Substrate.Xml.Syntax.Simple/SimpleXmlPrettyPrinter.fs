namespace Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic

/// The SimpleXmlPrettyPrinter prints substrate elements into the simple concrete syntax
type SimpleXmlPrettyPrinter() =
    
  interface ISubstratePrettyPrinter with
    member spp.PrintTerm t =
      match t with
      | :? XmlSubstrateQueryTerm as t ->
        PrettyPrinter.PrettyPrint <| spp.TokenizeTerm t
      | _ -> failwith "Expecting DummySubstrateTerm when printing SimpleSqlSyntax"

  member private spp.TokenizeTerm (t: XmlSubstrateQueryTerm) =
    let outputVars = String.concat ", " 
                      <| Seq.map (fun (kv: KeyValuePair<string,ITerm>) -> 
                                    match kv.Key with
                                    | "" -> kv.Value.ToString()
                                    | att -> kv.Value.ToString() + " <-> \"" + att + "\"") t.Output
    [ TextToken <| "\"" + t.XPath + "\" | " + outputVars ]
