namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SqlSubstrate
open Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax

open System.Collections.Generic

type SubstratePrettyPrinterFactory() =
  
  static member SubstratePrettyPrinter (s: ISubstrate) (kind: string) =  
    match s with
    | :? SqlSubstrate ->
      match kind with
      | "simple" -> new SimpleSqlPrettyPrinter() :> ISubstratePrettyPrinter
      | _ -> failwithf "Unknown SQL substrate syntax kind %O" kind
    | _ -> failwith "Error while creating a substrate pretty printer: unknown substrate type"
