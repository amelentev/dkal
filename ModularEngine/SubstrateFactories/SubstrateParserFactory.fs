namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SqlSubstrate
open Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax

open System.Collections.Generic

type SubstrateParserFactory() =
  
  static member SubstrateParser (s: ISubstrate) (kind: string) (ns: string) (types: Dictionary<string, IType>) = 
    match s with
    | :? SqlSubstrate ->
      match kind with
      | "simple" -> new SimpleSqlParser(ns, types) :> ISubstrateParser
      | _ -> failwithf "Unknown SQL substrate syntax kind %O" kind
    | _ -> failwith "Error while creating a substrate parser: unknown substrate type"
