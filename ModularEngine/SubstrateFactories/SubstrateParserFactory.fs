namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SqlSubstrate
open Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax
open Microsoft.Research.Dkal.Substrate.TypedSqlSyntax

open System.Collections.Generic

type SubstrateParserFactory() =
  
  static member SubstrateParser (s: ISubstrate) (kind: string) (ns: string) (types: Dictionary<string, IType>) = 
    match s with
    | :? SqlSubstrate as s ->
      match kind with
      | "simple" -> new SimpleSqlParser(s, ns, types) :> ISubstrateParser
      | "typed" -> new TypedSqlParser(ns) :> ISubstrateParser
      | _ -> failwithf "Unknown SQL substrate syntax kind %O" kind
    | _ -> failwith "Error while creating a substrate parser: unknown substrate type"
