// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

type SubstrateParserFactory() =
  
  static let parsers = new Dictionary<System.Type * string, System.Type>()

  static member RegisterParser (substrateType: System.Type) (kind: string) (parserType: System.Type) =
    parsers.[(substrateType, kind)] <- parserType

  static member SubstrateParser (s: ISubstrate) (kind: string) (ns: string) (context: IParsingContext option) = 
    if parsers.ContainsKey (s.GetType(), kind) then
      let spt = parsers.[(s.GetType(), kind)]
      let sp = spt.GetConstructor([||]).Invoke([||]) :?> ISubstrateParser
      sp.SetNamespace ns
      sp.SetSubstrate s
      match context with
      | Some context -> sp.SetParsingContext context
      | None -> ()
      sp
    else
      failwithf "Error while creating a substrate parser: unknown substrate type/kind combination %O %O" (s.GetType()) kind
