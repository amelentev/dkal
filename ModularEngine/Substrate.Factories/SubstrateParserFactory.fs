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

/// The SubstrateParserFactory is used to construct parsers for different 
/// types of substrates. In order to have dependency injection, parsers are
/// registered at run-time (see Factories.Initializer project)
type SubstrateParserFactory() =
  
  /// For each substrate type and syntax kind it contains the type of the 
  /// substrate parser that needs to be used
  static let parsers = new Dictionary<System.Type * string, System.Type>()

  /// Given a substrate type (type implementing ISubstrate), a syntax kind
  /// (e.g., "simple", "typed") and a parser type (type implementing ISubstrateParser)
  /// it stores this information to be used when a request to construct a 
  /// parser arrives
  static member RegisterParser (substrateType: System.Type) (kind: string) (parserType: System.Type) =
    parsers.[(substrateType, kind)] <- parserType

  /// Given a substrate, a syntax kind and a parsing context it attempts to
  /// find a registered substrate parser that matches. If successful, it 
  /// returns such a ISubstrateParser implementation, on which the substrate,
  /// the namespace and the parsing context have been set using the appropiate
  /// setters
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
