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

/// The SubstratePrettyPrinterFactory is used to construct pretty printers
/// for different types of substrates. In order to have dependency injection, 
/// pretty printers are registered at run-time (see Factories.Initializer project)
type SubstratePrettyPrinterFactory() =
  
  /// For each substrate type and syntax kind it contains the type of the 
  /// substrate pretty printer that needs to be used
  static let printers = new Dictionary<System.Type*string, System.Type>()

  /// Given a substrate type (type implementing ISubstrate), a syntax kind
  /// (e.g., "simple", "typed") and a pretty printer type (type implementing 
  /// ISubstratePrettyPrinter) it stores this information to be used when a 
  /// request to construct a pretty printer arrives
  static member RegisterPrettyPrinter (substrateType: System.Type) (kind: string) (printerType: System.Type) =
    printers.[(substrateType, kind)] <- printerType

  /// Given a substrate and a syntax kind it attempts to find a registered 
  /// substrate pretty printer that matches. If successful, it returns such a 
  /// ISubstratePrettyPrinter implementation
  static member SubstratePrettyPrinter (s: ISubstrate) (kind: string) =  
    if printers.ContainsKey (s.GetType(), kind) then
      let spt = printers.[(s.GetType(), kind)]
      let sp = spt.GetConstructor([||]).Invoke([||]) :?> ISubstratePrettyPrinter
      sp
    else
      failwithf "Error while creating a substrate pretty printer: unknown substrate type/kind combination %O %O" (s.GetType()) kind
