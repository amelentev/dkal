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

type SubstratePrettyPrinterFactory() =
  
  static let printers = new Dictionary<System.Type*string, System.Type>()

  static member RegisterPrettyPrinter (substrateType: System.Type) (kind: string) (printerType: System.Type) =
    printers.[(substrateType, kind)] <- printerType

  static member SubstratePrettyPrinter (s: ISubstrate) (kind: string) =  
    if printers.ContainsKey (s.GetType(), kind) then
      let spt = printers.[(s.GetType(), kind)]
      let sp = spt.GetConstructor([||]).Invoke([||]) :?> ISubstratePrettyPrinter
      sp
    else
      failwithf "Error while creating a substrate pretty printer: unknown substrate type/kind combination %O %O" (s.GetType()) kind
