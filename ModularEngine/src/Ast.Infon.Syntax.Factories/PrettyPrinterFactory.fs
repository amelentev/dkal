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

namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Typed

/// The PrettyPrinterFactory provides a factory to construct different pretty
/// printers. 
type PrettyPrinterFactory() =

  /// Constructs an InfonPrinter. A pretty printer kind must be provided
  static member InfonPrinter (kind: string) = 
    match kind with
    | "simple" -> SimplePrettyPrinter() :> IInfonPrettyPrinter
    | "typed" -> TypedPrettyPrinter() :> IInfonPrettyPrinter
    | k -> failwith <| "Unrecognized pretty printer kind: " + k
