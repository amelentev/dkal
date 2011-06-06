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
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Typed

/// The ParserFactory provides a factory to construct different parsers. A 
/// parser kind must be provided.
type ParserFactory() =
  static member InfonParser (kind: string, me: string) = 
    match kind with
    | "simple" -> 
      let ret = SimpleParser() :> IInfonParser
      ret.SetParsingContext (new ParsingContext(me))
      ret
    | "typed" -> TypedParser() :> IInfonParser
    | k -> failwith <| "Unrecognized parser kind: " + k

