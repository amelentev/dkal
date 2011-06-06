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

namespace Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Typed

open System.IO

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Xml

open System.Collections.Generic

/// The TypedParser parses from the typed concrete syntax, which uses explicit
/// typed variables
type TypedXmlParser() = 

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      ()

    member sp.SetNamespace (ns: string) = 
      Parser.Namespace <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      ()

    member sp.ParseTerm s = 
      GeneralParser.TryParse (Parser.XmlSubstrateTerm Lexer.tokenize) s :> ISubstrateTerm

