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

namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Typed

open System.IO
open System.Collections.Generic
open Microsoft.Research.Dkal.Ast.Syntax.Parsing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

/// The TypedParser parses the typed concrete syntax, which carries type 
/// annotations in every function application and every variable
type TypedSqlParser() = 

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      ()
      
    member sp.SetNamespace (ns: string) = 
      Parser.ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      ()

    member sp.ParseTerm s =
      GeneralParser.TryParse (Parser.ISubstrateTerm Lexer.tokenize) s
      