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

namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple

open System.IO

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with variable type information
type SimpleSqlParser() = 

  interface ISubstrateParser with

    member sp.SetParsingContext (context: IParsingContext) = 
      Parser.ctx <- Some context

    member sp.SetNamespace (ns: string) = 
      Parser.ns <- ns

    member sp.SetSubstrate (substrate: ISubstrate) =
      match substrate with
      | :? SqlSubstrate as substrate -> 
        Parser.substrate <- Some substrate
      | _ -> failwith "Expecting SqlSubstrate on SimpleSqlParser"

    member sp.ParseTerm s = 
      GeneralParser.TryParse (Parser.SubstrateTerm Lexer.tokenize) s 
      
      
