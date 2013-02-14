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

namespace Microsoft.Research.Dkal.Factories.Initializer

open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Substrate.Factories
open Microsoft.Research.Dkal.Substrate.Sql
open Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Typed
open Microsoft.Research.Dkal.Substrate.Xml
open Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Typed
open Microsoft.Research.Dkal.Substrate.Basic
open Microsoft.Research.Dkal.Ast.Substrate.Basic.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Substrate.Basic.Syntax.Typed
open Microsoft.Research.Dkal.Substrate.Rdf
open Microsoft.Research.Dkal.Ast.Substrate.Rdf.Syntax
open Microsoft.Research.Dkal.Substrate.Reflection

/// Initializes the substrate factories by suscribing all known implementations
/// of substrate parsers and pretty printers. It also adds the basic substrate
/// to the SubstrateMap in order to make it always available
type FactoriesInitializer() =
  
  /// Initialize the substrate factories. Must be called when the front-end starts
  static member Init() =
    // Set basic substrate in Substrate map
    SubstrateMap.AddSubstrate <| new BasicSubstrate()
    
    // SQL Substrate parsers
    SubstrateParserFactory.RegisterParser typeof<SqlSubstrate> "simple" typeof<SimpleSqlParser>
    SubstrateParserFactory.RegisterParser typeof<SqlSubstrate> "typed" typeof<TypedSqlParser>

    // SQL substrate pretty printers
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<SqlSubstrate> "simple" typeof<SimpleSqlPrettyPrinter>
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<SqlSubstrate> "typed" typeof<TypedSqlPrettyPrinter>

    // XML Substrate parsers
    SubstrateParserFactory.RegisterParser typeof<XmlSubstrate> "simple" typeof<SimpleXmlParser>
    SubstrateParserFactory.RegisterParser typeof<XmlSubstrate> "typed" typeof<TypedXmlParser>

    // XML substrate pretty printers
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<XmlSubstrate> "simple" typeof<SimpleXmlPrettyPrinter>
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<XmlSubstrate> "typed" typeof<TypedXmlPrettyPrinter>

    // Basic substrate parsers
    SubstrateParserFactory.RegisterParser typeof<BasicSubstrate> "simple" typeof<SimpleBasicParser>
    SubstrateParserFactory.RegisterParser typeof<BasicSubstrate> "typed" typeof<TypedBasicParser>

    // Basic substrate pretty printers
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<BasicSubstrate> "simple" typeof<SimpleBasicPrettyPrinter>
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<BasicSubstrate> "typed" typeof<TypedBasicPrettyPrinter>

    // Rdf substrate
    SubstrateParserFactory.RegisterParser typeof<RdfSubstrate> "simple" typeof<SparqlParser>
    SubstrateParserFactory.RegisterParser typeof<RdfSubstrate> "typed" typeof<SparqlParser>

    // Reflection substrate
    SubstrateParserFactory.RegisterParser typeof<ReflectionSubstrate> "simple" typeof<ReflectionParser>
