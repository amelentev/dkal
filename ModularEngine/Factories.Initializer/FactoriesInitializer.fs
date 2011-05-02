namespace Microsoft.Research.Dkal.Factories.Initializer

open Microsoft.Research.Dkal.Substrate.Factories
open Microsoft.Research.Dkal.Substrate.Sql
open Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Typed
open Microsoft.Research.Dkal.Substrate.Xml
open Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Typed

type FactoriesInitializer() =
  
  static member Init() =
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
