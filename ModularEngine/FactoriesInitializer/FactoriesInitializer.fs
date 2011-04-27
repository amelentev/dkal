namespace Microsoft.Research.Dkal.Factories.Initializer

open Microsoft.Research.Dkal.Substrate.Factories
open Microsoft.Research.Dkal.SqlSubstrate
open Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax
open Microsoft.Research.Dkal.Substrate.TypedSqlSyntax

type FactoriesInitializer() =
  
  static member Init() =
    // Substrate parsers
    SubstrateParserFactory.RegisterParser typeof<SqlSubstrate> "simple" typeof<SimpleSqlParser>
    SubstrateParserFactory.RegisterParser typeof<SqlSubstrate> "typed" typeof<TypedSqlParser>

    // Substrate pretty printers
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<SqlSubstrate> "simple" typeof<SimpleSqlPrettyPrinter>
    SubstratePrettyPrinterFactory.RegisterPrettyPrinter typeof<SqlSubstrate> "typed" typeof<TypedSqlPrettyPrinter>
