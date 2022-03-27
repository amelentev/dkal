# {anchor:top} {{Substrate.Factories}} Module
----
This module defines the following types:
* [SubstrateParserFactory](Substrate.Factories-Module#SubstrateParserFactory)
* [SubstrateFactory](Substrate.Factories-Module#SubstrateFactory)
* [SubstratePrettyPrinterFactory](Substrate.Factories-Module#SubstratePrettyPrinterFactory)
----
## {anchor:SubstrateParserFactory} {{SubstrateParserFactory}} Type
The SubstrateParserFactory is used to construct parsers for different  types of substrates. In order to have dependency injection, parsers are registered at run-time (see Factories.Initializer project)

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | unit | {{RegisterParser}} | Type, string, Type | Given a substrate type (type implementing ISubstrate), a syntax kind (e.g., "simple", "typed") and a parser type (type implementing ISubstrateParser) it stores this information to be used when a request to construct a  parser arrives |
| {{static}} | [ISubstrateParser](Interfaces-Module#ISubstrateParser) | {{SubstrateParser}} | [ISubstrate](Interfaces-Module#ISubstrate), string, string, [IParsingContext](Interfaces-Module#IParsingContext) option | Given a substrate, a syntax kind and a parsing context it attempts to find a registered substrate parser that matches. If successful, it  returns such a ISubstrateParser implementation, on which the substrate, the namespace and the parsing context have been set using the appropiate setters |
>{[Back to top](#top)}>
----
## {anchor:SubstrateFactory} {{SubstrateFactory}} Type
The SubstrateFactory provides a factory to construct different substrates.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ISubstrate](Interfaces-Module#ISubstrate) | {{Substrate}} | string, string list, string list | Construct a Substrate. A substrate kind, substrate arguments (such as  connection string, in the case of SQL) and namespaces must be provided |
>{[Back to top](#top)}>
----
## {anchor:SubstratePrettyPrinterFactory} {{SubstratePrettyPrinterFactory}} Type
The SubstratePrettyPrinterFactory is used to construct pretty printers for different types of substrates. In order to have dependency injection,  pretty printers are registered at run-time (see Factories.Initializer project)

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | unit | {{RegisterPrettyPrinter}} | Type, string, Type | Given a substrate type (type implementing ISubstrate), a syntax kind (e.g., "simple", "typed") and a pretty printer type (type implementing  ISubstratePrettyPrinter) it stores this information to be used when a  request to construct a pretty printer arrives |
| {{static}} | [ISubstratePrettyPrinter](Interfaces-Module#ISubstratePrettyPrinter) | {{SubstratePrettyPrinter}} | [ISubstrate](Interfaces-Module#ISubstrate), string | Given a substrate and a syntax kind it attempts to find a registered  substrate pretty printer that matches. If successful, it returns such a  ISubstratePrettyPrinter implementation |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
