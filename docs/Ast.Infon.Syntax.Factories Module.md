# {anchor:top} {{Ast.Infon.Syntax.Factories}} Module
----
This module defines the following types:
* [ParserFactory](Ast.Infon.Syntax.Factories-Module#ParserFactory)
* [PrettyPrinterFactory](Ast.Infon.Syntax.Factories-Module#PrettyPrinterFactory)
----
## {anchor:ParserFactory} {{ParserFactory}} Type
The ParserFactory provides a factory to construct different parsers.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [IInfonParser](Ast.Infon-Module#IInfonParser) | {{InfonParser}} | string, string | Constructs an InfonParser. A parser kind and the name of the current  principal must both be provided. |
>{[Back to top](#top)}>
----
## {anchor:PrettyPrinterFactory} {{PrettyPrinterFactory}} Type
The PrettyPrinterFactory provides a factory to construct different pretty printers.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [IInfonPrettyPrinter](Ast.Infon-Module#IInfonPrettyPrinter) | {{InfonPrinter}} | string | Constructs an InfonPrinter. A pretty printer kind must be provided |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
