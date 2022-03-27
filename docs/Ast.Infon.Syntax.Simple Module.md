# {anchor:top} {{Ast.Infon.Syntax.Simple}} Module
----
This module defines the following types:
* [SimplePrettyPrinter](Ast.Infon.Syntax.Simple-Module#SimplePrettyPrinter)
* [SimpleParser](Ast.Infon.Syntax.Simple-Module#SimpleParser)
----
## {anchor:SimplePrettyPrinter} {{SimplePrettyPrinter}} Type
The SimplePrettyPrinter prints AST elements into the simple concrete syntax, which uses declared typed variables

### Implemented Interfaces
* IInfonPrettyPrinter
* IPrettyPrinter
>{[Back to top](#top)}>
----
## {anchor:SimpleParser} {{SimpleParser}} Type
The SimpleParser parses from the simple concrete syntax, which uses declared  typed variables. It must be initialized with a Context that holds variable  type information, relation declarations, etc.

### Implemented Interfaces
* IInfonParser
* IParser
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
