# {anchor:top} {{Ast.Syntax.Parsing}} Module
----
This module defines the following types:
* [ParsingContext](Ast.Syntax.Parsing-Module#ParsingContext)
* [LocalParsingContext](Ast.Syntax.Parsing-Module#LocalParsingContext)
* [GeneralParser](Ast.Syntax.Parsing-Module#GeneralParser)
----
## {anchor:ParsingContext} {{ParsingContext}} Type
A ParsingContext stores information that helps the parsing process such as macros, variable types, fresh variable ids, etc.

### Implemented Interfaces
* IParsingContext
>{[Back to top](#top)}>
----
## {anchor:LocalParsingContext} {{LocalParsingContext}} Type
A LocalParsingContext refers to a parent IParsingContext for extra information

### Implemented Interfaces
* IParsingContext
>{[Back to top](#top)}>
----
## {anchor:GeneralParser} {{GeneralParser}} Type
A general parser provides a unified interface to parse and throw parsing exceptions

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | b | {{TryParse}} | LexBuffer<Char> -> b, string |  |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
