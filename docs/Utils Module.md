# {anchor:top} {{Utils}} Module
----
This module defines the following types:
* [PrettyPrintToken](Utils-Module#PrettyPrintToken)
* [TextToken](Utils-Module#TextToken)
* [ManyTokens](Utils-Module#ManyTokens)
* [PrettyPrinter](Utils-Module#PrettyPrinter)
* [ParseException](Utils-Module#ParseException)
----
## {anchor:PrettyPrintToken} {{PrettyPrintToken}} Type
Pretty printing tokens are fed to the PrettyPrinter in order to produce a string representation

### Implemented Interfaces
* IEquatable<PrettyPrintToken>
* IStructuralEquatable
* IComparable<PrettyPrintToken>
* IComparable
* IStructuralComparable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [PrettyPrintToken](Utils-Module#PrettyPrintToken) | {{NewManyTokens}} | [PrettyPrintToken](Utils-Module#PrettyPrintToken) list |  |
| {{static}} | [PrettyPrintToken](Utils-Module#PrettyPrintToken) | {{NewTextToken}} | string |  |
|  | int | {{CompareTo}} | [PrettyPrintToken](Utils-Module#PrettyPrintToken) |  |
|  | int | {{CompareTo}} | Object |  |
|  | int | {{CompareTo}} | Object, IComparer |  |
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | [PrettyPrintToken](Utils-Module#PrettyPrintToken) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| int | {{Tag}} |  |
| bool | {{IsManyTokens}} |  |
| bool | {{IsTextToken}} |  |
| [PrettyPrintToken](Utils-Module#PrettyPrintToken) | {{NewLineToken}} |  |
| bool | {{IsNewLineToken}} |  |
| [PrettyPrintToken](Utils-Module#PrettyPrintToken) | {{UntabToken}} |  |
| bool | {{IsUntabToken}} |  |
| [PrettyPrintToken](Utils-Module#PrettyPrintToken) | {{TabToken}} |  |
| bool | {{IsTabToken}} |  |
>{[Back to top](#top)}>
----
## {anchor:TextToken} {{TextToken}} Type
**Base type**: PrettyPrintToken
Produces the given text and continues

### Implemented Interfaces
* IEquatable<PrettyPrintToken>
* IStructuralEquatable
* IComparable<PrettyPrintToken>
* IComparable
* IStructuralComparable

### Properties
|| Return type || Property name || Description ||
| string | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:ManyTokens} {{ManyTokens}} Type
**Base type**: PrettyPrintToken
Produces the text given by the nested tokens and continues

### Implemented Interfaces
* IEquatable<PrettyPrintToken>
* IStructuralEquatable
* IComparable<PrettyPrintToken>
* IComparable
* IStructuralComparable

### Properties
|| Return type || Property name || Description ||
| [PrettyPrintToken](Utils-Module#PrettyPrintToken) list | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:PrettyPrinter} {{PrettyPrinter}} Type
The PrettyPrinter provides functionality to print a list of PrettyPrintTokens

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | string | {{PrettyPrint}} | [PrettyPrintToken](Utils-Module#PrettyPrintToken) list | Transform the tokens into their string representation |
>{[Back to top](#top)}>
----
## {anchor:ParseException} {{ParseException}} Type
**Base type**: Exception
Raised when something goes wrong during parsing. It contains the line and  column on which the error was produced, an informative text and the full text with the context on which the error occured

### Implemented Interfaces
* ISerializable
* _Exception
* IStructuralEquatable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | Exception |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Data0}} |  |
| string | {{Data1}} |  |
| int | {{Data2}} |  |
| int | {{Data3}} |  |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
