# {anchor:top} {{Ast.Tree}} Module
----
This module defines the following types:
* [Function](Ast.Tree-Module#Function)
* [Application](Ast.Tree-Module#Application)
* [ActivePatterns](Ast.Tree-Module#ActivePatterns)
* [Builders](Ast.Tree-Module#Builders)
----
## {anchor:Function} {{Function}} Type
Functions are used in App ITerms to indicate what function is applied. They have an arbitrary-sized typed list of arguments. They return a typed single value. The Identity field is None if the function is not  associative; it points to the ITerm that behaves as identity otherwise

### Implemented Interfaces
* IEquatable<Function>
* IStructuralEquatable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | [Function](Ast.Tree-Module#Function) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Name}} |  |
| [IType](Interfaces-Module#IType) | {{RetType}} |  |
| [IType](Interfaces-Module#IType) list | {{ArgsType}} |  |
| [ITerm](Interfaces-Module#ITerm) option | {{Identity}} |  |
>{[Back to top](#top)}>
----
## {anchor:Application} {{Application}} Type
Application ITerms are the inner nodes of the AST tree. They have a Function and a list of arguments

### Implemented Interfaces
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [Function](Ast.Tree-Module#Function) | {{Function}} |  |
| [ITerm](Interfaces-Module#ITerm) list | {{Args}} |  |
>{[Back to top](#top)}>
----
## {anchor:ActivePatterns} {{ActivePatterns}} Type
Defines the public interface on how to pattern match AST elements defined in the Ast.Tree module

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [Function](Ast.Tree-Module#Function) * [ITerm](Interfaces-Module#ITerm) list option | {{|App|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to recognize function application AST elements |
>{[Back to top](#top)}>
----
## {anchor:Builders} {{Builders}} Type
Defines the public interface on how to construct AST elements defined in the Ast.Tree module

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{App}} | [Function](Ast.Tree-Module#Function), [ITerm](Interfaces-Module#ITerm) list | Build a function application AST node. It will check that the amount of  parameters and their types are correct before building the term |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
