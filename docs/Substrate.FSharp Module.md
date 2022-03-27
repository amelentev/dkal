# {anchor:top} {{Substrate.FSharp}} Module
----
This module defines the following types:
* [FSharp](Substrate.FSharp-Module#FSharp)
* [Function](Substrate.FSharp-Module#Function)
* [FunctionTerm](Substrate.FSharp-Module#FunctionTerm)
* [Function](Substrate.FSharp-Module#Function)
* [Var](Substrate.FSharp-Module#Var)
* [Const](Substrate.FSharp-Module#Const)
* [FunctionQueryTerm](Substrate.FSharp-Module#FunctionQueryTerm)
* [FSharpSubstrate](Substrate.FSharp-Module#FSharpSubstrate)
----
## {anchor:FSharp} {{FSharp}} Type
A substrate the wraps a mapping from function names to (curried) F# functions. This substrate is not intended to be used directly. The purpose of this substrate is to serve as a basis for the implementation of specialized substrates via initialization of an instance of an FSharpSubstrate with a concrete mapping from function names to corresponding F# implementations. Example for usage: Substrate.Crypto

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{createFunction}} | string, Type, [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) list |  |
| {{static}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{var}} | string |  |
| {{static}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{con}} | a |  |
| {{static}} | [IVar](Interfaces-Module#IVar) | {{toVar}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) |  |
| {{static}} | Object | {{toConstElem}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) |  |
>{[Back to top](#top)}>
----
## {anchor:Function} {{Function}} Type
A function application AST element

### Implemented Interfaces
* IEquatable<Function>
* IStructuralEquatable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | [Function](Substrate.FSharp-Module#Function) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Name}} |  |
| Type | {{ReturnType}} |  |
| [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) list | {{Args}} |  |
>{[Back to top](#top)}>
----
## {anchor:FunctionTerm} {{FunctionTerm}} Type
A type to keep AST elements of the F# Substrate

### Implemented Interfaces
* IEquatable<FunctionTerm>
* IStructuralEquatable
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{NewConst}} | [Constant](Ast-Module#Constant) |  |
| {{static}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{NewVar}} | [IVar](Interfaces-Module#IVar) |  |
| {{static}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{NewFunction}} | [Function](Substrate.FSharp-Module#Function) |  |
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  |  |
|  | a option | {{UnifyFrom}} | a, [ITerm](Interfaces-Module#ITerm) |  |
|  | bool | {{Equals}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| int | {{Tag}} |  |
| bool | {{IsConst}} |  |
| bool | {{IsVar}} |  |
| bool | {{IsFunction}} |  |
| [IType](Interfaces-Module#IType) | {{Type}} |  |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} |  |
| bool | {{isGround}} |  |
| bool | {{isGroundOrVar}} |  |
>{[Back to top](#top)}>
----
## {anchor:Function} {{Function}} Type
**Base type**: FunctionTerm
Function application AST element for the F# Substrate (uses Function type)

### Implemented Interfaces
* IEquatable<FunctionTerm>
* IStructuralEquatable
* ITerm

### Properties
|| Return type || Property name || Description ||
| [Function](Substrate.FSharp-Module#Function) | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:Var} {{Var}} Type
**Base type**: FunctionTerm
Variable AST element for the F# Substrate

### Implemented Interfaces
* IEquatable<FunctionTerm>
* IStructuralEquatable
* ITerm

### Properties
|| Return type || Property name || Description ||
| [IVar](Interfaces-Module#IVar) | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:Const} {{Const}} Type
**Base type**: FunctionTerm
Constant AST element for the F# Substrate

### Implemented Interfaces
* IEquatable<FunctionTerm>
* IStructuralEquatable
* ITerm

### Properties
|| Return type || Property name || Description ||
| [Constant](Ast-Module#Constant) | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:FunctionQueryTerm} {{FunctionQueryTerm}} Type
The Result is either a _ground_ term, a constant or a variable. When solving the term the values of Body and Result are unified.

### Implemented Interfaces
* ITerm
* ISubstrateQueryTerm
* ISubstrateTerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  |  |
|  | a option | {{UnifyFrom}} | a, [ITerm](Interfaces-Module#ITerm) |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{Body}} |  |
| [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{Result}} |  |
| string | {{Namespace}} |  |
| [IType](Interfaces-Module#IType) | {{Type}} |  |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} |  |
>{[Back to top](#top)}>
----
## {anchor:FSharpSubstrate} {{FSharpSubstrate}} Type
FSharpSubstrate that takes FunctionQueryTerms and solves them by means of applying the functions to their arguments

### Implemented Interfaces
* ISubstrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) | {{createFunctionTerm}} | string, [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) list |  |
|  | [FunctionQueryTerm](Substrate.FSharp-Module#FunctionQueryTerm) | {{createQueryTerm}} | [FunctionTerm](Substrate.FSharp-Module#FunctionTerm), [FunctionTerm](Substrate.FSharp-Module#FunctionTerm) |  |
|  | unit | {{Add}} | string, a |  |
|  | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{Solve}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) seq, [ISubstitution](Interfaces-Module#ISubstitution) seq |  |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
