# {anchor:top} {{Substrate.Basic}} Module
----
This module defines the following types:
* [BasicSubstrateTerm](Substrate.Basic-Module#BasicSubstrateTerm)
* [BasicSubstrate](Substrate.Basic-Module#BasicSubstrate)
* [ActivePatterns](Substrate.Basic-Module#ActivePatterns)
* [Builders](Substrate.Basic-Module#Builders)
* [BasicPrimitives](Substrate.Basic-Module#BasicPrimitives)
----
## {anchor:BasicSubstrateTerm} {{BasicSubstrateTerm}} Type
A BasicSubstrateTerm is a query term for the BasicSubstrate. It has a left side term, which can only be a variable (used for output) or a constant  (used for comparison), and a right side term which can be a complex  expression.

### Implemented Interfaces
* ISubstrateQueryTerm
* ISubstrateTerm
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{Left}} | The left side of the BasicSubstrateTerm. It can contain a variable (for  output) or a constant (for comparison) |
| [ITerm](Interfaces-Module#ITerm) | {{Right}} | The right side of the BasicSubstrateTerm. It can contain a complex  expression |
>{[Back to top](#top)}>
----
## {anchor:BasicSubstrate} {{BasicSubstrate}} Type
The BasicSubstrate is a calculator substrate that does not handle data. It  can solve basic arithmetic computations. E.g., {| "basic" | Y := X + 10 |}

### Implemented Interfaces
* ISubstrate
>{[Back to top](#top)}>
----
## {anchor:ActivePatterns} {{ActivePatterns}} Type
Defines the public interface on how to pattern match AST elements defined for the BasicSubstrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|AndBool|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches a conjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|OrBool|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches a disjunction |
| {{static}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) option | {{|AsBoolean|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches an asBoolean construct (used to nest queries) |
>{[Back to top](#top)}>
----
## {anchor:Builders} {{Builders}} Type
Defines the public interface on how to construct AST elements defined in the BasicSubstrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AndBool}} | [ITerm](Interfaces-Module#ITerm) list | Constructs a conjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{OrBool}} | [ITerm](Interfaces-Module#ITerm) list | Constructs a disjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AsBoolean}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) | Constructs an AsBoolean expression (used for nested substrate queries) |
>{[Back to top](#top)}>
----
## {anchor:BasicPrimitives} {{BasicPrimitives}} Type
Defines the primitive functions used to construct Application elements in the BasicSubstrate builders

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [Function](Ast.Tree-Module#Function) option | {{SolveOverloadOperator}} | string, [IType](Interfaces-Module#IType) | Given an overloaded function name and the type of one of its parameters it looks the type information to see if there is a match. |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
