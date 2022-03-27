# {anchor:top} {{Ast}} Module
----
This module defines the following types:
* [Substitution](Ast-Module#Substitution)
* [Variable](Ast-Module#Variable)
* [Constant](Ast-Module#Constant)
* [PrincipalConstant](Ast-Module#PrincipalConstant)
* [ForallTerm](Ast-Module#ForallTerm)
* [ExplicitSubstitutionTerm](Ast-Module#ExplicitSubstitutionTerm)
* [Builders](Ast-Module#Builders)
* [ActivePatterns](Ast-Module#ActivePatterns)
* [Type](Ast-Module#Type)
* [Substrate](Ast-Module#Substrate)
----
## {anchor:Substitution} {{Substitution}} Type
Implementation of the ISubstitution interface using a Dictionary

### Implemented Interfaces
* ISubstitution

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |
|  | string | {{ToString}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [ISubstitution](Interfaces-Module#ISubstitution) | {{Id}} | Returns a new Substitution that behaves like the identity |
>{[Back to top](#top)}>
----
## {anchor:Variable} {{Variable}} Type
Variables are typed

### Implemented Interfaces
* IEquatable<Variable>
* IStructuralEquatable
* IVar
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | [Variable](Ast-Module#Variable) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Name}} |  |
| [IType](Interfaces-Module#IType) | {{Type}} |  |
>{[Back to top](#top)}>
----
## {anchor:Constant} {{Constant}} Type
Constants are implicitly typed (they have the type of the wrapped element)

### Implemented Interfaces
* IConst
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |
|  | string | {{ToString}} |  |  |

### Properties
|| Return type || Property name || Description ||
| Object | {{Value}} |  |
>{[Back to top](#top)}>
----
## {anchor:PrincipalConstant} {{PrincipalConstant}} Type
**Base type**: Constant
Principal constants

### Implemented Interfaces
* IConst
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{ToString}} |  |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Name}} |  |
>{[Back to top](#top)}>
----
## {anchor:ForallTerm} {{ForallTerm}} Type
Represents a universally quantified AST term

### Implemented Interfaces
* IEquatable<ForallTerm>
* IStructuralEquatable
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Instantiate}} | [ISubstitution](Interfaces-Module#ISubstitution) | Instantiates the quantifiers using the given substitution over  the bound variables |
|  | [ITerm](Interfaces-Module#ITerm) * [ISubstitution](Interfaces-Module#ISubstitution) | {{ChangeVarName}} | [ISubstitution](Interfaces-Module#ISubstitution) | Changes the variable name to be different to any variable appearing in the  given substitution |
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | [ForallTerm](Ast-Module#ForallTerm) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| [IVar](Interfaces-Module#IVar) | {{Var}} |  |
| [ITerm](Interfaces-Module#ITerm) | {{Term}} |  |
| [ITerm](Interfaces-Module#ITerm) | {{InnerTerm}} | Returns the quantifier-free formula inside of this ForallTerm |
>{[Back to top](#top)}>
----
## {anchor:ExplicitSubstitutionTerm} {{ExplicitSubstitutionTerm}} Type
Represents an AST term that carries an explicit substitution. Whenever a substitution is applied on an ExplicitSubstitutionTerm, instead of modifying the contained term, the given substitution is composed with the explictly carried substitution. This is particularly useful to prevent fragile terms from being corrupted when substitutions are applied on them (e.g., digital signatures on evidence terms)

### Implemented Interfaces
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |
|  | string | {{ToString}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{Term}} | The contained term on which substituions are not going to be applied  directly |
| [ISubstitution](Interfaces-Module#ISubstitution) | {{Substitution}} | The susbstitution that's being explicitly carried |
>{[Back to top](#top)}>
----
## {anchor:Builders} {{Builders}} Type
Defines the public interface on how to construct AST elements defined in the Ast module

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{Var}} | [IVar](Interfaces-Module#IVar) | Constructs a variable ITerm |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{Const}} | [IConst](Interfaces-Module#IConst) | Constructs a constant ITerm |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{Principal}} | string | Constructs a principal constant ITerm |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{Forall}} | [IVar](Interfaces-Module#IVar), [ITerm](Interfaces-Module#ITerm) | Constructs a forall quantified ITerm with a single variable |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{ForallMany}} | [IVar](Interfaces-Module#IVar) list, [ITerm](Interfaces-Module#ITerm) | Constructs a forall quantified ITerm with many variables |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{True}} | Constructs a true literal ITerm |
| [ITerm](Interfaces-Module#ITerm) | {{False}} | Constructs a false literal ITerm |
>{[Back to top](#top)}>
----
## {anchor:ActivePatterns} {{ActivePatterns}} Type
Defines the public interface on how to pattern match AST elements defined in the Ast module

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | unit option | {{|Infon|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the infon type |
| {{static}} | unit option | {{|Principal|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the principal type |
| {{static}} | unit option | {{|SubstrateUpdate|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the substrate update type |
| {{static}} | unit option | {{|SubstrateQuery|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the substrate query type |
| {{static}} | unit option | {{|Action|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the action type |
| {{static}} | unit option | {{|Condition|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the condition type |
| {{static}} | unit option | {{|Rule|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the rule type |
| {{static}} | unit option | {{|Evidence|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the evidence type |
| {{static}} | Type option | {{|Substrate|_|}} | [IType](Interfaces-Module#IType) | Active pattern to match the substrate (.NET elements) type |
| {{static}} | [Variable](Ast-Module#Variable) option | {{|Var|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match variables |
| {{static}} | [IConst](Interfaces-Module#IConst) option | {{|Const|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match constants |
| {{static}} | Object option | {{|SubstrateConstant|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match .NET type elements (integers, strings, etc.) |
| {{static}} | string option | {{|PrincipalConstant|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match principal constants |
| {{static}} | unit option | {{|True|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match true boolean literal |
| {{static}} | unit option | {{|False|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match false boolean literal |
| {{static}} | [IVar](Interfaces-Module#IVar) * [ITerm](Interfaces-Module#ITerm) option | {{|Forall|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern to match forall quantified terms |
>{[Back to top](#top)}>
----
## {anchor:Type} {{Type}} Type
Defines the basic IType implementations for DKAL types

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [IType](Interfaces-Module#IType) | {{FromFullName}} | string |  |

### Properties
|| Return type || Property name || Description ||
| [IType](Interfaces-Module#IType) | {{Infon}} | Type for infons |
| [IType](Interfaces-Module#IType) | {{Principal}} | Type for principals |
| [IType](Interfaces-Module#IType) | {{SubstrateUpdate}} | Type for substrate update terms |
| [IType](Interfaces-Module#IType) | {{SubstrateQuery}} | Type for substrate query terms |
| [IType](Interfaces-Module#IType) | {{Action}} | Type for actions used in policy rules |
| [IType](Interfaces-Module#IType) | {{Condition}} | Type for conditions used in policy rules |
| [IType](Interfaces-Module#IType) | {{Rule}} | Type for policy rules |
| [IType](Interfaces-Module#IType) | {{Evidence}} | Type for evidence (a.k.a. justification, or proof) |
| [IType](Interfaces-Module#IType) | {{Boolean}} |  |
| [IType](Interfaces-Module#IType) | {{Int32}} |  |
| [IType](Interfaces-Module#IType) | {{Double}} |  |
| [IType](Interfaces-Module#IType) | {{String}} |  |
>{[Back to top](#top)}>
----
## {anchor:Substrate} {{Substrate}} Type
Defines an IType implementation to use .NET types as DKAL types

### Implemented Interfaces
* IType

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |
|  | string | {{ToString}} |  |  |

### Properties
|| Return type || Property name || Description ||
| Type | {{Type}} | The .NET type wrapped by this Substrate type |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
