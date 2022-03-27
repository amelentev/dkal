# {anchor:top} {{Ast.Infon}} Module
----
This module defines the following types:
* [IInfonPrettyPrinter](Ast.Infon-Module#IInfonPrettyPrinter)
* [IInfonParser](Ast.Infon-Module#IInfonParser)
* [ActivePatterns](Ast.Infon-Module#ActivePatterns)
* [Builders](Ast.Infon-Module#Builders)
* [Primitives](Ast.Infon-Module#Primitives)
----
## {anchor:IInfonPrettyPrinter} {{IInfonPrettyPrinter}} Type
IInfonPrettyPrinter provides an interface to print top-level AST elements

### Implemented Interfaces
* IPrettyPrinter

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{PrintPolicy}} | [Policy](Globals-Module#Policy) | Returns a string representation for the given Policy |
|  | string | {{PrintSignature}} | [Signature](Globals-Module#Signature) | Returns a string representation for the given Signature |
|  | string | {{PrintAssembly}} | [Assembly](Globals-Module#Assembly) | Returns a string representation for the given Assembly |
>{[Back to top](#top)}>
----
## {anchor:IInfonParser} {{IInfonParser}} Type
IInfonParser provides an interface for top-level parsers that interpret policies

### Implemented Interfaces
* IParser

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{SetParsingContext}} | [IParsingContext](Interfaces-Module#IParsingContext) | Sets the IParsingContext implementation |
|  | [ITerm](Interfaces-Module#ITerm) | {{ParseInfon}} | string | Parse an infon (ITerm) from the input string |
|  | [ITerm](Interfaces-Module#ITerm) | {{ParseRule}} | string | Parse a rule (ITerm) from the input string |
|  | [Policy](Globals-Module#Policy) | {{ParsePolicy}} | string | Parse a Policy from the input string |
|  | [Signature](Globals-Module#Signature) | {{ParseSignature}} | string | Parse a Signature from the input string |
|  | [Assembly](Globals-Module#Assembly) | {{ParseAssembly}} | string | Parse an Assembly from the input string |
>{[Back to top](#top)}>
----
## {anchor:ActivePatterns} {{ActivePatterns}} Type
Defines the public interface on how to pattern match AST elements defined in the Ast.Infon module

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|SeqRule|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the sequencing of rules |
| {{static}} | unit option | {{|EmptyRule|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the empty rule |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|Rule|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the simple rule |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|SeqCondition|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the sequencing of conditions |
| {{static}} | unit option | {{|EmptyCondition|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the empty condition |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|WireCondition|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the upon condition |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|KnownCondition|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the if condition |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|SeqAction|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the sequencing of actions |
| {{static}} | unit option | {{|EmptyAction|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the empty action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|Send|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the send action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|JustifiedSend|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the send with justification action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|JustifiedSay|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the say with justification action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|Learn|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the learn infon action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|Forget|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the forget infon action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|Install|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the install rule action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|Uninstall|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the uninstall rule action |
| {{static}} | [ISubstrateUpdateTerm](Interfaces-Module#ISubstrateUpdateTerm) option | {{|Apply|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the apply substrate update action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|Fresh|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the fresh id action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|Complete|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for complete relation declaration action |
| {{static}} | unit option | {{|EmptyInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for the empty infon |
| {{static}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) option | {{|AsInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for asInfon |
| {{static}} | [ITerm](Interfaces-Module#ITerm) option | {{|NotInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for infon negation |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|AndInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for infon conjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|OrInfon|_|}} | [ITerm](Interfaces-Module#ITerm) |  |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|ImpliesInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for infon implication |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|SaidInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for said quotation |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|JustifiedInfon|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for justified infons (infons with evidence) |
| {{static}} | unit option | {{|EmptyEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for empty evidence |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ISubstitution](Interfaces-Module#ISubstitution) option | {{|ConcretizationEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for concretization of evidence (explicit substitutions) |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|SignatureEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for signed evidence |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|ModusPonensEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for modus ponens evidence |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|ImplicationIntroductionEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for implication introduction evidence |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|AndEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for conjunction of evidence |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|AndEliminationEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for conjunction elimination evidence |
| {{static}} | [ITerm](Interfaces-Module#ITerm) * [ITerm](Interfaces-Module#ITerm) option | {{|OrIntroductionEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for disjunction introduction evidence |
| {{static}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) option | {{|AsInfonEvidence|_|}} | [ITerm](Interfaces-Module#ITerm) | Active pattern for asInfon evidence (basic theorems) |
>{[Back to top](#top)}>
----
## {anchor:Builders} {{Builders}} Type
Defines the public interface on how to construct AST elements defined in the Ast.Infon module

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{SeqRule}} | [ITerm](Interfaces-Module#ITerm) list | Build a sequencing of rules |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{RuleRule}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a simple rule |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{SeqCondition}} | [ITerm](Interfaces-Module#ITerm) list | Build a sequencing of conditions |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{WireCondition}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build an upon condition |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{KnownCondition}} | [ITerm](Interfaces-Module#ITerm) | Build an if condition |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{SeqAction}} | [ITerm](Interfaces-Module#ITerm) list | Build a sequencing of actions |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{SendAction}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a send action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{JustifiedSendAction}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a send with justification action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{JustifiedSayAction}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a say with justification action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{LearnAction}} | [ITerm](Interfaces-Module#ITerm) | Build a learn infon action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{ForgetAction}} | [ITerm](Interfaces-Module#ITerm) | Build a forget infon action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{InstallAction}} | [ITerm](Interfaces-Module#ITerm) | Build an install rule action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{UninstallAction}} | [ITerm](Interfaces-Module#ITerm) | Build an uninstall rule action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{ApplyAction}} | [ITerm](Interfaces-Module#ITerm) | Build an apply substrate update action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{FreshAction}} | [ITerm](Interfaces-Module#ITerm) | Build a fresh id action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{CompleteAction}} | [ITerm](Interfaces-Module#ITerm) | Build a complete relation action |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AsInfon}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) | Build an asInfon |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{NotInfon}} | [ITerm](Interfaces-Module#ITerm) | Build negation of an infon |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AndInfon}} | [ITerm](Interfaces-Module#ITerm) list | Build an infon conjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{OrInfon}} | [ITerm](Interfaces-Module#ITerm) list |  |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{ImpliesInfon}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build an infon implication |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{SaidInfon}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a said quotation infon |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{JustifiedInfon}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a justified infon (infon with evidence) |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{PrefixedInfon}} | [ITerm](Interfaces-Module#ITerm) list, [ITerm](Interfaces-Module#ITerm) | Build a quotation infon with several principals (e.g., p1 said p2 said  p3 said [...](...) i) |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{SignatureEvidence}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a signature evidence and wrap it with a ExplicitSubstitutionTerm  to prevent it from being substituted |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{ModusPonensEvidence}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build a modus ponens evidence / Implication Elimination / ->e |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{ImplicationIntroductionEvidence}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build Implication Introduction evidence / ->i |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AndEvidence}} | [ITerm](Interfaces-Module#ITerm) list | Build Conjunction Introduction evidence / &i |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AndEliminationEvidence}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build Conjunction Elimination evidence / &e |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{OrIntroductionEvidence}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Build Disjunction Introduction evidence / |i |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AsInfonEvidence}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) | Build asInfon evidence (for basic theorems) |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{EmptyRule}} | Build an empty rule |
| [ITerm](Interfaces-Module#ITerm) | {{EmptyCondition}} | Build an empty condition |
| [ITerm](Interfaces-Module#ITerm) | {{EmptyAction}} | Build an empty action |
| [ITerm](Interfaces-Module#ITerm) | {{EmptyInfon}} | Build an empty infon |
| [ITerm](Interfaces-Module#ITerm) | {{EmptyEvidence}} | Build empty evidence |
>{[Back to top](#top)}>
----
## {anchor:Primitives} {{Primitives}} Type
Defines the primitive functions used to construct Application elements in the Ast module builders

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [Function](Ast.Tree-Module#Function) option | {{SolveFunction}} | string | Given a primitive function name it returns a Fuction, if anyone matches; None otherwise |
>{[Back to top](#top)}>
----
>{Automatically generated on 7/17/2013 12:17:16 PM}>
