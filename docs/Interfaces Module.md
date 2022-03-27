# {anchor:top} {{Interfaces}} Module
----
This module defines the following types:
* [IType](Interfaces-Module#IType)
* [ITerm](Interfaces-Module#ITerm)
* [IVar](Interfaces-Module#IVar)
* [IConst](Interfaces-Module#IConst)
* [ISubstitution](Interfaces-Module#ISubstitution)
* [IParser](Interfaces-Module#IParser)
* [IPrettyPrinter](Interfaces-Module#IPrettyPrinter)
* [ISubstrateTerm](Interfaces-Module#ISubstrateTerm)
* [ISubstrateUpdateTerm](Interfaces-Module#ISubstrateUpdateTerm)
* [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm)
* [ISubstrate](Interfaces-Module#ISubstrate)
* [IInfostrate](Interfaces-Module#IInfostrate)
* [IMailBox](Interfaces-Module#IMailBox)
* [IRouter](Interfaces-Module#IRouter)
* [ISignatureProvider](Interfaces-Module#ISignatureProvider)
* [ILogicEngine](Interfaces-Module#ILogicEngine)
* [IExecutor](Interfaces-Module#IExecutor)
* [IParsingContext](Interfaces-Module#IParsingContext)
* [ISubstrateParser](Interfaces-Module#ISubstrateParser)
* [ISubstratePrettyPrinter](Interfaces-Module#ISubstratePrettyPrinter)
* [ITranslatedExpr](Interfaces-Module#ITranslatedExpr)
* [ITranslator](Interfaces-Module#ITranslator)
----
## {anchor:IType} {{IType}} Type
Each IType implementation represents a type for AST elements. For instance infons, principals, integers, rules, evidence, ...

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{IsSubtypeOf}} | [IType](Interfaces-Module#IType) | Returns true if this type can be seen as the typed passed as parameter |

### Properties
|| Return type || Property name || Description ||
| string | {{Name}} | Short name for this type |
| string | {{FullName}} | Full name for this type |
| [IType](Interfaces-Module#IType) option | {{BaseType}} | Base type |
>{[Back to top](#top)}>
----
## {anchor:ITerm} {{ITerm}} Type
ITerm implementations are the AST elements. They encode everything from  variables and constants to asInfon queries, rules, etc.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) | Returns a new ITerm which results from applying the given substitution |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  | Returns a new ITerm which is a normalized version of this one |
|  | [ISubstitution](Interfaces-Module#ISubstitution) option | {{UnifyFrom}} | [ISubstitution](Interfaces-Module#ISubstitution), [ITerm](Interfaces-Module#ITerm) | It tries to unify this ITerm with the given one, starting from the  given substitution. If the unification is successful, a new substitution (which is a specialization of the given one) is returned. None is returned if unification fails |
|  | [ISubstitution](Interfaces-Module#ISubstitution) option | {{Unify}} | [ITerm](Interfaces-Module#ITerm) | It tries to unify this ITerm with the given one. If unification is successful a substitution that guarantees syntactic equality is returned, otherwise  None is returned |

### Properties
|| Return type || Property name || Description ||
| [IType](Interfaces-Module#IType) | {{Type}} | The type of this AST element |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} | The free variables in this AST element |
| [IVar](Interfaces-Module#IVar) list | {{BoundVars}} | The bound variables in this AST element |
>{[Back to top](#top)}>
----
## {anchor:IVar} {{IVar}} Type
IVar is used to represent any AST element that is to be treated like a  variable

### Implemented Interfaces
* ITerm

### Properties
|| Return type || Property name || Description ||
| string | {{Name}} | Variable name, which is used as variable identifier |
>{[Back to top](#top)}>
----
## {anchor:IConst} {{IConst}} Type
IConst is an interface that is implemented by all the AST nodes that represent ground values

### Implemented Interfaces
* ITerm

### Properties
|| Return type || Property name || Description ||
| Object | {{Value}} | The value of this constant |
>{[Back to top](#top)}>
----
## {anchor:ISubstitution} {{ISubstitution}} Type
ISubstitution implementations model substitutions that map variables to AST  elements (IVar --> ITerm). They are used as results of unification

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [IVar](Interfaces-Module#IVar) | Applies this substitution to the given IVar |
|  | [ISubstitution](Interfaces-Module#ISubstitution) | {{Extend}} | [IVar](Interfaces-Module#IVar), [ITerm](Interfaces-Module#ITerm) | Returns a new substitution that results from extending the current  substitution so that it maps v to t and leaves the rest unchanged |
|  | [ISubstitution](Interfaces-Module#ISubstitution) | {{ComposeWith}} | [ISubstitution](Interfaces-Module#ISubstitution) | Returns a new substitution that results from first applying s' and  then applying the current Substitution |
|  | bool | {{DomainContains}} | [IVar](Interfaces-Module#IVar) | Returns true iff v is affected by this substitution |
|  | [ISubstitution](Interfaces-Module#ISubstitution) | {{RestrictTo}} | [IVar](Interfaces-Module#IVar) list | Returns a new substitution that results from restricting the current  one to only modify the variables given in the list |
|  | [ISubstitution](Interfaces-Module#ISubstitution) | {{Forget}} | [IVar](Interfaces-Module#IVar) list | Returns a new substitution that results from forgetting the current mapping to the variables given in the list |

### Properties
|| Return type || Property name || Description ||
| [IVar](Interfaces-Module#IVar) list | {{Domain}} | Returns the vars affected by this substitution |
| bool | {{IsVariableRenaming}} | Returns true if this substitution only renames variables |
>{[Back to top](#top)}>
----
## {anchor:IParser} {{IParser}} Type
IParser provides an interface for different parsers. All implementations of IParser process input strings and return ITerms

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [IType](Interfaces-Module#IType) | {{ParseType}} | string | Parse an IType from the input string |
|  | [ITerm](Interfaces-Module#ITerm) | {{ParseTerm}} | string | Parse an ITerm from the input string |
>{[Back to top](#top)}>
----
## {anchor:IPrettyPrinter} {{IPrettyPrinter}} Type
IPrettyPrinter provides an interface to print ITerms back into different concrete syntaxes. An IPrettyPrinter implementation should be bound to an IParser implementation that can parse the output back

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{PrintType}} | [IType](Interfaces-Module#IType) | Returns a string representation for the given IType |
|  | string | {{PrintTerm}} | [ITerm](Interfaces-Module#ITerm) | Returns a string representation for the given ITerm (infon, rule, etc.) |
>{[Back to top](#top)}>
----
## {anchor:ISubstrateTerm} {{ISubstrateTerm}} Type
ISubstrateTerm implementations are AST elements which are related to a substrate. Each ISubstrateTerm has a Namespace, each ISubstrate understands a set of Namespaces.

### Implemented Interfaces
* ITerm

### Properties
|| Return type || Property name || Description ||
| string | {{Namespace}} | Tha namespace for this ISubstrateTerm. This is used to locate an ISubstrate implementation that will understand this ISubstrateTerm |
>{[Back to top](#top)}>
----
## {anchor:ISubstrateUpdateTerm} {{ISubstrateUpdateTerm}} Type
ISubstrateUpdateTerm implementations are substrate terms that perform updates (addition, modification, removal, changing parameters etc.)  on a substrate. They are used in "apply" acctions

### Implemented Interfaces
* ISubstrateTerm
* ITerm
>{[Back to top](#top)}>
----
## {anchor:ISubstrateQueryTerm} {{ISubstrateQueryTerm}} Type
ISubstrateQueryTerm implementations are substrate terms that perform queries on a substrate. They are used in asInfon constructs

### Implemented Interfaces
* ISubstrateTerm
* ITerm
>{[Back to top](#top)}>
----
## {anchor:ISubstrate} {{ISubstrate}} Type
ISubstrate implementations give DKAL the ability to query and modify data sitting in various formats and representations (XML, arithmetics, SQL, etc.) Each substrate understand a set of namespaces, which are used to fetch the  proper substrate when an ISubstrateTerm is found and needs to be executed

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{Solve}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) seq, [ISubstitution](Interfaces-Module#ISubstitution) seq | Performs the given queries (which must have a namespace understood by this substrate) for each of the given input substitutions (partial results).  Returns a sequence of resolved substitutions (more specialized than substs) |
|  | bool | {{Update}} | [ISubstrateUpdateTerm](Interfaces-Module#ISubstrateUpdateTerm) seq | Applies the given ISubstrateUpdateTerms (which must have a namespace  understood by this substrate). Returns true upon success, false otherwise |
|  | bool | {{AreConsistentUpdates}} | [ISubstrateUpdateTerm](Interfaces-Module#ISubstrateUpdateTerm) seq | Given the ISubstrateUpdateTerms (which must have a namespace understood  by this substrate) it returns true iff it is safe to apply them in  parallel (e.g. no row is being deleted and added at the same time, etc.) |
|  | [IVar](Interfaces-Module#IVar) list | {{RequiredVars}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) | Given the ISubstrateQueryTerm (which must have a namespace understood  by this substrate), it returns which of the query free variables must  be instantiated before executing the query. For instance, an arithmetic calculator substrate may require all variables to be instantiated,  except for the variable that gets the result (left-hand side can be free) |

### Properties
|| Return type || Property name || Description ||
| HashSet<string> | {{Namespaces}} | Returns the set of namespaces that this ISubstrate implementation understands |
>{[Back to top](#top)}>
----
## {anchor:IInfostrate} {{IInfostrate}} Type
Provides an interface for a repository of knowledge in the form of infon ITerms

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{Learn}} | [ITerm](Interfaces-Module#ITerm) | Adds the given infon ITerm to the infostrate |
|  | bool | {{Forget}} | [ITerm](Interfaces-Module#ITerm) | Removes the given infon ITerm from the infostrate |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) seq | {{Knowledge}} | Returns a sequence of knowledge from the infostrate |
>{[Back to top](#top)}>
----
## {anchor:IMailBox} {{IMailBox}} Type
Provides an interface for a repository of incoming infon ITerms

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{Add}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Adds the given infon ITerm to the mailbox |
|  | unit | {{Remove}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) list option | Removes the given infon ITerm from the mailbox |
|  | unit | {{Prune}} |  | Eliminates "old" messages, each mailbox decides how to implement this operation, which should be invoked at the end of each execution round |
|  | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{Matches}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm), [ISubstitution](Interfaces-Module#ISubstitution) seq | Match an infon to messages in the mailbox. It returns a subset of (possibly specialized) substitutions. |
>{[Back to top](#top)}>
----
## {anchor:IRouter} {{IRouter}} Type
IRouter provides an interface for communication with the environment.  Implementations of IRouter handle both incoming and outcoming messages

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{Start}} |  | Must be called to initialize the router |
|  | unit | {{Stop}} |  | Must be called after the router is no longer needed |
|  | unit | {{Receive}} | [ITerm](Interfaces-Module#ITerm) -> [ITerm](Interfaces-Module#ITerm) -> unit |  |
|  | unit | {{Send}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm) | Sends an infon ITerm to the target principal ITerm |

### Properties
|| Return type || Property name || Description ||
| string | {{Me}} | Returns the current principal name |
| string list | {{Roster}} | Returns the names of the known principals |
>{[Back to top](#top)}>
----
## {anchor:ISignatureProvider} {{ISignatureProvider}} Type
ISignatureProvider provides an interface for implementations to construct and verify signed infons

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{ConstructSignature}} | [ITerm](Interfaces-Module#ITerm), string | Construct a signature for the given ITerm |
|  | bool | {{CheckSignature}} | [ITerm](Interfaces-Module#ITerm), string, int | Checks if the given signature is correct |
>{[Back to top](#top)}>
----
## {anchor:ILogicEngine} {{ILogicEngine}} Type
ILogicEngine provides an interface for logic reasoning engines that handle the infostrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{Start}} |  | Must be called to initialize the engine |
|  | unit | {{Stop}} |  | Must be called after the engine is no longer needed |
|  | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{Derive}} | [ITerm](Interfaces-Module#ITerm), [ISubstitution](Interfaces-Module#ISubstitution) seq | Given an infon ITerm with (possibly) free variables and an initial sequence of substitutions it returns all those (possibly specialized) substitutions that make the infon hold |
|  | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{DeriveJustification}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm), [ISubstitution](Interfaces-Module#ISubstitution) seq | Constructs evidence for the given infon ITerm that matches the given  proofTemplate, if possible. Works under the given substitutions, returning more concrete ones (to instantiate the proofTemplate when successfull) |
|  | [ITerm](Interfaces-Module#ITerm) option | {{CheckJustification}} | [ITerm](Interfaces-Module#ITerm) | Checks if the given evidence is a well-formed justification, if it succeeds it returns the infon that is justified by the evidence; it it does not  suceed it returns None |
|  | bool | {{Complete}} | [ITerm](Interfaces-Module#ITerm) | Learns that the knowledge learnt so far for a given relation (wrapped as an infon) is complete (closed) |

### Properties
|| Return type || Property name || Description ||
| [IInfostrate](Interfaces-Module#IInfostrate) | {{Infostrate}} | The knowledge source for the logic engine |
| [ISignatureProvider](Interfaces-Module#ISignatureProvider) | {{SignatureProvider}} | The signature checking implementation for this logic engine |
>{[Back to top](#top)}>
----
## {anchor:IExecutor} {{IExecutor}} Type
IExecutor provides an interface for the engine driver. IExecutor  implementations are in charge of keeping a policy and apply all the rules as their conditions are triggered

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{Start}} |  | Must be called to initialize the executor |
|  | unit | {{Stop}} |  | Must be called when the executor is no longer needed |
|  | bool | {{InstallRule}} | [ITerm](Interfaces-Module#ITerm) | Installs the given rule ITerm on the executor. The rule is followed from  this point onwards. Returns false if the rule was already installed |
|  | bool | {{UninstallRule}} | [ITerm](Interfaces-Module#ITerm) | Uninstalls the given rule ITerm on the executor. Returns true if the rule  was not present |
|  | unit | {{FixedPointCallback}} | unit -> unit |  |
|  | unit | {{WakeUpCallback}} | unit -> unit |  |
|  | unit | {{ActionCallback}} | [ITerm](Interfaces-Module#ITerm) -> unit |  |
|  | unit | {{ReceiveCallback}} | [ITerm](Interfaces-Module#ITerm) -> [ITerm](Interfaces-Module#ITerm) -> unit |  |
|  | unit | {{RoundStartCallback}} | unit -> unit |  |
>{[Back to top](#top)}>
----
## {anchor:IParsingContext} {{IParsingContext}} Type
IParsingContext provides an interface for contexts that keep information necessary during the parsing process such as variable types, macro  definitions, etc.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{HasVariable}} | string | Returns true if there is a variable in the context with the given name |
|  | [IType](Interfaces-Module#IType) | {{VariableType}} | string | For a variable name in the context, it returns its type |
|  | unit | {{AddTypeRename}} | string, [IType](Interfaces-Module#IType) | Adds a type rename by giving a newTypeName to a given targetType |
|  | [IType](Interfaces-Module#IType) | {{TypeFromName}} | string | Given a type fullname it returns the AST type for it |
|  | bool | {{HasMacro}} | string | Returns true iff the contexts has a macro definition with the given name |
|  | unit | {{AddMacro}} | string, [IType](Interfaces-Module#IType), [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm), [IVar](Interfaces-Module#IVar) list | Given a macro name, a return type, a body and arguments, it incorporates these as a new macro in the context. |
|  | [IVar](Interfaces-Module#IVar) list | {{GetMacroArgs}} | string | Given a macro name in the context it returns the macro arguments |
|  | [IType](Interfaces-Module#IType) | {{GetMacroRetType}} | string | Given a macro name in the context it returns the macro return type |
|  | [ITerm](Interfaces-Module#ITerm) * [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) | {{ApplyMacro}} | string, [ITerm](Interfaces-Module#ITerm) list | Given a macro name and concrete arguments, it returns a tuple (t, sqt) where t is the element that should be inlined (the macro return variable) and sqt is the macro body with the concrete arguments (which should be  incorporated as an asInfon(sqt) somewhere before or after t is used) |
|  | [IVar](Interfaces-Module#IVar) | {{FreshVar}} | [IType](Interfaces-Module#IType) | Given a type it returns a fresh variable of that type |

### Properties
|| Return type || Property name || Description ||
| string | {{Me}} | Name of the principal for which the policy is being parsed |
>{[Back to top](#top)}>
----
## {anchor:ISubstrateParser} {{ISubstrateParser}} Type
ISubstrateParser provides an interface for substrate parsers that interpret  substrate queries and update terms

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{SetParsingContext}} | [IParsingContext](Interfaces-Module#IParsingContext) | Sets the parsing context (that contains type information and macros among  others) for this parser |
|  | unit | {{SetNamespace}} | string | Sets the namespace to be used in all the ISubstrateTerms produced by this parser |
|  | unit | {{SetSubstrate}} | [ISubstrate](Interfaces-Module#ISubstrate) | Sets the ISubstrate implementation that will be used to interpret the  ISubstrateTerms produced by this parser. This ISubstrate may be used to obtain extra information during parsing (for instance, the SQL substrate parser uses a SQL ISubstrate implementation to get type information for  the columns mentioned in the queries and updates) |
|  | [ISubstrateTerm](Interfaces-Module#ISubstrateTerm) | {{ParseTerm}} | string | It produces an ISubstrateTerm from the given string representation |
>{[Back to top](#top)}>
----
## {anchor:ISubstratePrettyPrinter} {{ISubstratePrettyPrinter}} Type
ISubstratePrettyPrinter provides an interface to print substrate AST elements

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{PrintTerm}} | [ISubstrateTerm](Interfaces-Module#ISubstrateTerm) | Returns a string representation for the given ISubstrateTerm |
>{[Back to top](#top)}>
----
## {anchor:ITranslatedExpr} {{ITranslatedExpr}} Type
Wraps around a generic translated expression

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | Object | {{getUnderlyingExpr}} |  | Gets the underlying expression which is completely domain related |
>{[Back to top](#top)}>
----
## {anchor:ITranslator} {{ITranslator}} Type
ITranslator provides a mechanism for translating ITerm elements to any other expression

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITranslatedExpr](Interfaces-Module#ITranslatedExpr) | {{translate}} | [ITerm](Interfaces-Module#ITerm) | Translates an ITerm to an ITranslatedExpr |
>{[Back to top](#top)}>
----
>{Automatically generated on 7/17/2013 12:17:16 PM}>
