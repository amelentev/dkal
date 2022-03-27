# {anchor:top} {{Substrate}} Module
----
This module defines the following types:
* [SubstrateDispatcher](Substrate-Module#SubstrateDispatcher)
* [DummySubstrateQueryTerm](Substrate-Module#DummySubstrateQueryTerm)
----
## {anchor:SubstrateDispatcher} {{SubstrateDispatcher}} Type
Dispatch queries to substrate across substrate implementations

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{Solve}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) seq, [ISubstitution](Interfaces-Module#ISubstitution) seq | Solve the given queries, invoking the necessary substrates, considering all  possible substitutions. Return more specialized substitutions (if successful) |
| {{static}} | bool | {{AreConsistentUpdates}} | [ISubstrateUpdateTerm](Interfaces-Module#ISubstrateUpdateTerm) seq | Returns true if the given updates can be consistently applied among  different substrates. It delegates the problem to each substrate |
| {{static}} | bool | {{Update}} | [ISubstrateUpdateTerm](Interfaces-Module#ISubstrateUpdateTerm) seq | Applies all the given updates by delegating them to each responsible substrate. Returns true if at least one change was produced |
>{[Back to top](#top)}>
----
## {anchor:DummySubstrateQueryTerm} {{DummySubstrateQueryTerm}} Type
A DummySubstrateQueryTerm wraps an ITerm and adds a namespace. All the ITerm operations are delegated to the wrapped ITerm. This construction is useful to implement substrate query terms using the same infrastructure used for infon AST elements (such as Ast.Tree Application nodes).

### Implemented Interfaces
* ITerm
* ISubstrateQueryTerm
* ISubstrateTerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{Query}} | The wrapped ITerm containing the actual query |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
