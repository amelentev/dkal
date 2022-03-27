# {anchor:top} {{Executor.Simple}} Module
----
This module defines the following types:
* [ConsistencyChecker](Executor.Simple-Module#ConsistencyChecker)
* [SimpleExecutor](Executor.Simple-Module#SimpleExecutor)
----
## {anchor:ConsistencyChecker} {{ConsistencyChecker}} Type
Provides functionality to check if a set of action ITerms is consistent or not. A set of action ITerms is consistent when all the actions in the set can be applied safely in any order without changing the overall result

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | bool | {{AreConsistentActions}} | [ITerm](Interfaces-Module#ITerm) list | Checks that the given list of actions is consistent |
>{[Back to top](#top)}>
----
## {anchor:SimpleExecutor} {{SimpleExecutor}} Type
The SimpleExecutor runs an endless loop. On each iteration, every rule is  processed to see if it has to be applied. All necessary changes are saved until the end of the iteration. If the set of changes is consistent, they all get applied and a new iteration starts.

### Implemented Interfaces
* IExecutor
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
