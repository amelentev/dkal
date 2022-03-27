# {anchor:top} {{Executor.Factories}} Module
----
This module defines the following types:
* [ExecutorFactory](Executor.Factories-Module#ExecutorFactory)
----
## {anchor:ExecutorFactory} {{ExecutorFactory}} Type
The ExecutorFactory provides a factory to construct different executors.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [IExecutor](Interfaces-Module#IExecutor) | {{Executor}} | string, [IRouter](Interfaces-Module#IRouter), [ILogicEngine](Interfaces-Module#ILogicEngine), [ISignatureProvider](Interfaces-Module#ISignatureProvider), [IInfostrate](Interfaces-Module#IInfostrate), [IMailBox](Interfaces-Module#IMailBox) | Construct an Executor. An executor kind, a router, a logic engine,  an evidence engine, an infostrate and a mailbox must all be provided. |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
