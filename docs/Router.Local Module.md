# {anchor:top} {{Router.Local}} Module
----
This module defines the following types:
* [LocalMailer](Router.Local-Module#LocalMailer)
* [LocalRoutingTable](Router.Local-Module#LocalRoutingTable)
* [LocalRouter](Router.Local-Module#LocalRouter)
----
## {anchor:LocalMailer} {{LocalMailer}} Type
A LocalMailer is a central hub used to communicate several principals that run in the same computer. Each principal suscribes an inbox function, which is invoked by the LocalMailer whenever a message for that principal needs to be delivered

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{AddCallback}} | int, unit -> unit | Adds a callback that will be invoked when the total number of exchanged messages among all principals reaches the specified target amount |
|  | unit | {{SetPrincipalInbox}} | string, [ITerm](Interfaces-Module#ITerm) -> [ITerm](Interfaces-Module#ITerm) -> unit | Adds a principal to the LocalMailer, specifying its callback inbox function that will be invoked passing the message as first argument and the sender principal as second argument |
|  | unit | {{SendMessage}} | [ITerm](Interfaces-Module#ITerm), [ITerm](Interfaces-Module#ITerm), string | Sends the given message from the given principal (second argument) to the  given destination principal (third argument) |

### Properties
|| Return type || Property name || Description ||
| string list | {{Principals}} | Returns the list of known principal names |
>{[Back to top](#top)}>
----
## {anchor:LocalRoutingTable} {{LocalRoutingTable}} Type
A SimpleRoutingTable contains information about the current principal's name and address as well as the names and addresses of all the other principals known

### Implemented Interfaces
* IRoutingTable
>{[Back to top](#top)}>
----
## {anchor:LocalRouter} {{LocalRouter}} Type
The LocalRouter provides a IRouter interface for several principals that run in different threads in the same physical computer. Communication is performed in memory by means of AST (with no serialization)

### Implemented Interfaces
* IRouter

### Properties
|| Return type || Property name || Description ||
| [LocalMailer](Router.Local-Module#LocalMailer) | {{LocalMailer}} | Returns the LocalMailer on which this LocalRouter is suscribed |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
