# {anchor:top} {{Router}} Module
----
This module defines the following types:
* [IPrincipalAddress](Router-Module#IPrincipalAddress)
* [IRoutingTable](Router-Module#IRoutingTable)
----
## {anchor:IPrincipalAddress} {{IPrincipalAddress}} Type
Empty interface to be used by any type that is used to represent a principal address/location. It can be a URL, a name, etc.
>{[Back to top](#top)}>
----
## {anchor:IRoutingTable} {{IRoutingTable}} Type
Interface implemented by routing tables. A routing table knows the name of  the current principal and his/her address. It also knows how to locate any other principal.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{HasPrincipal}} | string | Returns true only if the given principal name is known |
|  | bool | {{AddPrincipal}} | string, [IPrincipalAddress](Router-Module#IPrincipalAddress) | Adds the given principal and address to the routing table |
|  | [IPrincipalAddress](Router-Module#IPrincipalAddress) | {{PrincipalAddress}} | string | Returns the principal address of the given known principal |

### Properties
|| Return type || Property name || Description ||
| string | {{Me}} | Name of the current principal |
| [IPrincipalAddress](Router-Module#IPrincipalAddress) | {{MyAddress}} | Address of the current princiapl |
| string list | {{Principals}} | List of all kwown principals names |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
