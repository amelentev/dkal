# {anchor:top} {{Router.Simple}} Module
----
This module defines the following types:
* [ServiceAddress](Router.Simple-Module#ServiceAddress)
* [SimpleRoutingTable](Router.Simple-Module#SimpleRoutingTable)
* [IPrincipalService](Router.Simple-Module#IPrincipalService)
* [PrincipalService](Router.Simple-Module#PrincipalService)
* [ConnectionsHandler](Router.Simple-Module#ConnectionsHandler)
* [SimpleRouter](Router.Simple-Module#SimpleRouter)
----
## {anchor:ServiceAddress} {{ServiceAddress}} Type
An IPrincipalAddress implementation that uses strings to hold the address of the principal service. The string contains the URL of the listening side of the web-service running the principal service

### Implemented Interfaces
* IEquatable<ServiceAddress>
* IStructuralEquatable
* IComparable<ServiceAddress>
* IComparable
* IStructuralComparable
* IPrincipalAddress

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{CompareTo}} | [ServiceAddress](Router.Simple-Module#ServiceAddress) |  |
|  | int | {{CompareTo}} | Object |  |
|  | int | {{CompareTo}} | Object, IComparer |  |
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | [ServiceAddress](Router.Simple-Module#ServiceAddress) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Location}} |  |
>{[Back to top](#top)}>
----
## {anchor:SimpleRoutingTable} {{SimpleRoutingTable}} Type
A SimpleRoutingTable contains information about the current principal's name and address as well as the names and addresses of all the other principals known

### Implemented Interfaces
* IRoutingTable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [SimpleRoutingTable](Router.Simple-Module#SimpleRoutingTable) | {{FromXml}} | string | Constructs a RoutingTable from an XML file |
>{[Back to top](#top)}>
----
## {anchor:IPrincipalService} {{IPrincipalService}} Type
IPrincipalService provides an interface for exposing an inbox web service that other principals can use to leave us messages.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{ReceiveMessage}} | string, string, string | This operation is invoked by other principals when the want to leave us a message. |
>{[Back to top](#top)}>
----
## {anchor:PrincipalService} {{PrincipalService}} Type
PrincipalService implements the IPrincipalService interface and is  initialized with a callback messageProcessingFunc that is invoked every time a new message arrives.

### Implemented Interfaces
* IPrincipalService
>{[Back to top](#top)}>
----
## {anchor:ConnectionsHandler} {{ConnectionsHandler}} Type
A ConnectionHandler is responsible for creating and keeping connections to every other principal that is known. It's also in charge of keeping a running IPrincipalService to receive message from the outside

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{StartServer}} |  | Initializes the server-side host |
|  | unit | {{StartClients}} |  | Initializes the client-side channels (and factories) |
|  | unit | {{StopServer}} |  | Stops the server-side host |
|  | unit | {{StopClients}} |  | Stops the client-side channels |
|  | unit | {{Send}} | string, string | Sends a message to the given principal by invoking the proper channel |
>{[Back to top](#top)}>
----
## {anchor:SimpleRouter} {{SimpleRouter}} Type
The SimpleRouter provides a IRouter interface by means of web services. A RoutingTable is constructed by reading the XML from the given routingFile. A ConnectionsHandler instance is used to do the actual sending and receiving of messages. Infon ITerms are serialized and deserialized using the given IParser and IPrinter implementations.

### Implemented Interfaces
* IRouter
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
