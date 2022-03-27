# {anchor:top} {{Router.Factories}} Module
----
This module defines the following types:
* [RouterFactory](Router.Factories-Module#RouterFactory)
----
## {anchor:RouterFactory} {{RouterFactory}} Type
The RouterFactory provides a factory to construct different routers.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [IRouter](Interfaces-Module#IRouter) | {{Router}} | string, string | Construct a Router. Router kind and routingFile must be provided. |
| {{static}} | Dictionary<string, [IRouter](Interfaces-Module#IRouter)> | {{LocalRouters}} | string list | Construct routers for each of the principals in the list. All the  routers are going to used a central LocalMailer to communicate. This is used in the DkalMulti front end |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
