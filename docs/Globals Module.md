# {anchor:top} {{Globals}} Module
----
This module defines the following types:
* [SubstrateMap](Globals-Module#SubstrateMap)
----
## {anchor:SubstrateMap} {{SubstrateMap}} Type
Keep a global substrate map that indicates which substrate is serving which namespaces. This is used by the SubstrateDispatcher

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | unit | {{AddSubstrate}} | [ISubstrate](Interfaces-Module#ISubstrate) | Suscribe a substrate to the SubstrateMap |
| {{static}} | [ISubstrate](Interfaces-Module#ISubstrate) | {{GetSubstrate}} | string | Obtain the substrate that serves the given namespace |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
