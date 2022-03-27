# {anchor:top} {{Substrate.Xml}} Module
----
This module defines the following types:
* [XmlSubstrateQueryTerm](Substrate.Xml-Module#XmlSubstrateQueryTerm)
* [XmlSubstrate](Substrate.Xml-Module#XmlSubstrate)
----
## {anchor:XmlSubstrateQueryTerm} {{XmlSubstrateQueryTerm}} Type
An XPath expression used to query XmlSubstrates

### Implemented Interfaces
* ISubstrateQueryTerm
* ISubstrateTerm
* ITerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |
|  | string | {{ToString}} |  |  |

### Properties
|| Return type || Property name || Description ||
| string | {{XPath}} | XPath expression with (optional) input variables encoded as "$VARNAME" |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} | Input variables used in the XPath expression |
| IDictionary<string, [ITerm](Interfaces-Module#ITerm)> | {{Output}} | Map from result nodes attribute names (or node name in case of "") to  output ITerms. If the output term is a variable it gets instantiated;  if it is a constant it is compared |
>{[Back to top](#top)}>
----
## {anchor:XmlSubstrate} {{XmlSubstrate}} Type
An XML Substrate is an abstraction of an XML document that can be queried by means of XPath expressions encoded in XmlSubstrateQueryTerms

### Implemented Interfaces
* ISubstrate
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
