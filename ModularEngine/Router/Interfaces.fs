namespace Microsoft.Research.Dkal.Router

type IPrincipalAddress = interface end

type IRoutingTable =
  abstract member Me: string
  abstract member MyAddress: IPrincipalAddress
  abstract member Principals: string list
  abstract member PrincipalAddress: ppal: string -> IPrincipalAddress

