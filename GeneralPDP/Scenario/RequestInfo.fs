namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Microsoft.Research.GeneralPDP.XACML.Ast

module RequestInfo =
  
  type RequestInfo(req: RequestContext, requester: EndPointId) = 
    let policyPointers = req.PolicyPointers()
    let mutable pendingAttribute: (AttributeDesignator * Policy) option = None
    let mutable pendingPolicy: (PolicyId * EndPointId) option = None
    let mutable pendingResponse: (EndPointId * int) option = None
    let mutable currentIndex: int = 0
    member ri.Requester = requester
    member ri.PolicyPointers = policyPointers
    member ri.CurrentIndex with get() = currentIndex and set(c) = currentIndex <- c
    member ri.PendingAttribute with get() = pendingAttribute and set(p) = pendingAttribute <- p
    member ri.PendingPolicy with get() = pendingPolicy and set(p) = pendingPolicy <- p
    member ri.PendingResponse with get() = pendingResponse and set(r) = pendingResponse <- r
