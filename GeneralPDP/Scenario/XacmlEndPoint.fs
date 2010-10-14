namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.PDP.Basics
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Basics
open Message
open EndPoint
open EndPointImageFactory
open RequestInfo

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module XacmlEndPoint =

  type PolicyPointer = PolicyId * EndPointId

  type XacmlEndPoint(id: EndPointId, ?attRepId: EndPointId) = 
    inherit EndPoint(id)

    // decision engine for requests that arrive with no policy pointer
    let mutable localPolicy = Policy("empty-policy", None, "first-applicable", [], [])

    // current requests info
    let servingRequests = new Dictionary<RequestContext, RequestInfo>()

    // policies cache
    let leasedPolicies = new Dictionary<PolicyId * EndPointId, Policy>()

    // local fwd request numbers
    let mutable fwdReqNumber = 5000

    override ep.Process(m: IMessage) = 
      match m.Content with
      | XacmlRequestContent(req) -> 
          servingRequests.[req] <- RequestInfo(req, m.Sender)
          ep.ProcessRequest req
      | XacmlResponseContent(rsp) ->
          // a fwd request came back, see which request initiated it
          let mutable req = None
          for kvp in servingRequests do
            match kvp.Value.PendingResponse with
            | Some (epId, fwdReqNumber) -> 
                  if epId = m.Sender && rsp.Id = fwdReqNumber then
                    req <- Some kvp.Key
            | None -> ()
          match req with 
          | Some req -> 
              let ri = servingRequests.[req]
              ri.PendingResponse <- None
              let rsp = ResponseContext(req.Id, rsp.Decision, rsp.Status, rsp.Obligations)
              ep.SendResponseOrContinue(req, rsp)
          | None -> failwith "A response came for a request I didn't forward"
      | XacmlAttributeResponseContent(arc) ->
          let ri = servingRequests.[arc.Request]
          // if its not expected fail
          match ri.PendingAttribute with
          | None -> failwith "Received an attribute response for an attribute I didn't request"
          | Some((ad, policy)) ->
            // XXX i'm assuming it is the expected attribute, and no other            
            match arc.Attribute with
            // Attribute not found
            | None -> ep.SendMissingAttribute(ad, arc.Request.Id, ri.Requester)
            // Attribute found
            | Some att -> 
                let req = RequestContext(arc.Request.Id, arc.Request.Attributes @ [att])
                servingRequests.Remove(arc.Request) |> ignore
                servingRequests.[req] <- ri
                ri.PendingAttribute <- None
                ep.TryCalculateResponse(req, policy)
      | XacmlPolicyContent(pcy) ->
          // store in cache
          leasedPolicies.[(pcy.PolicyId, m.Sender)] <- pcy
          
          let mutable needToServe = []
          // if it is a policy for pending requests, serve the requests and clear the pending
          for kvp in servingRequests do
            if kvp.Value.PendingPolicy = Some (pcy.PolicyId, m.Sender) then
              needToServe <- needToServe @ [kvp.Key]
              kvp.Value.PendingPolicy <- None
              
          for req in needToServe do
            ep.TryCalculateResponse(req, pcy)

          // otherwise, install as new local policy          
          if needToServe.IsEmpty then
            localPolicy <- pcy      
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail
            
    // try to advance request processing      
    member private ep.ProcessRequest (req: RequestContext) = 
      let ri = servingRequests.[req]
      if ri.PolicyPointers.IsEmpty then
        // apply local policy
        ep.TryCalculateResponse(req, localPolicy)
      elif ri.CurrentIndex < ri.PolicyPointers.Length then
        let policyId, epId = ri.PolicyPointers.[ri.CurrentIndex]
        if policyId <> "" then
          // it is a policyId@policyRep link
          if leasedPolicies.ContainsKey((policyId, epId)) then
            // in cache
            ep.TryCalculateResponse(req, leasedPolicies.[(policyId, epId)])
          else
            // need to fetch
            ri.PendingPolicy <- Some(policyId, epId)
            ep.Send({receiver= epId;
                     sender= id;
                     content= XacmlPolicyRequestContent(PolicyRequestContext(policyId))})
        else
          // it is a pdp link
          // we need to eliminate policy links in the request to prevent infte iteration
          let atts = List.filter (fun (a: Attribute) -> a.Category <> ResourceCategory || a.AttributeId <> "policies") req.Attributes
          ri.PendingResponse <- Some(epId, fwdReqNumber)
          ep.Send({receiver= epId;
                   sender= id;
                   content= XacmlRequestContent(RequestContext(fwdReqNumber, atts))})
          fwdReqNumber <- fwdReqNumber + 1
      else
        failwith "Request information is not consistent"

    member private ep.SendResponseOrContinue (req: RequestContext, response: ResponseContext) = 
      let ri = servingRequests.[req]
      // just applied a single policy
      let mutable answer = None
      if ri.PolicyPointers.Length <= 1 then
          answer <- Some response
      // analizing a (non singleton) list of policies, one did not permit
      elif response.Decision <> Permit then
          answer <- Some (ResponseContext(req.Id, Deny, Status("ok", "", ""), []))
      // analizing the last policy and it permitted
      elif ri.CurrentIndex = ri.PolicyPointers.Length - 1 then
          answer <- Some response
            
      // send answer, if any 
      match answer with
      | None -> 
          // no answer only if we are in the middle of a (so-far successful) policy list evaluation
          ri.CurrentIndex <- ri.CurrentIndex + 1
          ep.ProcessRequest(req)
      | Some response -> 
          ep.Send({sender= id;
                    receiver= ri.Requester;
                    content= XacmlResponseContent(response)})
          servingRequests.Remove(req) |> ignore

        
    member private ep.TryCalculateResponse(req: RequestContext, policy: Policy) = 
      let ri = servingRequests.[req]
      try
        // do we have a definite answer
        let response = (pap policy) req
        ep.SendResponseOrContinue(req, response)
      with 
      | XacmlAttributeMissingException(ad) -> 
          match ad with
          | AttributeDesignatorExp(cat, dt, aid, issuer, mbp) ->
            // discover attribute repository (either statically set or from policy)
            let attRepId = match issuer with
                           | None -> match attRepId with
                                     | None -> None
                                     | Some a -> Some a
                           | Some a -> Some a
            // if there is an attribute repository, ask for the missing attribute
            match attRepId with
            | None -> ep.SendMissingAttribute(ad, req.Id, ri.Requester)
            | Some attRepId ->
                      ep.Send({sender= id;
                               receiver= attRepId;
                               content= XacmlAttributeRequestContent(AttributeRequestContext(ad, req))})
                      ri.PendingAttribute <- Some (ad, policy)
          | _ -> failwith "Expecting attribute designator"

    member private ep.SendMissingAttribute(ad: AttributeDesignator, reqId: int, requester: EndPointId) = 
      match ad with
      | AttributeDesignatorExp(cat, dt, aid, i, mbp) ->
          let msg = "Missing attribute " + cat.ToString() + " " + dt.ToString() + " " + aid
          ep.Send({sender= id;
                   receiver= requester;
                   content= XacmlResponseContent(ResponseContext(reqId, Indeterminate, 
                                                  Status("", msg, ""), []))})
      | _ -> failwith "expecting attribute designator"

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Image = Some (image (ep :> IEndPoint).Color EmptyDrawing)

    override ep.Description = ep.Id + ": XACML"
