namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Basics
open Message
open EndPoint
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module EnforcementEndPoint =

  type EnforcementEndPoint(id: EndPointId) = 
    inherit EndPoint(id)

    let mutable localReqNumber = 9000

    let pendingRequests = new Dictionary<int, int * EndPointId * EndPointId list>()

    // indicates if this endpoint has a sharepoint drawing or not
    let mutable sharePoint = false
    member ep.SharePoint with get() = sharePoint and set(b) = sharePoint <- b

    override ep.Process(m: IMessage) = 
      match m.Content with
      | XacmlRequestContent(req) ->
          let dps = req.DecisionPoints()
          match dps with
          | [] ->
            // No PDP indicated
            let msg = "Expecting at least one resource decision-point attribute"
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= XacmlResponseContent(ResponseContext(req.Id, Indeterminate, Status("", msg, ""), []))})
          | dp :: dps ->
            // For now, we just use the first PDP (but store the rest for possible future use)
            ep.Send({sender= id;
                     receiver= dp
                     content= XacmlRequestContent(RequestContext(localReqNumber, req.Attributes))})
            pendingRequests.[localReqNumber] <- (req.Id, m.Sender, dps)
            localReqNumber <- localReqNumber + 1
      | XacmlResponseContent(rsp) ->
          let reqId, requester, _ = pendingRequests.[rsp.Id]
          pendingRequests.Remove(rsp.Id) |> ignore
          ep.Send({sender= id;
                   receiver= requester;
                   content= XacmlResponseContent(ResponseContext(reqId, rsp.Decision, rsp.Status, rsp.Obligations))})
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.Image = 
      let d = if sharePoint then SharePointDrawing else FirewallDrawing
      Some (image (ep :> IEndPoint).Color d)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Description = ep.Id + ": Enforcement"
