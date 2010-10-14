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

module AdjudicationEndPoint =

  type AdjudicationEndPoint(id: EndPointId) = 
    inherit EndPoint(id)

    let mutable actualPdps = []
    let mutable localReqId = 1000

    let pendingRequests = new Dictionary<int, RequestContext * EndPointId * EndPointId list>()

    member ep.AddDecisionPoint(dpId: EndPointId) =
      actualPdps <- actualPdps @ [dpId]

    override ep.Process(m: IMessage) = 
      match m.Content with
      | XacmlRequestContent(req) ->
          match actualPdps with
          | [] ->
            // No actual PDP
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= XacmlResponseContent(ResponseContext(req.Id, NotApplicable, Status("ok", "", ""), []))})
          | pdp :: pdps ->
            // We try the first PDP (and store the rest in case answer is not-applicable)
            // we need to use a unique (local) req identifier
            let oldReq = req
            let req = RequestContext(localReqId, req.Attributes)
            ep.Send({sender= id;
                     receiver= pdp;
                     content= XacmlRequestContent(req)})
            pendingRequests.Add(localReqId, (oldReq, m.Sender, pdps))
            localReqId <- localReqId + 1
      | XacmlResponseContent(rsp) ->
          let req, requester, pdps = pendingRequests.[rsp.Id]
          pendingRequests.Remove(rsp.Id) |> ignore
          if rsp.Decision <> NotApplicable || pdps.IsEmpty then
            // go back to original request id and forward definite answer
            ep.Send({sender= id;
                     receiver= requester;
                     content= XacmlResponseContent(ResponseContext(req.Id, rsp.Decision, rsp.Status, rsp.Obligations))})
          else
            // try next pdp
            match pdps with
            | [] -> failwith "Impossible"
            | pdp :: pdps -> 
                  let oldReq = req
                  let req = RequestContext(localReqId, req.Attributes)
                  ep.Send({sender= id;
                     receiver= pdp;
                     content= XacmlRequestContent(req)})
                  pendingRequests.Add(localReqId, (oldReq, requester, pdps))
                  localReqId <- localReqId + 1
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.Image = Some (image (ep :> IEndPoint).Color EmptyDrawing)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Description = ep.Id + ": Adjudication"
