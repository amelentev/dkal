namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.Data.Ast
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Microsoft.Research.GeneralPDP.Utils.List
open Basics
open Message
open EndPoint
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module DataStorageEndPoint =

  type DataStorageEndPoint(id: EndPointId) = 
    inherit EndPoint(id)

    let inSyncWith = new HashSet<EndPointId>()

    let labels = new Dictionary<LabelId, LabelInfo>()
    let docs = new Dictionary<DocumentId, DocumentInfo>()

    let mutable localReqNumber = 4000

    let pendingRequests = new Dictionary<int, RequestContext * EndPointId>()

    // indicates if this endpoint has a sharepoint drawing or not
    let mutable sharePoint = false
    member ep.SharePoint with get() = sharePoint and set(b) = sharePoint <- b

    member ep.AddSyncEp (syncedEp: EndPointId) =
      inSyncWith.Add(syncedEp) |> ignore

    member ep.AddLabel (id: LabelId) (info: LabelInfo) =
      labels.Add(id, info)

    member ep.AddDocument (docId: DocumentId) (docInfo: DocumentInfo) (fromSync: bool) =
      // add to local document repository
      docs.Add(docId, docInfo)
      // send to sync (unless it came from sync)
      if not fromSync then
        for syncedEp in inSyncWith do
          ep.Send({sender= id;
                   receiver= syncedEp;
                   content= DocSyncContent(docId, docInfo)})

    member private ep.ConstructRequest (ls: string seq) (req: RequestContext) (sender: EndPointId) = 
      // compute list of policyPointers
      let policyPointers = Seq.map (fun l -> labels.[l].PolicyPointer) ls
      // compute list of possible pdps
      let pdps = elementsInAll (Seq.map (fun l -> labels.[l].Pdps) ls) |> Seq.sort |> Seq.toList
      let newReq = RequestContext(localReqNumber, ep.PrepareNewRequestAttributes req.Attributes policyPointers pdps)
      match pdps with
      | [] -> failwith ("No pdp to ask about request " + req.Id.ToString())
      | pdp :: otherPdps -> 
        // for now just use first one 
        ep.Send({sender= id;
                 receiver= pdp;
                 content= XacmlRequestContent(newReq)})
      pendingRequests.[localReqNumber] <- (req, sender)
      localReqNumber <- localReqNumber + 1

    override ep.Process(m: IMessage) = 
      match m.Content with
      | DocSyncContent(docId, docInfo) ->
        if inSyncWith.Contains(m.Sender) then
          ep.AddDocument docId docInfo true
        else
          failwith ("Attempt to sync from untrusted source: " + m.Sender)
      | XacmlRequestContent(req) ->
        match req.DocId(), req.Action() with
        | Some docId, Some "write" ->
          let ls = req.DocLabels()
          ep.ConstructRequest ls req m.Sender
        | Some docId, Some "read" when docs.ContainsKey docId ->
          if docs.ContainsKey docId then
            ep.ConstructRequest docs.[docId].Labels req m.Sender
          else
            let msg = "document-id does not match any stored document"
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= XacmlResponseContent(ResponseContext(req.Id, Indeterminate, Status("", msg, ""), []))})
        | _, _ -> 
          let msg = "document-id or action not recognized"
          ep.Send({sender= id;
                   receiver= m.Sender;
                   content= XacmlResponseContent(ResponseContext(req.Id, Indeterminate, Status("", msg, ""), []))})
      | XacmlResponseContent(rsp) ->
          let origReq, requester = pendingRequests.[rsp.Id]
          pendingRequests.Remove(rsp.Id) |> ignore
          // perform requested action (and update response if necessary)
          let response = 
            match origReq.DocId(), origReq.Action() with
            | Some documentId, Some "read" ->
              let newStatus = 
                if rsp.Decision = Permit then
                  Status(rsp.Status.Code, docs.[documentId].Content, rsp.Status.Status)
                else
                  rsp.Status
              ResponseContext(origReq.Id, rsp.Decision, newStatus, rsp.Obligations)
            | Some documentId, Some "write" -> 
              if rsp.Decision = Permit then
                let ls = origReq.DocLabels()
                for l in ls do
                  if not (labels.ContainsKey l) then
                    failwith "Adding content with an unknown label"
                let docInfo = DocumentInfo(origReq.DocContent(), ls)
                ep.AddDocument documentId docInfo false
              ResponseContext(origReq.Id, rsp.Decision, rsp.Status, rsp.Obligations)
            | d, a -> failwith ("impossible: " + d.ToString() + " " + a.ToString()) // validation is at request side
          ep.Send({sender= id;
                   receiver= requester;
                   content= XacmlResponseContent(response)})
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    member private ep.PrepareNewRequestAttributes (oldAtts: Attribute list) (policyPointers: PolicyPointer seq) (pdps: EndPointId seq) =
      let mutable atts = []
      
      //// add pdps 
      //let pdps = Seq.sort pdps
      //let pdpAtts = Seq.map (fun pdp -> Attribute(ResourceCategory, "decision-point", StringAtomValue(pdp))) pdps |> Seq.toList
      //atts <- atts @ pdpAtts 
      
      // add policyPointers
      let policyPointersAttValue = String.concat "," (Seq.map (fun pp -> pp.ToString()) policyPointers) 
      let policyPointersAtt = Attribute(ResourceCategory, "policies", StringAtomValue policyPointersAttValue)
      atts <- atts @ [policyPointersAtt]
      // remove unncessary atts from original request
      for att in oldAtts do
        if att.Category = ResourceCategory && att.AttributeId = "document-id" then
          () // drop
        else
          atts <- atts @ [att]
      atts

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.Image = 
      let d = if sharePoint then SharePointDrawing else FirewallDrawing
      Some (image (ep :> IEndPoint).Color d)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Description = ep.Id + ": DataStorage"
