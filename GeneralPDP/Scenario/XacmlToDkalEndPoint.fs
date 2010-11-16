namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.XacmlPolicyTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.XacmlRequestTranslator
open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalResponseTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.Crawlers
open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine
open Message
open Basics
open EndPoint
open EndPointImageFactory
open RequestInfo

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module XacmlToDkalEndPoint =

  type XacmlToDkalEndPoint(id: EndPointId, dkalId: EndPointId, pctx: ParsingCtx) = 
    inherit EndPoint(id)

    let attributesNeeded = new HashSet<AttributeDesignator>()
    let mutable nextRequestId = 3000
    let pendingRequests = new Dictionary<int, EndPointId * int>()

    let ppalMe = pctx.LookupOrAddPrincipal(id)
    let ppalDkal = pctx.LookupOrAddPrincipal(dkalId)

    // determines if policy translation is done to rules or to infons
    let mutable translateToRules = false
    member ep.TranslateToRules  with get() = translateToRules and set (t) = translateToRules <- t

    override ep.Process(m: IMessage) = 
      match m.Content with
      | XacmlRequestContent(req) -> 
                ep.SendRequestToDkal(nextRequestId, req)
                pendingRequests.[nextRequestId] <- (m.Sender, req.Id)
                nextRequestId <- nextRequestId + 1
      | XacmlPolicyContent(pcy) ->
              attributesNeeded.Clear()
              attributesNeeded.UnionWith(attributesInPolicy pcy)
              ep.InstallPolicyOnDkal(pcy)
      | InfonContent(infon) ->
              let tr = DkalResponseTranslator()
              let pep, response = tr.TranslateResponse(infon)
              let found, t = pendingRequests.TryGetValue(response.Id)
              if pep <> id || not found then
                failwith (ep.Id + ": I received a response from DKAL to a request I didn't make") 
              let reqSender, origId = t in
              pendingRequests.Remove(response.Id) |> ignore
              // adjust local request to original request number
              let response = ResponseContext(origId, response.Decision, response.Status, response.Obligations)
              ep.Send({sender= id;
                       receiver= reqSender;
                       content= XacmlResponseContent (response)})
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.Image = Some (image (ep :> IEndPoint).Color EmptyDrawing)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    member private ep.SendRequestToDkal (reqId: int, req: RequestContext) =
      let tr = XacmlRequestTranslator(pctx)
      let req = RequestContext(reqId, req.Attributes)
      let infon = tr.TranslateRequest id req attributesNeeded
      ep.Send({sender= ep.Id;
               receiver= dkalId;
               content= InfonContent(infon)})

    member private ep.InstallPolicyOnDkal (p: Policy) =
      let tr = XacmlPolicyTranslator(dkalId, id, pctx)
      // translate to rules or infons depending on setting
      if translateToRules then
        let dPolicy = tr.TranslatePolicyToCommRules p
        ep.Send({sender= ep.Id;
                  receiver= dkalId;
                  content= DkalPolicyContent(dPolicy)})
      else
        let infons = tr.TranslatePolicyToInfons p
        for infon in infons do
          let signature = App(pctx.LookupFunction("Ev.signedBy"), [Const(Principal(ppalMe)); infon; Const(Int(42))])
          let justified = App(pctx.LookupFunction("justified"), [infon; signature]) 
          ep.Send({sender= ep.Id;
                   receiver= dkalId;
                   content= InfonContent(justified)})

    override ep.Description = ep.Id + ": XACML->DKAL"
  