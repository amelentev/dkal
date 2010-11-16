namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalPolicyTranslator
open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalRequestTranslator
open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalResponseTranslator
open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalTermTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.XacmlResponseTranslator
open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast
open Basics
open Message
open EndPoint
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module DkalToXacmlEndPoint = 

  type DkalToXacmlEndPoint(id: EndPointId, xacmlId: EndPointId, dkalId: EndPointId, 
                           ?pctx: ParsingCtx, ?dkalPolicy: DkalPolicy) =
    inherit EndPoint(id)

    let dkalRequestTranslator = DkalRequestTranslator()
    let dkalResponseTranslator = DkalResponseTranslator()
    let pctx = match pctx with 
               | None -> let pctx, _ = xacmlAwareParsingCtx(id)
                         pctx
               | Some pctx -> pctx
    let ppalMe = pctx.LookupOrAddPrincipal(id)
    let xacmlResponseTranslator = XacmlResponseTranslator(pctx)

    let pendingResponses = new Queue<Infon * EndPointId * int>()

    override ep.Process (m: IMessage) = 
      match m.Content with
      | XacmlResponseContent(resp) ->
              if pendingResponses.Count > 0 then
                let (reqInfon, reqSender, reqId) = pendingResponses.Dequeue()
                // forward definite answer
                if resp.Decision = Permit || resp.Decision = Deny then
                  let dkalResponse = xacmlResponseTranslator.TranslateResponse reqId reqSender resp
                  ep.Send({sender= id;
                            receiver= reqSender;
                            content= InfonContent(App(pctx.LookupFunction("said"), [Const(Principal(ppalMe)); dkalResponse]))})
                else
                  // ask dkal if no definite answer from XACML
//                  let signature = App(pctx.LookupFunction("Ev.signedBy"), 
//                                    [Const(Principal(pctx.LookupOrAddPrincipal(id))); reqInfon; Const(Int(42))])
//                  let justified = App(pctx.LookupFunction("justified"), [reqInfon; signature]) 
                  ep.Send({sender= id;
                            receiver= dkalId;
                            content= InfonContent(reqInfon)})
              else
                ep.Fail "I received an XACML response but no one had asked a request"
      | InfonContent(infon) ->
              if m.Sender = dkalId then
                  // infon comes from my dkal engine, it must be a response
                  try
                    let reqPpal, resp = dkalResponseTranslator.TranslateResponse(infon)
                    let strippedResponseInfon = (DkalTermTranslator()).StripSignatures(infon)
                    let impostedInfon = match strippedResponseInfon with
                                        | App(f, [_; d]) when f.name = "said" -> App(f, [Const(Principal(ppalMe)); d])
                                        | _ -> failwith "expecting 'said' in response infon"
                    ep.Send({sender= id;
                              receiver= reqPpal;
                              content= InfonContent(impostedInfon)})
                  with 
                  | DkalResponseTranslatorException(e) -> printfn "%O" e
              else
                  // infon comes from somewhere else, it must be a request (or something else)
                  try
                    let reqId, pep, req, unusedInfons = dkalRequestTranslator.TranslateRequest(infon)
                    pendingResponses.Enqueue((infon, m.Sender, reqId))
                    ep.Send({sender= id;
                             receiver= xacmlId;
                             content= XacmlRequestContent(req)})
                    // inform of unused infons
                    for inf in unusedInfons do
                      printfn "Unused infon when translating request: %O" (inf.ToSX())
                  with 
                  | DkalRequestTranslatorException(e) -> printfn "%O" e
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.CleanUp() = ()

    override ep.StartUp() = 
      match dkalPolicy with
      | None -> ()
      | Some pcy -> 
          let tr = DkalPolicyTranslator()
          let xacmlPolicy, unusedAssertions = tr.TranslateCommRules(pcy)
          // send policy to xacml 
          ep.Send({sender= id;
                    receiver= xacmlId;
                    content= XacmlPolicyContent(xacmlPolicy)})
          if not unusedAssertions.IsEmpty then
            printfn "there are unused assertions after translation"
            // send unused assertions (if any) to dkal
            (*ep.Send({sender= id;
                      receiver= dkalId;
                      content= DkalAssertionsContent(unusedAssertions)})*)


    override ep.Image = Some (image (ep :> IEndPoint).Color EmptyDrawing)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description
      
    override ep.Description = ep.Id + ": DKAL->XACML"
