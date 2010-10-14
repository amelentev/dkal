namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalTermTranslator
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.Data.Ast
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Msagl.Drawing
open Microsoft.Research.GeneralPDP.Utils.String
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open EndPointImageFactory

open System.Drawing

module Basics =
  
  type EndPointId = string

  type Content = 
  | InfonContent of Infon
  | XacmlRequestContent of RequestContext
  | XacmlResponseContent of ResponseContext
  | XacmlPolicyContent of Policy
  | XacmlPolicyRequestContent of PolicyRequestContext
  | XacmlAttributeRequestContent of AttributeRequestContext
  | XacmlAttributeResponseContent of AttributeResponseContext
  | DkalPolicyContent of DkalPolicy
  | DkalPolicyRequestContent of DkalPolicyRequest
  | DocSyncContent of DocumentId * DocumentInfo
  with 
    override c.ToString() =
      match c with
      | InfonContent(msg) -> 
          let translator = DkalTermTranslator()
          winEndOfLines (translator.StripSignatures(msg).ToSX().ToString()) 
      | XacmlRequestContent(req) -> req.ToString()
      | XacmlResponseContent(resp) -> resp.ToString()
      | XacmlPolicyContent(pcy) -> pcy.ToString()
      | XacmlPolicyRequestContent(pr) -> pr.ToString()
      | XacmlAttributeRequestContent(arc) -> arc.ToString()
      | XacmlAttributeResponseContent(arc) -> arc.ToString()
      | DkalPolicyContent(dp) -> dp.ToString()
      | DkalPolicyRequestContent(dpr) -> dpr.ToString()
      | DocSyncContent(docId, docInfo) -> 
          "Sync of doc " + docId + "\r\n" + docInfo.ToString()
    
    member c.Type() = 
      match c with
      | InfonContent _ -> "Infon"
      | XacmlRequestContent _ -> "XACML Req"
      | XacmlResponseContent _ -> "XACML Rsp"
      | XacmlPolicyContent _ -> "XACML Pcy"
      | XacmlPolicyRequestContent _ -> "XACML Pcy Req"
      | XacmlAttributeRequestContent _ -> "XACML Att Req"
      | XacmlAttributeResponseContent _ -> "XACML Att Rsp"
      | DkalPolicyContent _ -> "DKAL Pcy"
      | DkalPolicyRequestContent _ -> "DKAL Pcy Req"
      | DocSyncContent _ -> "Doc Sync"

    member c.Digest(?size: int) = 
      let size = match size with
                 | None -> 60
                 | Some s -> s
      let complete = c.ToString()
      if complete.Length < size then
        complete
      else
        complete.Substring(0, size) + "..."

        
  type IMessage = 
    abstract Sender: EndPointId
    abstract Receiver: EndPointId
    abstract Content: Content


  and IEndPoint = 
    abstract PutInScenario: IScenario -> unit

    abstract Start: unit -> unit
    abstract CheckPoint: unit -> unit
    abstract Finish: unit -> unit

    abstract Send: IMessage -> unit
    abstract Receive: IMessage -> unit
    abstract Process: IMessage -> unit

    abstract ApplyStyle: Node -> unit
    abstract Image: Image option
    abstract Color: EPColor with get,set

    abstract Id: EndPointId
    abstract Description: string


  and IScenario = 
    abstract Name: string
    abstract AddEndPoint: IEndPoint -> unit
    abstract Route: IMessage -> unit
    
    abstract UnsuscribeViewer: IScenarioViewer -> unit
    abstract SuscribeViewer: IScenarioViewer -> unit


  and IScenarioViewer = 
    abstract AssignScenario: IScenario -> unit    

    abstract NotifyNewEndPoint: IEndPoint -> unit
    abstract NotifyNewMessage: IMessage -> unit


  // for simulation purposes
  type SimulationStep = 
  | MessageStep of IMessage
  | SleepStep of int
