namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalPolicyTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.XacmlPolicyTranslator
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory
open Basics
open Message
open EndPoint
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module PolicyRepositoryEndPoint =

  type PolicyRepositoryEndPoint(id: EndPointId, xacmlPolicies: Policy list, dkalPolicies: DkalPolicy list) = 
    inherit EndPoint(id)

    let d2xTranslator = DkalPolicyTranslator()
    let pctx, _ = xacmlAwareParsingCtx id
    let x2dTranslator = XacmlPolicyTranslator("A","B", pctx) // TODO A & B should go

    let emptyXacmlPolicy pid = Policy(pid, None, "first-applicable", [], [])
    let emptyDkalPolicy pid = DkalPolicy(pid, [])

    let xPoliciesDict = new Dictionary<PolicyId, Policy>()
    let dPoliciesDict = new Dictionary<PolicyId, DkalPolicy>()
    do
      for policy in xacmlPolicies do
        xPoliciesDict.Add(policy.PolicyId, policy)
      for policy in dkalPolicies do
        dPoliciesDict.Add(policy.PolicyId, policy)

    override ep.Process(m: IMessage) = 
      match m.Content with
      | XacmlPolicyRequestContent(pr) -> 
          // first try dkal
          if dPoliciesDict.ContainsKey(pr.PolicyId) then
            let pcy, unusedAssertions = d2xTranslator.TranslateCommRules(dPoliciesDict.[pr.PolicyId])
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= XacmlPolicyContent(pcy)})

          // otherwise, xacml
          elif xPoliciesDict.ContainsKey(pr.PolicyId) then
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= XacmlPolicyContent(xPoliciesDict.[pr.PolicyId])})

          // otherwise, send an empty policy
          else
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= XacmlPolicyContent(emptyXacmlPolicy pr.PolicyId)})
      | DkalPolicyRequestContent(dpr) ->
          // first try dkal
          if dPoliciesDict.ContainsKey(dpr.PolicyId) then
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= DkalPolicyContent(dPoliciesDict.[dpr.PolicyId])})
          
          // otherwise, xacml
          elif xPoliciesDict.ContainsKey(dpr.PolicyId) then
            let pcy = x2dTranslator.TranslatePolicyToCommRules(xPoliciesDict.[dpr.PolicyId])
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= DkalPolicyContent(pcy)})

          // otherwise, send an empty policy
          else
            ep.Send({sender= id;
                     receiver= m.Sender;
                     content= DkalPolicyContent(emptyDkalPolicy dpr.PolicyId)})

      | XacmlPolicyContent(pcy) ->
          xPoliciesDict.Add(pcy.PolicyId, pcy)
      | DkalPolicyContent(pcy) ->
          dPoliciesDict.Add(pcy.PolicyId, pcy)
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.Image = Some (image (ep :> IEndPoint).Color DatabaseDrawing)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Description = ep.Id + ": PcyRepository"
