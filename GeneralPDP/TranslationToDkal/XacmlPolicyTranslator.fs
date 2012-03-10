namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.Simplifier

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util

open XacmlToExps
open XacmlToDisjointExps
open ExpressionTranslator

open Option
open System.Collections.Generic

module XacmlPolicyTranslator =

  type XacmlPolicyTranslator (sender: string, receiver: string, pctx: ParsingCtx) =

    let translator = ExpressionTranslator(pctx)
   
    let unfold = List.reduceBack (fun t1 t2 -> App(pctx.LookupFunction("and"), [t1; t2])) 
    
    let simplifyConditions cdps = 
        let simplifiedConditionPairs = List.map (fun (c,d) -> (simplifyExp c, d)) cdps
        let filteredConditions = List.filter (fun (c,_) -> c <> (ValueExp(BoolAtomValue false))) simplifiedConditionPairs
        filteredConditions

    member private this.BuildPolicyCommRule (condition: Expression, decision: Decision) =
      let translator = ExpressionTranslator(pctx)
      let reqId = Var(pctx.MakeVar "REQ" Type.Int)
      let pep = Var(pctx.MakeVar "PEP" Type.Principal)
      let conditions = match condition with
                       | ApplyExp("and", es) -> es
                       | c -> [c]
      let conditions' = List.map translator.TranslateExpression conditions
      let asInfons = List.map translator.AsInfon conditions'
      let attributes = translator.AttributeInfons reqId pep condition
      let reqPresent = translator.RequestArrivedInfon reqId pep 
      let trigger = [reqPresent] @ attributes @ asInfons
      let decisionInfon = translator.DecisionInfon reqId pep decision
      SendTo {ai = {origin = fakePos; principal = {internal_id= 0; name= sender; typ= Type.Principal}};
              target = Const(Principal({internal_id= 0; name= receiver; typ= Type.Principal}));
              message = decisionInfon;
              proviso = Term.Empty;
              trigger = trigger |> unfold;
              certified = CertifiedSay}

    member private this.TranslateCondition (condition: Expression, decision: Decision) = 
      let reqId = Var(pctx.MakeVar "REQ" Type.Int)
      let pep = Var(pctx.MakeVar "PEP" Type.Principal)
      let pap = Const(Principal(pctx.LookupOrAddPrincipal(receiver)))

      let conditions = match condition with
                       | ApplyExp("and", es) -> es
                       | c -> [c]
      let conditions' = List.map translator.TranslateExpression conditions
      let asInfons = List.map translator.AsInfon conditions'
      let attributes = translator.AttributeInfons reqId pep condition
      let reqPresent = translator.RequestArrivedInfon reqId pep
      
      let guard = unfold ([reqPresent] @ attributes @ asInfons)
      let decisionInfon = translator.DecisionInfon reqId pep decision
      let impliedDecision = App(pctx.LookupFunction("said"), [pap; decisionInfon])

      App(pctx.LookupFunction("follows"), [guard; impliedDecision])

    member this.TranslatePolicyToInfons (policy: Policy) =
      let cdPairs = policyToDisjointCondDecisionPairs policy |> simplifyConditions
      List.map this.TranslateCondition cdPairs

    member this.TranslatePolicyToCommRules (policy: Policy) = 
      let cdPairs = policyToCondDecisionPairs policy |> simplifyConditions
      let _, trustAssertions = xacmlBasicTrustingCtx sender receiver
      let commRules = List.map this.BuildPolicyCommRule cdPairs
      // Add default last NotApplicable comm rule
      let naCommRule = this.BuildPolicyCommRule (ValueExp(BoolAtomValue true), NotApplicable)
      DkalPolicy(policy.PolicyId, trustAssertions @ commRules @ [naCommRule])


      