namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.GeneralPDP.XACML.Ast
open XacmlToExps

module XacmlToDisjointExps = 

  let rec policyToDisjointCondDecisionPairs (p: Policy) =

    // TODO: "only-one-applicable" policy combiner is not yet supported
    // TODO add obligations 
    // TODO currently ignoring Issuer and MustBePresent keys
    let addTarget conds targ = 
      let targCond = targetToExps targ
      List.map (fun c -> ApplyExp("and", [targCond; c])) conds
    let addNotTarget conds targ = 
      let notTargCond = ApplyExp("not", [targetToExps targ])
      List.map (fun c -> ApplyExp("or", [notTargCond; c])) conds
    let pair (d: Decision) (c: Expression) = (c, d)
    let permitDenyNotApplicableConditions cdPairs = 
      let first (a,_) = a
      let pcs = List.filter (fun (c,d) -> d = Permit) cdPairs
      let dcs = List.filter (fun (c,d) -> d = Deny) cdPairs
      let nacs = List.filter (fun (c,d) -> d = NotApplicable) cdPairs
      (List.map first pcs, List.map first dcs, List.map first nacs)

    match p with
    | Policy(_, targ, rcid, rs, os) -> 
      let permitRules rs = List.filter (fun (r: Rule) -> r.Decision = Permit) rs
      let denyRules rs = List.filter (fun (r: Rule) -> r.Decision = Deny) rs
      match rcid with
      | "permit-overrides" -> 
        // permit whenever a permit rule is triggered
        let permitConditions = List.map ruleToExps (permitRules rs)

        // deny whenever no permit rule is triggered and a deny rule is triggered
        let noPermitCondition = ApplyExp("not", [ApplyExp("or", permitConditions)])
        let denyConditions = List.map (fun r -> ApplyExp("and", [noPermitCondition; ruleToExps r])) (denyRules rs)
          
        // not applicable in any other case
        let noDenyCondition = ApplyExp("not", [ApplyExp("or", List.map ruleToExps (denyRules rs))])
        let notApplicableConditions = [ApplyExp("and", [noPermitCondition; noDenyCondition])]

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // if target is not satisfied then return notApplicable
        let notApplicableConditions = addNotTarget notApplicableConditions targ
        List.map (pair Permit) permitConditions @  List.map (pair Deny) denyConditions @ List.map (pair NotApplicable) notApplicableConditions

      | "deny-overrides" -> 
        // deny whenever a deny rule is triggered
        let denyConditions = List.map ruleToExps (denyRules rs)

        // permit whenever no deny rule is triggered and a permit rule is triggered
        let noDenyCondition = ApplyExp("not", [ApplyExp("or", denyConditions)])
        let permitConditions = List.map (fun r -> ApplyExp("and", [noDenyCondition; ruleToExps r])) (permitRules rs)

        // not applicable in any other case
        let noPermitCondition = ApplyExp("not", [ApplyExp("or", List.map ruleToExps (permitRules rs))])
        let notApplicableConditions = [ApplyExp("and", [noDenyCondition; noPermitCondition])]

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // if target is not satisfied then return notApplicable
        let notApplicableConditions = addNotTarget notApplicableConditions targ
        List.map (pair Permit) permitConditions @  List.map (pair Deny) denyConditions @ List.map (pair NotApplicable) notApplicableConditions

      | "first-applicable" ->
        // construct permit and deny conditions by accumulating a path condition
        let mutable permitConditions = []
        let mutable denyConditions = []
        let mutable conditionSoFar = ValueExp(BoolAtomValue true)
        for r in rs do
          let ruleCond = ruleToExps r
          let fullCond = ApplyExp("and", [conditionSoFar; ruleCond])
          conditionSoFar <- ApplyExp("and", [conditionSoFar; ApplyExp("not", [ruleCond])])
          if r.Decision = Permit then
            permitConditions <- permitConditions @ [fullCond]
          elif r.Decision = Deny then
            denyConditions <- denyConditions @ [fullCond]

        // not applicable in any other case
        let notApplicableConditions = [conditionSoFar]

        // rules are already disjoint at this point

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // if target is not satisfied then return notApplicable
        let notApplicableConditions = addNotTarget notApplicableConditions targ
        List.map (pair Permit) permitConditions @  List.map (pair Deny) denyConditions @ List.map (pair NotApplicable) notApplicableConditions
          
      | rc -> failwith ("unknown rule combiner: " + rc)
      
    | PolicySet(_, targ, pcid, ps, os) -> 
      match pcid with
      | "deny-overrides" -> 
        let condDecisionPairs = List.collect policyToDisjointCondDecisionPairs ps
        let permitConditions, denyConditions, _ = permitDenyNotApplicableConditions condDecisionPairs

        // permit if no rule denies and one rule permits          
        let noDenyCondition = ApplyExp("not", [ApplyExp("or", denyConditions)])
        let permitConditions = List.map (fun c -> ApplyExp("and", [noDenyCondition; c])) permitConditions
          
        // ignoring policies notApplicableConditions, and using the fact that !deny && !permit <=> notApplicable
        let noPermitCondition = ApplyExp("not", [ApplyExp("or", permitConditions)])
        let notApplicableConditions = [ApplyExp("and", [noDenyCondition; noPermitCondition])]

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // if target is not satisfied then return notApplicable
        let notApplicableConditions = addNotTarget notApplicableConditions targ
          
        List.map (pair Permit) permitConditions @  List.map (pair Deny) denyConditions @ List.map (pair NotApplicable) notApplicableConditions
 
      | "permit-overrides" -> 
        let condDecisionPairs = List.collect policyToDisjointCondDecisionPairs ps
        let permitConditions, denyConditions, _ = permitDenyNotApplicableConditions condDecisionPairs

        // deny if no rule permits and one rule denies
        let noPermitCondition = ApplyExp("not", [ApplyExp("or", permitConditions)])
        let denyConditions = List.map (fun c -> ApplyExp("and", [noPermitCondition; c])) denyConditions
          
        // ignoring policies notApplicableConditions, and using the fact that !deny && !permit <=> notApplicable
        let noDenyCondition = ApplyExp("not", [ApplyExp("or", denyConditions)])
        let notApplicableConditions = [ApplyExp("and", [noDenyCondition; noPermitCondition])]

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // if target is not satisfied then return notApplicable
        let notApplicableConditions = addNotTarget notApplicableConditions targ
          
        List.map (pair Permit) permitConditions @  List.map (pair Deny) denyConditions @ List.map (pair NotApplicable) notApplicableConditions
   
      | "first-applicable" -> 
        // construct permit and deny conditions by accumulating a path condition
        let mutable permitConditions = []
        let mutable denyConditions = []
        let mutable conditionSoFar = ValueExp(BoolAtomValue true)
        for p in ps do
          let pcs, dcs, nacs = permitDenyNotApplicableConditions (policyToDisjointCondDecisionPairs p)
          // add path condition to each condition from policy p
          for pc in pcs do
            let pc = ApplyExp("and", [conditionSoFar; pc])
            permitConditions <- permitConditions @ [pc]
          for dc in dcs do
            let pc = ApplyExp("and", [conditionSoFar; dc])
            denyConditions <- denyConditions @ [dc]

          conditionSoFar <- ApplyExp("and", [conditionSoFar; ApplyExp("or", nacs)])

        // not applicable in any other case
        let notApplicableConditions = [conditionSoFar]

        // rules are already disjoint at this point

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // if target is not satisfied then return notApplicable
        let notApplicableConditions = addNotTarget notApplicableConditions targ
        List.map (pair Permit) permitConditions @  List.map (pair Deny) denyConditions @ List.map (pair NotApplicable) notApplicableConditions

      | pc -> failwith ("unknown policy combiner: " + pc)
