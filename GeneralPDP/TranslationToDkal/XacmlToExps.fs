namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.GeneralPDP.XACML.Ast

module XacmlToExps = 

  let subjectTargetToExps (st: SubjectTarget) = 
    let translateSubjectMatch (sm: SubjectMatch) =
      ApplyExp(sm.MatchId, [sm.Designator; ValueExp(sm.Value)])
    ApplyExp("and", List.map translateSubjectMatch st.SubjectMatchs)

  let resourceTargetToExps (rt: ResourceTarget) = 
    let translateResourceMatch (rm: ResourceMatch) =
      ApplyExp(rm.MatchId, [rm.Designator; ValueExp(rm.Value)])
    ApplyExp("and", List.map translateResourceMatch rt.ResourceMatchs)

  let actionTargetToExps (at: ActionTarget) = 
    let translateActionMatch (am: ActionMatch) =
      ApplyExp(am.MatchId, [am.Designator; ValueExp(am.Value)])
    ApplyExp("and", List.map translateActionMatch at.ActionMatchs)

  let targetToExps (targ: option<Target>) =
    match targ with
    | None -> ValueExp(BoolAtomValue(true))
    | Some targ -> 
      let subjects = match targ.Subjects with
                      | [] -> ValueExp(BoolAtomValue(true))
                      | ss -> ApplyExp("or", List.map (subjectTargetToExps) ss)
      let resources = match targ.Resources with
                      | [] -> ValueExp(BoolAtomValue(true))
                      | rs -> ApplyExp("or", List.map (resourceTargetToExps) rs)
      let actions = match targ.Actions with
                    | [] -> ValueExp(BoolAtomValue(true))
                    | acs -> ApplyExp("or", List.map (actionTargetToExps) acs)
      ApplyExp("and", [subjects; resources; actions])
   
  let ruleToExps (r: Rule) = 
    let target = targetToExps(r.Target)
    let condition = match r.Condition with
                    | None -> ValueExp(BoolAtomValue(true))
                    | Some c -> c
    ApplyExp("and", [target; condition])
      
  let rec policyToCondDecisionPairs (p: Policy) =

    // TODO: "only-one-applicable" policy combiner is not yet supported
    // TODO add obligations 
    // TODO currently ignoring Issuer and MustBePresent keys
    let addTarget conds targ = 
      let targCond = targetToExps targ
      List.map (fun c -> ApplyExp("and", [targCond; c])) conds
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
      | "permit-overrides" 
      | "deny-overrides" -> 
        let permitConditions = List.map ruleToExps (permitRules rs)
        let denyConditions = List.map ruleToExps (denyRules rs)

        // add the target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // pairs
        let permitPairs = List.map (pair Permit) permitConditions 
        let denyPairs = List.map (pair Deny) denyConditions 

        if rcid = "permit-overrides" then
          permitPairs @ denyPairs
        else
          denyPairs @ permitPairs

      | "first-applicable" ->
        let mutable condDecisionPairs = []
        for r in rs do
          let condWithTarget = ApplyExp("and", [targetToExps targ; ruleToExps r])
          condDecisionPairs <- condDecisionPairs @ [(condWithTarget, r.Decision)]
        condDecisionPairs 

      | rc -> failwith ("unknown rule combiner: " + rc)
      
    | PolicySet(_, targ, pcid, ps, os) -> 
      match pcid with
      | "permit-overrides"
      | "deny-overrides" -> 
        let cdPairs = List.collect policyToCondDecisionPairs ps
        let permitConditions, denyConditions, _ = permitDenyNotApplicableConditions cdPairs

        // add target
        let permitConditions = addTarget permitConditions targ
        let denyConditions = addTarget denyConditions targ

        // pairs
        let permitPairs = List.map (pair Permit) permitConditions 
        let denyPairs = List.map (pair Deny) denyConditions 

        if pcid = "permit-overrides" then
          permitPairs @ denyPairs
        else
          denyPairs @ permitPairs

      | "first-applicable" -> 
        let mutable condDecisionPairs = []
        for p in ps do
          let conds = List.map (fun (c,d) -> (ApplyExp("and", [targetToExps targ; c]),d)) (policyToCondDecisionPairs p)
          condDecisionPairs <- condDecisionPairs @ conds
        condDecisionPairs

      | pc -> failwith ("unknown policy combiner: " + pc)
