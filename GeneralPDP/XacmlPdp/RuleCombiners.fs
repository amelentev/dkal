namespace Microsoft.Research.GeneralPDP.XACML.PDP

open Microsoft.Research.GeneralPDP.XACML.Ast
open Basics

open System.Collections.Generic

module RuleCombiners = 

  /// An rule combinator environment which stores XACML rule combination algorithms used in policies
  let ruleCombEnv = new Dictionary<string, RuleCombiningAlgF>()

  let rec ruleDenyOverrides (decisions: (Decision * RuleF) list) (req: RequestContext) =
    let check d1 d2 d3 =
      match d1, d2, d3 with
        | _, Permit, NotApplicable -> Permit
        | _, Permit, next -> next
        | _, Deny, _ -> Deny
        | Deny, Indeterminate, Indeterminate -> Indeterminate
        | Deny, Indeterminate, Deny -> Deny
        | Deny, Indeterminate, Permit -> Indeterminate
        | Deny, Indeterminate, NotApplicable -> Indeterminate
        | Permit, Indeterminate, Indeterminate -> Indeterminate
        | Permit, Indeterminate, Deny -> Deny
        | Permit, Indeterminate, Permit -> Permit
        | _, NotApplicable, next -> next
        | _, _, _ -> Indeterminate
    match decisions with
      | [] -> NotApplicable
      | (eff,rf) :: rs -> check eff (rf req) (ruleDenyOverrides rs req)

  let rec rulePermitOverrides (decisions: (Decision * RuleF) list) (req: RequestContext) =
    let check d1 d2 d3 =
      match d1, d2, d3 with
        | _, Deny, NotApplicable -> Deny
        | _, Deny, next -> next
        | _, Permit, _ -> Permit
        | Permit, Indeterminate, Indeterminate -> Indeterminate
        | Permit, Indeterminate, Permit -> Permit
        | Permit, Indeterminate, Deny -> Indeterminate
        | Permit, Indeterminate, NotApplicable -> Indeterminate
        | Deny, Indeterminate, Indeterminate -> Indeterminate
        | Deny, Indeterminate, Permit -> Permit
        | Deny, Indeterminate, Deny -> Deny
        | _, NotApplicable, next -> next
        | _, _, _ -> Indeterminate
    match decisions with
      | [] -> NotApplicable
      | (eff,rf) :: rs -> check eff (rf req) (rulePermitOverrides rs req)

  let rec ruleFirstApplicable (decisions: (Decision * RuleF) list) (req: RequestContext) =
    let check d1 d2 =
      match d1, d2 with
        | Permit, _ -> Permit
        | Deny, _ -> Deny
        | Indeterminate, _ -> Indeterminate
        | NotApplicable, next -> next
    match decisions with
      | [] -> NotApplicable
      | (eff,rf) :: rs -> check (rf req) (ruleFirstApplicable rs req)

  // ------------ Do the rule combiner environment filling ------------
  ruleCombEnv.Add("deny-overrides", ruleDenyOverrides)
  ruleCombEnv.Add("permit-overrides", rulePermitOverrides)
  ruleCombEnv.Add("first-applicable", ruleFirstApplicable)