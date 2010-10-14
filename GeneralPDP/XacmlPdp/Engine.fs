namespace Microsoft.Research.GeneralPDP.XACML.PDP

open Microsoft.Research.GeneralPDP.XACML.Ast
open Basics
open Functions
open Matchs
open RuleCombiners
open PolicyCombiners

module Engine =

  /// Traverse a list of attributes and collect a bag with the ones matching type and id
  let rec attributeRetriever (ad: AttributeDesignator) (rc: RequestContext) =
    let issuerMatch (i1: option<IssuerId>) (i2: option<IssuerId>) =
      match i1, i2 with
      | None, _ -> true
      | Some s1, Some s2 -> s1 = s2
      | _, _ -> false
    let rec getAttrs (ad: AttributeDesignator) (atts: Attribute list) = 
      match ad with
      | AttributeDesignatorExp(cat, dt, aid, i, mbp) ->
        match atts with
        | [] -> []
        | att::atts -> (if cat = att.Category &&
                          aid = att.AttributeId && 
                          (att.Value.DataType() = Some dt) &&
                          issuerMatch i att.IssuerId then [att.Value] else []) @ getAttrs ad atts
      | _ -> failwith "Expecting attribute designator"
    let (values: Value list) = getAttrs ad rc.Attributes
    match ad with
    | AttributeDesignatorExp(cat, dt, aid, i, mbp) ->
      if values.IsEmpty && mbp then 
        raise(XacmlAttributeMissingException(ad))
      else 
        BagValue values
    | _ -> failwith "Expecting attribute designator"

  /// Given a target and a requests it returns a ValueT indicating if the requests matches the target
  and targetSemantics (subf: PredicateF) (resf: PredicateF) (actf: PredicateF) (req: RequestContext) = 
    logicalAnd [(subf req); (resf req); (actf req)]

  /// Returns (BoolAtomValue true) iff a requests context matches at least one predicate
  and anyMatch (preds: PredicateF list) (req: RequestContext) =
    logicalOr (List.map (fun s -> s req) preds)

  /// Returns (BoolAtomValue true) iff a requests context matches every predicate
  and allMatch (matches: PredicateF list) (req: RequestContext) =
    logicalAnd (List.map (fun s -> s req) matches)

  /// Apply the given match function onto the request
  and matches (valmatchf: FunctionF) (value: AttributeValue) 
              (designator: RequestContext -> Value) (req: RequestContext) =
    let rec traverse (v: Value) = 
      match v with 
      | BagValue [] -> BoolAtomValue false
      | BagValue (v::vs) -> check (valmatchf [value;v]) (traverse (BagValue vs))
      | _ -> IndeterminateValue
    and check b1 b2 = 
      match b1, b2 with
      | BoolAtomValue true, _ -> BoolAtomValue true
      | _, BoolAtomValue true -> BoolAtomValue true
      | BoolAtomValue false, IndeterminateValue -> IndeterminateValue
      | IndeterminateValue, BoolAtomValue false -> IndeterminateValue
      | BoolAtomValue false, BoolAtomValue false -> BoolAtomValue false
      | _, _ -> IndeterminateValue
    traverse (designator req)

  /// Return v no matter which RequestContext is coming
  and constVal (v: Value) (req: RequestContext) = v
    
  /// Evaluate function application expression over a RequestContext
  and apply (f: Value list -> Value) (es: (RequestContext -> Value) list) (req: RequestContext) =
    f (List.map (fun e -> e req) es)

  /// Apply a rule to a RequestContext to get a DecisionT
  and ruleSemantics (targetf: PredicateF) (condf: PredicateF) (effect: Decision) (req: RequestContext) =
    let check v d =
      match v, d with
      | BoolAtomValue true, decision -> decision
      | BoolAtomValue false, _ -> NotApplicable
      | _, _ -> Indeterminate
    check (targetf req) (check (condf req) effect)

  /// Apply a policy to a RequestContext to get a DecisionT and a list of obligations
  and policySemantics (targetf: PredicateF) (combf: RuleCombiningAlgF) (rules: (Decision * RuleF) list) 
                      (obligations: Obligation list) (req: RequestContext) =
    let rec check v r =
      match v, r with
      | BoolAtomValue false, _ -> (NotApplicable, [])
      | BoolAtomValue true, result -> result
      | _, _ -> (Indeterminate, [])
    and obligue effect = (effect, relevant effect obligations)
    and relevant effect obligations = 
      match effect, obligations with
      | _, [] -> []
      | Indeterminate, _ -> []
      | decision, ((Obligation (eff,s))::obls) -> 
          if decision = eff
            then (Obligation (eff,s)) :: relevant decision obls
          else
            relevant decision obls
    check (targetf req) (obligue (combf rules req))

  /// Aply a policySet to a RequestContext to get a DecisionT and a list of obligations
  and policySetSemantics (targetf: PredicateF) (combf: PolicyCombiningAlgF) (policies: (PredicateF * PolicyF) list)
                          (obligations: Obligation list) (req: RequestContext) =
    let check v r =
      match v,r with
      | (BoolAtomValue false), _ -> (NotApplicable, [])
      | (BoolAtomValue true), res -> res
      | _, _ -> (Indeterminate, [])
    check (targetf req) (combf policies obligations req)

  /// Apply a policy to a request and construct a response
  and pdpf (policyf: PolicyF) (req: RequestContext) =
    let check (decision, obligations) =
      match decision, obligations with 
      | Permit, obls -> ResponseContext(req.Id, Permit, Status("ok", "", ""), obls)
      | Deny, obls -> ResponseContext(req.Id, Deny, Status("ok", "", ""), obls)
      | NotApplicable, _ -> ResponseContext(req.Id, NotApplicable, Status("ok", "", ""), [])
      | Indeterminate, _ -> ResponseContext(req.Id, Indeterminate, Status("error?", "", ""), [])
    check (policyf req)

  /// Transform a PolicyT into a PolicyF
  and pap (policy: Policy) =
    let rec compilePolicy (policy: Policy) = 
      match policy with
      | Policy (name, target, rcid, rules, obligations) ->
          let targ = compileTarget target
          let policyf = policySemantics targ (ruleCombEnv.[rcid]) (List.map compileRule rules) obligations
          (targ, policyf)
      | PolicySet (name, target, pcid, policies, obligations) ->
          let targ = compileTarget target
          let policyf = policySetSemantics targ (policyCombEnv.[pcid]) (List.map compilePolicy policies) obligations
          (targ, policyf)
    let (targetf, policyf) = compilePolicy policy
    pdpf policyf

  /// Transform a sintactic RuleT into a semantic RuleF
  and compileRule (rule: Rule) =
    let cond = 
      match rule.Condition with
      | None -> ValueExp(BoolAtomValue true)
      | Some c -> c
    let rulef = ruleSemantics (compileTarget rule.Target) (compileExpression cond) rule.Decision
    (rule.Decision, rulef)

  /// Transform a sintactic TargetT into a semantic PredicateF  (if target is None then the predicate is always true)
  and compileTarget (t: Option<Target>) =
    match t with 
    | None -> constVal (BoolAtomValue true)
    | Some target -> targetSemantics (compileSubjects target.Subjects)
                                        (compileResources target.Resources)
                                        (compileActions target.Actions)

  /// Transform a list of subject targets into a PredicateF (if list=[] then the predicate is always true)
  and compileSubjects (subjects: SubjectTarget list) =
    let compileSubjectMatch (sm: SubjectMatch) =
      matches (matchEnv.[sm.MatchId]) sm.Value (compileExpression sm.Designator)
    let compileSubjectTarget (st: SubjectTarget) =
      allMatch (List.map compileSubjectMatch st.SubjectMatchs)
    if subjects.IsEmpty 
      then constVal (BoolAtomValue true)
      else anyMatch (List.map compileSubjectTarget subjects)

  /// Transform a list of resource targets into a PredicateF (if list=[] then the predicate is always true)
  and compileResources (resources: ResourceTarget list) =
    let compileResourceMatch (rm: ResourceMatch) =
      matches (matchEnv.[rm.MatchId]) rm.Value (compileExpression rm.Designator)
    let compileResourceTarget (rt: ResourceTarget) =
      allMatch (List.map compileResourceMatch rt.ResourceMatchs)
    if resources.IsEmpty 
      then constVal (BoolAtomValue true)
      else anyMatch (List.map compileResourceTarget resources)

  /// Transform a list of action targets into a PredicateF (if list=[] then the predicate is always true)
  and compileActions (actions: ActionTarget list) =
    let compileActionMatch (am: ActionMatch) =
      matches (matchEnv.[am.MatchId]) am.Value (compileExpression am.Designator)
    let compileActionTarget (at: ActionTarget) =
      allMatch (List.map compileActionMatch at.ActionMatchs)
    if actions.IsEmpty 
      then constVal (BoolAtomValue true)
      else anyMatch (List.map compileActionTarget actions)

  /// Transform an ExpressionT into a PredicateF
  and compileExpression (exp: Expression) = 
    match exp with
    | ValueExp x -> constVal x
    | ApplyExp (id, exps) -> apply (funcEnv.[id]) (List.map compileExpression exps)
    | AttributeDesignatorExp (cat, aid, dt, i, mbp) -> attributeRetriever exp


