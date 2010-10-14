namespace Microsoft.Research.GeneralPDP.XACML

open Microsoft.Research.GeneralPDP.Utils.PrettyPrinter

open System

module Ast = 

  /// Type of policy IDs
  type PolicyId = string

  type PolicyRequestContext(policyId: string) = 
    member pr.PolicyId = policyId
    override pr.ToString() = 
      "XACMLPolicyRequest " + policyId

  /// Basic datatypes that attributes may have
  type Datatype = IntDatatype | BoolDatatype | DoubleDatatype | StringDatatype
    with 
      override dt.ToString() = 
          match dt with 
          | IntDatatype -> "int"
          | BoolDatatype -> "bool"
          | DoubleDatatype -> "double"
          | StringDatatype -> "string"
      static member FromString(s: string) = 
        match s.ToLower() with
        | "int" -> IntDatatype
        | "bool" -> BoolDatatype
        | "double" -> DoubleDatatype
        | "string" -> StringDatatype
        | _ -> failwith ("Unrecognized datatype: " + s)

  /// The AttributeCategory indicates to which element (in the request) the attribute refers
  type AttributeCategory = SubjectCategory | ResourceCategory | ActionCategory | EnvironmentCategory
    with
      override ac.ToString() = 
        match ac with 
        | SubjectCategory -> "Subject"
        | ResourceCategory -> "Resource"
        | ActionCategory -> "Action"
        | EnvironmentCategory -> "Environment"
      static member FromString(s: string) = 
        match s.ToLower() with
        | "subject" -> SubjectCategory
        | "resource" -> ResourceCategory
        | "action" -> ActionCategory
        | "environment" -> EnvironmentCategory
        | _ -> failwith ("Unrecognized category: " + s)

  /// Attributes are used in policy expressions and also in requests
  type Value = 
  | IntAtomValue of int
  | BoolAtomValue of bool
  | DoubleAtomValue of double
  | StringAtomValue of string
  | IndeterminateValue
  | BagValue of Value list
  with 
    member v.ToBareString() =
        match v with
        | IntAtomValue i -> i.ToString()
        | BoolAtomValue b -> b.ToString().ToLower()
        | DoubleAtomValue d -> d.ToString()
        | StringAtomValue s -> "\"" + s + "\""
        | IndeterminateValue -> "IndeterminateValue"
        | BagValue vs -> "[" + (List.map (fun v -> v.ToString()) vs |> String.concat ", ") + "]"
    override v.ToString() =
        match v with 
        | IntAtomValue i -> "int \"" + i.ToString() + "\""
        | BoolAtomValue b -> "bool \"" + b.ToString() + "\""
        | DoubleAtomValue d -> "double \"" + d.ToString() + "\""
        | StringAtomValue s -> "string \"" + s + "\""
        | IndeterminateValue -> "IndeterminateValue"
        | BagValue vs -> "bag [" + (List.map (fun v -> v.ToString()) vs |> String.concat ", ") + "]"
    member v.DataType() =
      match v with
      | IntAtomValue _ -> Some IntDatatype
      | BoolAtomValue _ -> Some BoolDatatype
      | DoubleAtomValue _ -> Some DoubleDatatype
      | StringAtomValue _ -> Some StringDatatype
      | _ -> None

  /// An AttributeValue cannot be indeterminate nor a bag
  type AttributeValue = Value
    
  /// Auxiliary types, mostly strings
  and AttributeId = string
  and IssuerId = string
  and RuleCombinatorId = string
  and PolicyCombinatorId = string
  and FunctionId = string
  and MatchId = string
  and MustBePresent = bool

  /// Expressions used in XACML policies
  type Expression = 
  | ValueExp of AttributeValue
  | ApplyExp of FunctionId * Expression list
  | AttributeDesignatorExp of AttributeCategory * Datatype * AttributeId * option<IssuerId> * MustBePresent
  with
    member e.Tokenize() = 
      match e with 
      | ValueExp a -> [a.ToString() |> TextToken] 
      | ApplyExp (f, es) -> ["\"" + f.ToString() + "\" (" |> TextToken; 
                      TabToken;
                      List.map (fun (e: Expression) -> e.Tokenize()) es |> List.concat |> ManyTokens;
                      UntabToken;
                      TextToken ")"]
      | AttributeDesignatorExp (ac, dt, a, oi, mbp) ->
                      [ac.ToString() + " " +
                        dt.ToString() + 
                        " \"" + a + "\"" +
                        (match oi with None -> "" | Some i -> " by \"" + i + "\"") + 
                        (if mbp then " MustBePresent" else "") |> TextToken]
    override e.ToString() = e.Tokenize() |> prettyPrint
      
  /// XACML Request
  type RequestContext(id: int, attributes: Attribute list) = 
    member rc.Id = id
    member rc.Attributes = attributes
    member rc.DocId() = 
      let mutable documentId = None
      for att in rc.Attributes do
        if att.Category = ResourceCategory && att.AttributeId = "document-id" then
          match att.Value with
          | StringAtomValue(id) -> 
              documentId <- Some id
          | _ -> 
              failwith "Expecting string value in document-id"
      documentId
    member rc.DocLabels() = 
      let mutable labels = []
      for att in rc.Attributes do
        if att.Category = ResourceCategory && att.AttributeId = "labels" then
          match att.Value with
          | StringAtomValue(labelText) -> 
              labels <- labels @ (List.ofArray (labelText.Split [|','|]))
          | _ -> 
              failwith "Expecting string value in labels"
      labels
    member rc.DocContent() = 
      let mutable content = ""
      for att in rc.Attributes do
        if att.Category = ResourceCategory && att.AttributeId = "content" then
          match att.Value with
          | StringAtomValue(c) -> 
              content <- content + c
          | _ -> 
              failwith "Expecting string value in content"
      content
    member rc.Action() = 
      let mutable action = None
      for att in rc.Attributes do
        if att.Category = ActionCategory && att.AttributeId = "action-id" then
          match att.Value with
          | StringAtomValue(id) -> 
              action <- Some id
          | _ -> 
              failwith "Expecting string value in action-id"
      action
    member rc.DecisionPoints() = 
      let mutable pointers = []
      for att in rc.Attributes do
        if att.Category = ResourceCategory && att.AttributeId = "decision-point" then
          match att.Value with
          | StringAtomValue(pointer) -> 
              pointers <- pointers @ [pointer]
          | _ -> 
              failwith "Expecting string value in decision-point"
      pointers
    member rc.PolicyPointers() = 
      let mutable policyPointers = []
      for att in rc.Attributes do
        if att.Category = ResourceCategory && att.AttributeId = "policies" then
          match att.Value with
          | StringAtomValue(ps) -> 
                let ps = ps.Split([|','|])
                for p in ps do
                  let parts = p.Split([|'@'|])
                  if parts.Length = 1 then
                    policyPointers <- policyPointers @ [("", parts.[0])]
                  elif parts.Length = 2 then
                    policyPointers <- policyPointers @ [(parts.[0], parts.[1])]
                  else
                    failwith "Expecting 'policyId@policyRepositoryPointer' in policy list"
          | _ -> 
                failwith "Expecting string value in policy-id"
      policyPointers
    member rc.Tokenize() =
      ["XACMLRequest " + rc.Id.ToString() |> TextToken; 
        TabToken;
        List.map (fun (a: Attribute) -> a.Tokenize()) rc.Attributes |> List.concat |> ManyTokens;
        UntabToken]
    override rc.ToString() = rc.Tokenize() |> prettyPrint

  and Attribute(category: AttributeCategory, attributeId: AttributeId, value: AttributeValue, ?issuerId: IssuerId) =  
    member a.Category = category
    member a.AttributeId = attributeId
    member a.Value = value
    member a.IssuerId = issuerId
    member a.Tokenize() =
      [a.Category.ToString() + " \"" + a.AttributeId + "\" = " + a.Value.ToString() + 
        (match a.IssuerId with None -> "" | Some i -> " by \"" + i + "\"") |> TextToken]
  
  /// XACML Response
  and ResponseContext(id: int, decision: Decision, status: Status, obligations: Obligation list) = 
    member rc.Id = id
    member rc.Decision = decision
    member rc.Status = status
    member rc.Obligations = obligations
    member rc.Tokenize() = 
      ["XACMLResponse " + id.ToString() + " " + rc.Decision.ToString() |> TextToken;
        TabToken;
        rc.Status.Tokenize() |> ManyTokens;
        List.map (fun (o: Obligation) -> o.Tokenize()) rc.Obligations |> List.concat |> ManyTokens;
        UntabToken]
    override rc.ToString() = rc.Tokenize() |> prettyPrint
  
  and Decision = Permit | Deny | Indeterminate | NotApplicable
    with
      override d.ToString() =
        match d with
        | Permit -> "Permit"
        | Deny -> "Deny"
        | Indeterminate -> "Indeterminate"
        | NotApplicable -> "NotApplicable"
      static member FromString(s: string) = 
        match s.ToLower() with
        | "permit" -> Permit
        | "deny" -> Deny
        | "indeterminate" -> Indeterminate
        | "notapplicable" -> NotApplicable
        | _ -> failwith ("Unrecognized decision: " + s)

  and Status(code: String, message: String, status: String) =
    member s.Code = code 
    member s.Message = message
    member s.Status = status
    member s.Tokenize() =
      ["Code = \"" + s.Code + "\"" |> TextToken;
        "Message = \"" + s.Message + "\"" |> TextToken;
        "Status = \"" + s.Status + "\"" |> TextToken]

  and Obligation = Obligation of Decision * String
    with 
      member o.Tokenize() = 
        match o with 
          | Obligation (eff, s) -> ["Obligation on " + eff.ToString() + " do \"" + s + "\"" |> TextToken] 

  /// XACML Policy
  and Policy = 
  | Policy of PolicyId * option<Target> * RuleCombinatorId * Rule list * Obligation list
  | PolicySet of PolicyId * option<Target> * PolicyCombinatorId * Policy list * Obligation list
  with
    member p.PolicyId = 
      match p with
      | Policy (name, _, _, _, _) -> name
      | PolicySet (name, _, _, _, _) -> name
    member p.Tokenize() =
      match p with
        | Policy (name, target, rcid, rules, obligations) -> 
          ["XACMLPolicy \"" + name + "\"" |> TextToken;
            TabToken;
            (match target with None -> [] | Some t -> t.Tokenize()) |> ManyTokens;
            "RuleCombinator \"" + rcid + "\"" |> TextToken;
            List.map (fun (r: Rule) -> r.Tokenize()) rules |> List.concat |> ManyTokens;
            List.map (fun (o: Obligation) -> o.Tokenize()) obligations|> List.concat |> ManyTokens;
            UntabToken]
        | PolicySet (name, target, pcid, policies, obligations) ->
          ["XACMLPolicySet \"" + name + "\"" |> TextToken;
            TabToken;
            (match target with None -> [] | Some t -> t.Tokenize()) |> ManyTokens;
            "PolicyCombinator \"" + pcid + "\"" |> TextToken;
            List.map (fun (p: Policy) -> p.Tokenize()) policies |> List.concat |> ManyTokens;
            List.map (fun (o: Obligation) -> o.Tokenize()) obligations|> List.concat |> ManyTokens;
            UntabToken]
    override p.ToString() = p.Tokenize() |> prettyPrint

  and Rule(target: option<Target>, condition: option<Expression>, decision: Decision) =
    member r.Target = target
    member r.Condition = condition
    member r.Decision = decision
    member r.Tokenize() = 
      ["Rule" |> TextToken;
        TabToken;
        (match r.Target with None -> [] | Some t -> t.Tokenize()) |> ManyTokens]
        @ (match r.Condition with
          | None -> []
          | Some c -> ["Condition" |> TextToken; TabToken] @ c.Tokenize() @ [UntabToken])
        @ [sprintf "%A" r.Decision |> TextToken;
        UntabToken]
    override r.ToString() = r.Tokenize() |> prettyPrint
         
  and Target(subjects: SubjectTarget list, resources: ResourceTarget list, actions: ActionTarget list) =
    member t.Subjects = subjects
    member t.Resources = resources
    member t.Actions = actions
    member t.Tokenize() =
      [List.map (fun (st: SubjectTarget) -> st.Tokenize()) t.Subjects |> List.concat |> ManyTokens;
        List.map (fun (rt: ResourceTarget) -> rt.Tokenize()) t.Resources |> List.concat |> ManyTokens;
        List.map (fun (at: ActionTarget) -> at.Tokenize()) t.Actions |> List.concat |> ManyTokens]

  and SubjectTarget(subjectMatchs: SubjectMatch list) =
    member st.SubjectMatchs = subjectMatchs
    member st.Tokenize() =
      ["SubjectTarget" |> TextToken;
        TabToken;
        List.map (fun (sm: SubjectMatch) -> sm.Tokenize()) st.SubjectMatchs |> List.concat |> ManyTokens;
        UntabToken]
    override st.ToString() = st.Tokenize() |> prettyPrint

  and ResourceTarget(resourceMatchs: ResourceMatch list) =
    member rt.ResourceMatchs = resourceMatchs
    member rt.Tokenize() =
      ["ResourceTarget" |> TextToken;
        TabToken;
        List.map (fun (rm: ResourceMatch) -> rm.Tokenize()) rt.ResourceMatchs |> List.concat |> ManyTokens;
        UntabToken]
    override rt.ToString() = rt.Tokenize() |> prettyPrint

  and ActionTarget(actionMatchs: ActionMatch list) =
    member at.ActionMatchs = actionMatchs
    member at.Tokenize() =
      ["ActionTarget" |> TextToken;
        TabToken;
        List.map (fun (am: ActionMatch) -> am.Tokenize()) at.ActionMatchs |> List.concat |> ManyTokens;
        UntabToken]
    override at.ToString() = at.Tokenize() |> prettyPrint

  and SubjectMatch(matchId: MatchId, value: AttributeValue, designator: AttributeDesignator) =
    member sm.MatchId = matchId
    member sm.Value = value
    member sm.Designator = designator
    member sm.Tokenize() =
      ["\"" + sm.MatchId + "\"" |> TextToken;
        TabToken;
        sm.Designator.Tokenize() |> ManyTokens;
        "= " + sm.Value.ToString() |> TextToken;
        UntabToken]

  and AttributeDesignator = Expression

  and ResourceMatch(matchId: MatchId, value: AttributeValue, designator: AttributeDesignator) =
    member rm.MatchId = matchId
    member rm.Value = value
    member rm.Designator = designator
    member rm.Tokenize() =
      ["\"" + rm.MatchId + "\"" |> TextToken;
        TabToken;
        rm.Designator.Tokenize() |> ManyTokens;
        "= " + rm.Value.ToString() |> TextToken;
        UntabToken]

  and ActionMatch(matchId: MatchId, value: AttributeValue, designator: AttributeDesignator) =
    member am.MatchId = matchId
    member am.Value = value
    member am.Designator = designator
    member am.Tokenize() =
      ["\"" + am.MatchId + "\"" |> TextToken;
        TabToken;
        am.Designator.Tokenize() |> ManyTokens;
        "= " + am.Value.ToString() |> TextToken;
        UntabToken]

  // XACML attribute requests 
  type AttributeRequestContext(ad: AttributeDesignator, req: RequestContext) = 
    member arc.Designator = ad
    member arc.Request = req
    member arc.Tokenize() = 
      ["XACMLAttributeRequest" |> TextToken;
        TabToken;
        arc.Designator.Tokenize() |> ManyTokens;
        arc.Request.Tokenize() |> ManyTokens;
        UntabToken]
    override arc.ToString() = 
      arc.Tokenize() |> prettyPrint

  // XACML attribute responses
  type AttributeResponseContext(att: Attribute option, req: RequestContext) = 
    member arc.Attribute = att
    member arc.Request = req
    member arc.Tokenize() = 
      ["XACMLAttributeResponse" |> TextToken;
        TabToken;
        (match arc.Attribute with
        | None -> "Attribute not found" |> TextToken
        | Some att -> att.Tokenize() |> ManyTokens);
        arc.Request.Tokenize() |> ManyTokens;
        UntabToken]
    override arc.ToString() = 
      arc.Tokenize() |> prettyPrint
