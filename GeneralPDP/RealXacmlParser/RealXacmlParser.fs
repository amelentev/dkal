namespace Microsoft.Research.GeneralPDP.RealXACML

open Microsoft.Research.GeneralPDP.XACML.Ast

open System
open System.Collections.Generic
open System.IO
open System.Xml

module RealXacmlParser = 
  
  type XacmlAttribute = Microsoft.Research.GeneralPDP.XACML.Ast.Attribute

  type RealXacmlParser() =

    // enumerate children (to avoid using XmlNodeList)
    let getAllChildren (node: XmlNode) = 
      let mutable ret = []
      for n in node.ChildNodes do
        ret <- ret @ [n]
      ret

    let getChildren (node: XmlNode) (name: string) = 
      let mutable ret = []
      for n in node.ChildNodes do
        if n.Name.ToLower() = name.ToLower() then
          ret <- ret @ [n]
      ret

    let getChild (node: XmlNode) (name: string) = 
      match getChildren node name with
      | n :: _ -> n
      | [] -> failwith ("No child named " + name)
    
    let maybeGetChild (node: XmlNode) (name: string) = 
      match getChildren node name with
      | n :: _ -> Some n
      | [] -> None

    let removePrefix (inp: string) = 
      inp.Substring(inp.LastIndexOf(':')+1)

    member private rp.ParseDatatype (datatype: string) = 
      let datatype = if datatype.Contains "#" then
                       datatype.Substring(datatype.LastIndexOf('#')+1).ToLower()
                     elif datatype.Contains ":" then
                       datatype.Substring(datatype.LastIndexOf(':')+1).ToLower()
                     else
                       datatype
      match datatype with
      | "anyuri"
      | "rfc822name"
      | "string" -> StringDatatype
      | "integer" -> IntDatatype
      | "double" -> DoubleDatatype
      | "boolean" -> BoolDatatype
      | t -> failwith ("Unsupported datatype: " + t)

    member private rp.ParseAtomValue (dt: Datatype) (s: string) =
      match dt with
        | IntDatatype -> IntAtomValue (Int32.Parse s)
        | BoolDatatype -> BoolAtomValue (Boolean.Parse s)
        | DoubleDatatype -> DoubleAtomValue (Double.Parse s)
        | StringDatatype -> StringAtomValue s

    member private rp.ParseAttribute (att: XmlNode) (cat: AttributeCategory) = 
      let aid = removePrefix att.Attributes.["AttributeId"].Value
      let dt = rp.ParseDatatype att.Attributes.["DataType"].Value
      let value = rp.ParseAtomValue dt (getChild att "AttributeValue").FirstChild.Value
      XacmlAttribute(cat, aid, value)

    member private rp.ParseExpression (e: XmlNode) = 
      match e.Name with 
      | "SubjectAttributeDesignator" ->
        let aid = removePrefix e.Attributes.["AttributeId"].Value
        let dt = rp.ParseDatatype e.Attributes.["DataType"].Value
        AttributeDesignatorExp(SubjectCategory, dt, aid, None, false)
      | "ResourceAttributeDesignator" ->
        let aid = removePrefix e.Attributes.["AttributeId"].Value
        let dt = rp.ParseDatatype e.Attributes.["DataType"].Value
        AttributeDesignatorExp(ResourceCategory, dt, aid, None, false)
      | "ActionAttributeDesignator" ->
        let aid = removePrefix e.Attributes.["AttributeId"].Value
        let dt = rp.ParseDatatype e.Attributes.["DataType"].Value
        AttributeDesignatorExp(ActionCategory, dt, aid, None, false)
      | "EnvironmentAttributeDesignator" ->
        let aid = removePrefix e.Attributes.["AttributeId"].Value
        let dt = rp.ParseDatatype e.Attributes.["DataType"].Value
        AttributeDesignatorExp(EnvironmentCategory, dt, aid, None, false)
      | "AttributeValue" ->
        let dt = rp.ParseDatatype e.Attributes.["DataType"].Value
        let value = rp.ParseAtomValue dt e.FirstChild.Value
        ValueExp(value)
      | "Condition" 
      | "Apply" ->
        let functionId = removePrefix e.Attributes.["FunctionId"].Value
        let exps = List.map rp.ParseExpression (getAllChildren e)
        ApplyExp(functionId, exps)
      | n -> failwith ("Unsupported expression name: " + n)

    member private rp.ParseTarget (t: XmlNode option) =
      match t with
      | None -> None
      | Some t ->
        let subjects = getChild t "Subjects"
        let resources = getChild t "Resources"
        let actions = getChild t "Actions"
        match maybeGetChild subjects "AnySubject", maybeGetChild resources "AnyResource", maybeGetChild actions "AnyAction" with
        | Some _, Some _, Some _ -> None // empty target
        | anyS, anyR, anyA ->
          let sts = match anyS with
                    | Some _ -> []
                    | _ -> 
                      List.map (fun (s: XmlNode) -> 
                        SubjectTarget(List.map (fun (sm: XmlNode) -> 
                          let matchId = removePrefix sm.Attributes.["MatchId"].Value
                          let dt = rp.ParseDatatype (getChild sm "AttributeValue").Attributes.["DataType"].Value
                          let value = rp.ParseAtomValue dt (getChild sm "AttributeValue").FirstChild.Value
                          let designator = rp.ParseExpression (getChild sm "SubjectAttributeDesignator")
                          SubjectMatch(matchId, value, designator)) (getChildren s "SubjectMatch"))) (getChildren subjects "Subject")
          let rts = match anyR with
                    | Some _ -> []
                    | _ -> 
                      List.map (fun (r: XmlNode) -> 
                        ResourceTarget(List.map (fun (rm: XmlNode) -> 
                          let matchId = removePrefix rm.Attributes.["MatchId"].Value
                          let dt = rp.ParseDatatype (getChild rm "AttributeValue").Attributes.["DataType"].Value
                          let value = rp.ParseAtomValue dt (getChild rm "AttributeValue").FirstChild.Value
                          let designator = rp.ParseExpression (getChild rm "ResourceAttributeDesignator")
                          ResourceMatch(matchId, value, designator)) (getChildren r "ResourceMatch"))) (getChildren resources "Resource")
          let ats = match anyA with
                    | Some _ -> []
                    | _ -> 
                      List.map (fun (a: XmlNode) -> 
                        ActionTarget(List.map (fun (am: XmlNode) -> 
                          let matchId = removePrefix am.Attributes.["MatchId"].Value
                          let dt = rp.ParseDatatype (getChild am "AttributeValue").Attributes.["DataType"].Value
                          let value = rp.ParseAtomValue dt (getChild am "AttributeValue").FirstChild.Value
                          let designator = rp.ParseExpression (getChild am "ActionAttributeDesignator")
                          ActionMatch(matchId, value, designator)) (getChildren a "ActionMatch"))) (getChildren actions "Action")
          Some (Target(sts, rts, ats))

    member private rp.ParseRule (r: XmlNode) =
      let effect = Decision.FromString r.Attributes.["Effect"].Value
      let target = rp.ParseTarget (maybeGetChild r "Target")
      let condition = match maybeGetChild r "Condition" with
                      | None -> None
                      | Some c -> Some (rp.ParseExpression c)
      Rule(target, condition, effect)

    member private rp.ParsePolicyFromNode (p: XmlNode) = 
      match p.Name.ToLower() with
      | "policy" -> 
        let policyId = p.Attributes.["PolicyId"].Value
        let ruleCombiner = removePrefix p.Attributes.["RuleCombiningAlgId"].Value
        let target = rp.ParseTarget (maybeGetChild p "Target")
        let rules = List.map rp.ParseRule (getChildren p "Rule")
        Policy(policyId, target, ruleCombiner, rules, [])
      | "policyset" -> 
        let policyId = p.Attributes.["PolicySetId"].Value
        let policyCombiner = removePrefix p.Attributes.["PolicyCombiningAlgId"].Value
        let target = rp.ParseTarget (maybeGetChild p "Target")
        let policies = List.map rp.ParsePolicyFromNode (getChildren p "Policy")
        PolicySet(policyId, target, policyCombiner, policies, [])
      | _ -> failwith "Expecting policy or policyset root element"

    member rp.ParsePolicy (inp: string) = 
      let doc = new XmlDocument()
      doc.LoadXml(inp)
      let root = doc.DocumentElement
      rp.ParsePolicyFromNode root

    member rp.ParseRequest (inp: string) = 
      let doc = new XmlDocument()
      doc.LoadXml(inp)
      let attributes = new List<XacmlAttribute>()
      let root = doc.DocumentElement
      match root.Name.ToLower() with
      | "request" -> 
        match maybeGetChild root "subject" with
        | Some n -> for m in getChildren n "attribute" do
                      attributes.Add(rp.ParseAttribute m SubjectCategory)
        | _ -> ()
        match maybeGetChild root "resource" with
        | Some n -> for m in getChildren n "attribute" do
                      attributes.Add(rp.ParseAttribute m ResourceCategory)
        | _ -> ()
        match maybeGetChild root "action" with
        | Some n -> for m in getChildren n "attribute" do
                      attributes.Add(rp.ParseAttribute m ActionCategory)
        | _ -> ()
        match maybeGetChild root "environment" with
        | Some n -> for m in getChildren n "attribute" do
                      attributes.Add(rp.ParseAttribute m EnvironmentCategory)
        | _ -> ()
        RequestContext(1, Seq.toList attributes)
      | _ -> failwith "Expecting request root element"

    member rp.ParseResponse (inp: string) = 
      let doc = new XmlDocument()
      doc.LoadXml(inp)
      let root = doc.DocumentElement
      match root.Name.ToLower() with
      | "response" -> 
        let result = getChild root "Result"
        let decision = Decision.FromString (getChild result "Decision").FirstChild.Value
        let code = removePrefix (getChild (getChild result "Status") "StatusCode").Attributes.["Value"].Value
        ResponseContext(1, decision, Status(code, "", ""), [])
      | _ -> failwith "Expecting response root element"

