namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Message
open Scenario
open EndPointImageFactory
open BasicEndPoint
open EnforcementEndPoint
open DataStorageEndPoint
open PolicyRepositoryEndPoint
open AttributeRepositoryEndPoint
open XacmlEndPoint
open DkalEndPoint 
open AdjudicationEndPoint
open XacmlToDkalEndPoint
open DkalToXacmlEndPoint
open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open Microsoft.Research.GeneralPDP.XACML.Parsing
open Microsoft.Research.GeneralPDP.Data.Ast

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast

open System
open System.IO
open System.Xml

module Parsing = 

  let readStringAttribute (n: XmlNode) (id: string) (def: string option) =
    match n.Attributes.[id] with
    | null -> match def with
              | None -> failwith ("expecting " + id + " string attribute on " + n.Name)
              | Some def -> def
    | a -> a.Value

  let readIntAttribute (n: XmlNode) (id: string) (def: int option) =
    match n.Attributes.[id] with
    | null -> match def with
              | None -> failwith ("expecting " + id + " int attribute on " + n.Name)
              | Some def -> def
    | a -> Int32.Parse(a.Value)

  let readBoolAttribute (n: XmlNode) (id: string) (def: bool option) =
    match n.Attributes.[id] with
    | null -> match def with
              | None -> failwith ("expecting " + id + " bool attribute on " + n.Name)
              | Some def -> def
    | a -> Boolean.Parse(a.Value.ToLower())

  let constructPctx (n: XmlNode) (id: EndPointId) =
    let pepId = readStringAttribute n "pepId" (Some "")
    let papId = readStringAttribute n "papId" (Some "")
    let xacml = readStringAttribute n "xacml" (Some "")
    match xacml with
    | "aware" -> xacmlAwareParsingCtx id
    | "basicTrust" -> 
      if pepId = "" then
        failwith "Expecting pepId string attribute in basic XACML trusting DKAL endpoint"
      xacmlBasicTrustingCtx id pepId
    | "singleRule" -> 
      if pepId = "" || papId = "" then
        failwith "Expecting pepId and papId string attributes in XACML single rule DKAL endpoint"
      xacmlSingleCommRuleCtx id papId pepId
    | "" -> basicParsingCtx id
    | x -> failwith ("Unrecognized XACML awareness in DKAL endpoint: " + x)

  let loadDkalPolicy (n: XmlNode) (id: EndPointId) (pctx: ParsingCtx) (assertions: Assertion list) = 
    let pid = readStringAttribute n "policyId" None
    let file = readStringAttribute n "file" (Some "")
    let assertions = assertions @ 
                      if file <> "" then 
                        pctx.ParseFile file
                      else 
                        []
    DkalPolicy(pid, assertions)

  let parseScenario inp = 
    let doc = new XmlDocument()
    doc.LoadXml(inp)

    // create scenario
    let root = doc.DocumentElement
    let scenario = match root.Name.ToLower() with
                   | "scenario" -> 
                      let name = readStringAttribute root "name" (Some "unnamed")
                      Scenario(name)
                   | _ -> failwith "Expecting scenario root element"

    // simulation steps
    let mutable steps = []

    for n in root.ChildNodes do
      // load endpoints
      if n.Name.ToLower() = "endpoint" then
        let id = readStringAttribute n "id" None
        let typ = readStringAttribute n "type" (Some "basic")
        let ep = match typ.ToLower() with
                 | "basic" -> BasicEndPoint(id) :> IEndPoint
                 | "enforcement" -> 
                    let ep = EnforcementEndPoint(id)
                    ep.SharePoint <- readBoolAttribute n "sharePoint" (Some false)
                    ep :> IEndPoint
                 | "datastorage" -> 
                    let ep = DataStorageEndPoint(id)
                    ep.SharePoint <- readBoolAttribute n "sharePoint" (Some false)
                    for p in n.ChildNodes do
                      // parse synced eps
                      if p.Name.ToLower() = "sync" then
                        let syncId = readStringAttribute p "syncId" None
                        ep.AddSyncEp syncId
                      // parse labels
                      if p.Name.ToLower() = "label" then
                        let labelId = readStringAttribute p "labelId" None
                        let policyPointer = PolicyPointer.Parse (readStringAttribute p "policyPointer" None)
                        let mutable pdps = []
                        for q in p.ChildNodes do
                          if q.Name.ToLower() = "pdp" then
                            let pdpId = readStringAttribute q "pdpId" None
                            pdps <- pdps @ [pdpId]
                        ep.AddLabel labelId (LabelInfo(pdps, policyPointer))
                      // parse docs
                      if p.Name.ToLower() = "document" then
                        let documentId = readStringAttribute p "documentId" None
                        let content = readStringAttribute p "content" None
                        let mutable labels = []
                        for q in p.ChildNodes do
                          if q.Name.ToLower() = "label" then
                            let labelId = readStringAttribute q "labelId" None
                            labels <- labels @ [labelId]
                        ep.AddDocument documentId (DocumentInfo(content, labels)) true
                    ep :> IEndPoint
                 | "policyrepository" -> 
                    let mutable xPolicies = []
                    let mutable dPolicies = []
                    for p in n.ChildNodes do
                      if p.Name.ToLower() = "xacmlpolicy" then
                        let policyFile = readStringAttribute p "policyFile" None
                        let rawContents = File.ReadAllText(policyFile)
                        let policy = parsePolicy rawContents
                        xPolicies <- xPolicies @ [policy]
                      if p.Name.ToLower() = "dkalpolicy" then
                        let pctx, assertions = constructPctx p id
                        let policy = loadDkalPolicy p id pctx assertions
                        dPolicies <- dPolicies @ [policy]
                    PolicyRepositoryEndPoint(id, xPolicies, dPolicies) :> IEndPoint
                 | "attributerepository" -> 
                    AttributeRepositoryEndPoint(id) :> IEndPoint
                 | "xacml" -> 
                    match readStringAttribute n "attributeRepositoryId" (Some "") with
                    | "" -> XacmlEndPoint(id) :> IEndPoint
                    | s -> XacmlEndPoint(id, s) :> IEndPoint
                 | "dkal" -> 
                    let pepId = readStringAttribute n "pepId" (Some "")
                    let papId = readStringAttribute n "papId" (Some "")
                    let xacml = readStringAttribute n "xacml" (Some "")
                    let pctx, assertions = constructPctx n id
                    let policy = loadDkalPolicy n id pctx assertions
                    let sql = File.ReadAllText (readStringAttribute n "sql" None)
                    let ep = DkalEndPoint(id, sql, policy)
                    // engine options from policy file
                    if pctx.Options.ContainsKey "dispatcher" then
                      ep.SetDispatcher (pctx.Options.["dispatcher"])
                    if pctx.Options.ContainsKey "learning" then
                      ep.SetLearning (pctx.Options.["learning"])
                    // setup trust comm rules from
                    for p in n.ChildNodes do
                      if p.Name.ToLower() = "trustrulesfrom" then
                        let id = readStringAttribute p "id" None
                        ep.AddTrustRulesFrom id
                    // engine options from XML
                    let dispatcher = readStringAttribute n "dispatcher" (Some "")
                    if dispatcher <> "" then
                      ep.SetDispatcher (dispatcher)
                    let learning = readStringAttribute n "learning" (Some "")
                    if learning <> "" then
                      ep.SetLearning (learning)
                    ep :> IEndPoint
                 | "adjudication" ->
                    let ep = AdjudicationEndPoint(id)
                    for p in n.ChildNodes do
                      if p.Name.ToLower() = "pdp" then
                        ep.AddDecisionPoint(readStringAttribute p "id" None)
                    ep :> IEndPoint
                 | "xacml->dkal" ->
                    let dkalId = readStringAttribute n "dkalId" None
                    let pctx, _ = xacmlAwareParsingCtx id
                    let policyFile = readStringAttribute n "policyFile" (Some "")
                    if policyFile <> "" then 
                      pctx.ParseFile policyFile |> ignore
                    let ep = XacmlToDkalEndPoint(id, dkalId, pctx) 
                    let translateToRules = readBoolAttribute n "translateToRules" (Some false)
                    ep.TranslateToRules <- translateToRules
                    ep :> IEndPoint
                 | "dkal->xacml" ->
                    let dkalId = readStringAttribute n "dkalId" None
                    let xacmlId = readStringAttribute n "xacmlId" None
                    if readStringAttribute n "file" (Some "") <> "" then
                      let pctx, assertions = constructPctx n id
                      let pcy = loadDkalPolicy n id pctx assertions
                      DkalToXacmlEndPoint(id, xacmlId, dkalId, pctx, pcy) :> IEndPoint
                    else
                      DkalToXacmlEndPoint(id, xacmlId, dkalId) :> IEndPoint
                 | t -> failwith ("Unsopported endpoint type: " + t)
        ep.Color <- EPColor.Parse(readStringAttribute n "color" (Some "std"))
        scenario.AddEndPoint(ep)
      
      // collect simulation steps
      elif n.Name.ToLower() = "message" then
        let from = readStringAttribute n "from" None
        let too = readStringAttribute n "to" None
        let file = readStringAttribute n "file" None
        let rawContents = File.ReadAllText(file)
        let typ = readStringAttribute n "type" None
        let content = match typ.ToLower() with
                      | "infon" -> 
                          let pctx, _ = xacmlAwareParsingCtx(from)
                          InfonContent(pctx.ParseInfon rawContents)
                      | "xacmlrequest" -> XacmlRequestContent(parseRequest rawContents)
                      | "xacmlresponse" -> XacmlResponseContent(parseResponse rawContents)
                      | "xacmlpolicy" -> XacmlPolicyContent(parsePolicy rawContents)
                      | "xacmlpolicyrequest" -> XacmlPolicyRequestContent(parsePolicyRequest rawContents)
                      | "dkalpolicy" -> 
                          let pctx, assertions = constructPctx n from
                          let policy = loadDkalPolicy n from pctx assertions
                          DkalPolicyContent(policy)
                      | "dkalpolicyrequest" -> DkalPolicyRequestContent(DkalPolicyRequest.ParseFrom rawContents)
                      | t -> failwith ("Unsopported message type: " + t)
        steps <- steps @ [MessageStep {sender= from; receiver= too; content= content}]
      
      elif n.Name.ToLower() = "sleep" then
        let amount = readIntAttribute n "amount" None
        steps <- steps @ [SleepStep amount]

    scenario, steps
