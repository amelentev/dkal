namespace Microsoft.Research.GeneralPDP.Testing

open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.GeneralPDP.DKAL.Engine.Interface
open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Microsoft.Research.GeneralPDP.Translations.ToXACML.DkalResponseTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.XacmlPolicyTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.XacmlRequestTranslator
open Microsoft.Research.GeneralPDP.Translations.ToDKAL.Crawlers
open Microsoft.Research.GeneralPDP.RealXACML.RealXacmlParser

open System
open System.IO

module Main =
  let EXIT_CODE_XACML_DIFFERS = 1
  let EXIT_CODE_DKAL_COMMRULES_DIFFERS = 2
  let EXIT_CODE_DKAL_INFONS_DIFFERS = 3
  let EXIT_CODE_UNHANDLED_EXCEPTION = 4
  
  let main () =
    let args = System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail
    let parser = RealXacmlParser()
    try
      // read sql connection string from file
      let sql = File.ReadAllText(args.[0])

      // parse request
      let rawContents = File.ReadAllText(args.[1])
      let request = parser.ParseRequest rawContents
      printfn "%O" request

      // parse policy
      let rawContents = File.ReadAllText(args.[2])
      let policy = parser.ParsePolicy rawContents
      printfn "%O" policy

      // parse response
      let rawContents = File.ReadAllText(args.[3])
      let response = parser.ParseResponse rawContents
      printfn "Expected response:\n%O" response

      // execute XACML engine and check response
      let response' = (pap policy) request
      printfn "XACML PDP response:\n%O" response'
      if response.Decision <> response'.Decision then
        Environment.Exit(EXIT_CODE_XACML_DIFFERS)

      // translate policy to DKAL
      let pctx, _ = xacmlAwareParsingCtx "engine"
      let pTraslator = XacmlPolicyTranslator("engine", "engine", pctx)
      let dPolicy = pTraslator.TranslatePolicyToCommRules policy

      // translator for response
      let rspTranslator = DkalResponseTranslator()

      // create DKAL engine and install policy
      let heardFromDkal = ref false
      let engine = DkalEngineInterface("engine", sql, fun msg -> 
                                                       let _, response' = rspTranslator.TranslateResponse msg.message
                                                       printfn "DKAL CommRules response:\n%O" response'
                                                       heardFromDkal.Value <- true
                                                       if response.Decision <> response'.Decision then
                                                         Environment.Exit(EXIT_CODE_DKAL_COMMRULES_DIFFERS))
      engine.SetDispatcher("first")
      for assertion in dPolicy.Assertions do
        engine.AddRule assertion

      // translate request to DKAL 
      let reqTranslator = XacmlRequestTranslator(pctx)
      let atts = attributesInPolicy policy
      let dRequest = reqTranslator.TranslateRequest "engine" request atts

      // send the DKAL request to the commrule DKAL engine
      engine.ReceiveMessage {source= pctx.LookupOrAddPrincipal "engine";
                             target= pctx.LookupOrAddPrincipal "engine";
                             message= dRequest;
                             proviso= Term.Empty}
      engine.Talk()
      engine.CheckPoint()
      engine.Finish()
      
      // throw error if no response from dkal
      if not heardFromDkal.Value then
        Environment.Exit(EXIT_CODE_DKAL_COMMRULES_DIFFERS)

      // create a second engine with single rule policy
      let pctx, assertions = xacmlSingleCommRuleCtx "engine" "engine" "engine"
      heardFromDkal.Value <- false
      let engine = DkalEngineInterface("engine", sql, fun msg -> 
                                                       let _, response' = rspTranslator.TranslateResponse msg.message
                                                       printfn "DKAL Infons response:\n%O" response'
                                                       heardFromDkal.Value <- true
                                                       if response.Decision <> response'.Decision then
                                                         Environment.Exit(EXIT_CODE_DKAL_INFONS_DIFFERS))
      for assertion in assertions do
        engine.AddRule assertion

      // translate the policy to DKAL as infons and send it to the engine
      let pTraslator = XacmlPolicyTranslator("engine", "engine", pctx)
      let infons = pTraslator.TranslatePolicyToInfons policy
      for infon in infons do
        let signature = App(pctx.LookupFunction("Ev.signedBy"), [Const(Principal(pctx.LookupOrAddPrincipal "engine")); infon; Const(Int(42))])
        let justified = App(pctx.LookupFunction("justified"), [infon; signature]) 
        engine.ReceiveMessage {source= pctx.LookupOrAddPrincipal "engine";
                               target= pctx.LookupOrAddPrincipal "engine";
                               message= justified;
                               proviso= Term.Empty}

      // send the request to the engine
      let reqTranslator = XacmlRequestTranslator(pctx)
      let atts = attributesInPolicy policy
      let dRequest = reqTranslator.TranslateRequest "engine" request atts
      engine.ReceiveMessage {source= pctx.LookupOrAddPrincipal "engine";
                             target= pctx.LookupOrAddPrincipal "engine";
                             message= dRequest;
                             proviso= Term.Empty}
      engine.Talk()
      engine.CheckPoint()
      engine.Finish()

      // throw error if no response from dkal
      if not heardFromDkal.Value then
        Environment.Exit(EXIT_CODE_DKAL_INFONS_DIFFERS)

    with 
    | SyntaxError(p,s) -> 
      printfn "%O: %O" p s
      Environment.Exit(EXIT_CODE_UNHANDLED_EXCEPTION)
    | ex -> 
      printfn "Unhandled Exception: %O" ex
      Environment.Exit(EXIT_CODE_UNHANDLED_EXCEPTION)

  do main()
