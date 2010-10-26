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
  let EXIT_CODE_DKAL_DIFFERS = 2
  let EXIT_CODE_UNHANDLED_EXCEPTION = 3
  
  let main () =
    let args = System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail
    let parser = RealXacmlParser()
    try
      // parse request
      let rawContents = File.ReadAllText(args.[0])
      let request = parser.ParseRequest rawContents
      printfn "%O" request

      // parse policy
      let rawContents = File.ReadAllText(args.[1])
      let policy = parser.ParsePolicy rawContents
      printfn "%O" policy

      // parse response
      let rawContents = File.ReadAllText(args.[2])
      let response = parser.ParseResponse rawContents
      printfn "%O" response

      // execute XACML engine and check response
      let response' = (pap policy) request
      printfn "%O" response'
      if response.Decision <> response'.Decision then
        Environment.Exit(EXIT_CODE_XACML_DIFFERS)

      // translate policy to DKAL
      let pctx, _ = xacmlAwareParsingCtx "engine"
      let pTraslator = XacmlPolicyTranslator("engine", "engine", pctx)
      let dPolicy = pTraslator.TranslatePolicyToCommRules policy

      // translator for response
      let rspTranslator = DkalResponseTranslator()

      // create DKAL engine and install policy
      let engine = DkalEngineInterface("engine", fun msg -> 
                                                   let _, _, response' = rspTranslator.TranslateResponse msg.message
                                                   printfn "%O" response'
                                                   if response.Decision <> response'.Decision then
                                                     Environment.Exit(EXIT_CODE_DKAL_DIFFERS))
      engine.SetDispatcher("first")
      for assertion in dPolicy.Assertions do
        engine.AddRule assertion
      engine.Talk()

      // translate request to DKAL 
      let reqTranslator = XacmlRequestTranslator(pctx)
      let atts = attributesInPolicy policy
      let dRequest = reqTranslator.TranslateRequest "engine" request atts

      // send the DKAL request to the DKAL engine
      engine.ReceiveMessage {source= pctx.LookupOrAddPrincipal "engine";
                             target= pctx.LookupOrAddPrincipal "engine";
                             message= dRequest;
                             proviso= Term.Empty}
      engine.CheckPoint()
      engine.Finish()

    with 
    | SyntaxError(p,s) -> 
      printfn "%O: %O" p s
      Environment.Exit(EXIT_CODE_UNHANDLED_EXCEPTION)
    | ex -> 
      printfn "Unhandled Exception: %O" ex
      Environment.Exit(EXIT_CODE_UNHANDLED_EXCEPTION)

  do main()
