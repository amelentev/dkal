namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.DkalEngine
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.DkalEngine.Ast

module XacmlResponseTranslator =

  type XacmlResponseTranslator (pctx: ParsingCtx) =

    member this.TranslateResponse (reqId: int) (pep: string) (response: ResponseContext) =
      let reqId' = Const(Int(reqId))
      let pep' = Const(Principal(pctx.LookupOrAddPrincipal(pep)))
      let response' = Const(Text(response.Decision.ToString()))
      App(pctx.LookupFunction("req-*-by-*-decision-*"), [reqId'; pep'; response'])


