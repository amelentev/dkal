namespace Microsoft.Research.GeneralPDP.DKAL.Engine

open Microsoft.Research.DkalEngine
open DkalCommon

open System.IO

module ParsingCtxFactory =

  let basicParsingCtx (ppalName: string) = 
    let ppalName = "_me"

    let pctx = ParsingCtx()
    let assertions = pctx.ParsePrelude() 
                      //@ (pctx.ParseStream "" (new StringReader(privateSql)))
                      @ (pctx.ParseStream "" (new StringReader(identity ppalName)))
    pctx, assertions

  let xacmlAwareParsingCtx (ppalName: string) = 
    let ppalName = "_me"

    let pctx, assertions = basicParsingCtx(ppalName)
    let assertions = assertions @ (pctx.ParseStream "" (new StringReader(xacmlAwareDkalAssertions)))
    pctx, assertions

  let xacmlBasicTrustingCtx (ppalName: string) (pep: string) =
    let ppalName = "_me"
    
    let pctx, assertions = xacmlAwareParsingCtx(ppalName)
    let assertions = assertions @ (pctx.ParseStream "" (new StringReader(basicTrustDkalAssertions ppalName pep)))
    pctx, assertions

  let xacmlSingleCommRuleCtx (ppalName: string) (pap: string) (pep: string) =
    let ppalName = "_me"

    let pctx, assertions = xacmlBasicTrustingCtx ppalName pep
    let assertions = assertions @ (pctx.ParseStream "" (new StringReader(singleCommRuleDkalAssertions ppalName pap)))
    pctx, assertions

