namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.GeneralPDP.XACML.Ast

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util

open ExpressionTranslator

open Option
open System.Collections.Generic

module XacmlRequestTranslator =

  type XacmlRequestTranslator (pctx: ParsingCtx) =

    member this.TranslateRequest (reqId: int) (pep: string) (req: RequestContext) 
                                 (fullAttributes: HashSet<AttributeDesignator>) =
      let translator = ExpressionTranslator(pctx)
      let reqId' = Const(Int(reqId))
      let pep' = Const(Principal(pctx.LookupOrAddPrincipal(pep)))

      let unfold = 
        List.reduceBack (fun t1 t2 -> App(pctx.LookupFunction("and"), [t1; t2]))

      let foundAttribute (a: Attribute) = 
        let dt = get (a.Value.DataType())
        // capture embedded infons
        if a.Category = SubjectCategory && a.AttributeId = "infon" then
          match a.Value with
          | StringAtomValue(s) -> 
              pctx.ParseInfon(s)
          | _ -> failwith "Expecting string in embedded infon"
        else
          translator.AttributeInfon reqId' pep' 
            (AttributeDesignatorExp(a.Category, dt, a.AttributeId, None, false)) (translator.TranslateValue a.Value)

      let missingAttribute (ad: AttributeDesignator) =
        let dt = match ad with
                  | AttributeDesignatorExp(_, dt, _, _, _) -> dt
                  | _ -> failwith "Expecting attribute designator in attribute translation"
        translator.AttributeInfon reqId' pep' ad (translator.TranslateNull dt)

      let presentAttributes = List.map foundAttribute req.Attributes
      let fullAttributes = Seq.filter (fun ad -> match ad with
                                                  | AttributeDesignatorExp(cat, dt, aid, _, _) -> 
                                                      not (List.exists (fun (a: Attribute) -> 
                                                        cat = a.Category && dt = get (a.Value.DataType()) && aid = a.AttributeId) req.Attributes)
                                                  | _ -> true) fullAttributes

      let missingAttributes = Seq.map missingAttribute fullAttributes |> Seq.toList
      
      let reqArrived = translator.RequestArrivedInfon reqId' pep'

      [reqArrived] @ presentAttributes @ missingAttributes |> unfold




