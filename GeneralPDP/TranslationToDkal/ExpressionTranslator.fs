namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.GeneralPDP.XACML.Ast
open Context
open Crawlers

open System

module ExpressionTranslator =

  type ExpressionTranslator(pctx: ParsingCtx) = 

    let ctx = Context(pctx)

    member t.TranslateFunction(f: FunctionId) =
      let f' = match f with
               | "not" -> "not"
               | "and" -> "&&"
               | "or" -> "||"
               | "integer-less-than-or-equal" -> "<="
               | "integer-less-than" -> "<"
               | "integer-greater-than-or-equal" -> ">="
               | "integer-greater-than" -> ">"
               | "string-equal" -> "=="
               | "integer-equal" -> "=="
               | "boolean-equal" -> "=="
               | f -> f
      pctx.LookupFunction(f')


    member t.TranslateNull(dt: Datatype) =
      App(t.TranslateFunction(dt.ToString() + "_null"), [])


    member t.TranslateValue(value: Value) =
      match value with
      | IntAtomValue(i) -> Const(Int(i))
      | StringAtomValue(s) -> Const(Text(s))
      | BoolAtomValue(b) -> App(pctx.LookupFunction(b.ToString().ToLower()), [])
      | _ -> failwith ("No translation to DKAL for " + value.DataType.ToString())


    member t.TranslateExpression (e: Expression) =
      match e with
      | AttributeDesignatorExp (cat, dt, aid, i, mbp) ->
          if not (ctx.ContainsAttributeDesignator(e)) then
            ctx.Add(e) |> ignore
          Var(ctx.GetVar(e))
      | ValueExp v -> t.TranslateValue(v)
      // For now one-and-only (and bags in general) are not supported by DKAL
      | ApplyExp (f, [e]) when f.EndsWith("-one-and-only") -> t.TranslateExpression e
      | ApplyExp (f, es) ->
        let f' = t.TranslateFunction f
        let es' = List.map t.TranslateExpression es
        if f = "or" || f = "and" then
          List.reduceBack (fun t1 t2 -> App(f', [t1; t2])) es'
        else
          App(f', es')


    /// Returns the list of attribute infons for every variable mentioned in the input expression
    member t.AttributeInfons (reqId: Term) (pep: Term) (exp: Expression) =
      let atts = attributesInExpression exp |> Seq.toList
      let attributeInfon (ad: AttributeDesignator) =
        t.AttributeInfon reqId pep ad (Var(ctx.GetVar ad))
      List.map attributeInfon atts
      

    member t.AttributeInfon (reqId: Term) (pep: Term) (ad: AttributeDesignator) (value: Term) =
      match ad with 
      | AttributeDesignatorExp(cat, dt, aid, _, _) -> 
          App(t.TranslateFunction ("req-*-by-*-has-*-attribute-" + dt.ToString() + "-*-valued-*"),
            [reqId; 
              pep;
              Const(Text(cat.ToString().ToLower()));
              Const(Text(aid));
              value])
      | _ -> failwith "Expecting AttributeDesignator while translating attribute"


    member t.AsInfon (term: Term) =
      App(t.TranslateFunction "asInfon", [term])

    member t.RequestArrivedInfon (reqId: Term) (pep: Term) = 
      App(t.TranslateFunction "req-*-issued-by-*", [reqId; pep])

    member t.DecisionInfon (reqId: Term) (pep: Term) (decision: Decision) =
      let decision' = Const(Text(decision.ToString().ToLower()))
      App(t.TranslateFunction "req-*-by-*-decision-*", [reqId; pep; decision'])

