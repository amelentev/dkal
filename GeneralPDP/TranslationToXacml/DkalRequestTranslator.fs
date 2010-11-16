namespace Microsoft.Research.GeneralPDP.Translations.ToXACML

open DkalTermTranslator
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.DkalEngine.Ast

module DkalRequestTranslator =

  exception DkalRequestTranslatorException of string
  let fail s s' = 
    raise(DkalRequestTranslatorException ("Expecting " + s + " when translating request to XACML, found: " + s'))

  type DkalRequestTranslator () =
    
    let termTranslator = DkalTermTranslator()

    member this.TranslateRequest (infon: Infon) =

      // and(t1, and(t2, and(t3, t4))) -> [t1; t2; t3; t4]
      let rec obtainConjunctionTerms t = 
        match t with
        | App(f, [t1; t2]) when f.name = "and" -> 
            obtainConjunctionTerms t1 @ obtainConjunctionTerms t2
        | t -> [t]

      // certified(said(p,t)) -> t
      let rec stripPrefix t = 
        match t with 
        | App(f, [t; _]) when f.name = "justified" -> 
            stripPrefix t
        | App(f, [_; t]) when f.name = "said" -> 
            stripPrefix t
        | t -> t

      // get a list of terms that inform request number, request attributes and other stuff
      let terms = stripPrefix infon |> obtainConjunctionTerms

      // to be filled as the terms get processed
      let mutable attributes = []
      let mutable reqId = None
      let mutable reqFrom = None
      let mutable unusedTerms = []

      // process terms
      for t in terms do
        match t with
        | App(f, [Const(Int(i)); Const(Principal(p))]) 
            when 
              f.name = "req-*-issued-by-*" -> 
                  reqId <- Some i
                  reqFrom <- Some p.Name
        | App(f, [_; _; Const(Text(cat)); Const(Text(aid)); value]) 
            when 
              f.name = "req-*-by-*-has-*-attribute-string-*-valued-*" || f.name = "req-*-by-*-has-*-attribute-int-*-valued-*" ->
                match value with
                | Const(c) ->
                    // Its a const value, we translate it to an XACML value
                    let value' = termTranslator.TranslateValue(c)
                    attributes <- attributes @ [Attribute(AttributeCategory.FromString(cat), aid, value')]
                | _ -> 
                    // Its not a const value, it might be a null value
                    try
                      termTranslator.TranslateTerm(value) |> ignore
                      unusedTerms <- unusedTerms @ [t]
                    with
                    | DkalNullTermException(_) -> ()
                    | _ -> fail "(possibly null) value" (value.ToSX().ToString())
        | t -> 
            unusedTerms <- unusedTerms @ [t]

      match reqId, reqFrom with
      | None, _ 
      | _, None -> fail "req-*-issued-by-*" (infon.ToSX().ToString())
      | Some reqId, Some reqFrom -> 
          reqId, reqFrom, RequestContext(reqId, attributes), unusedTerms


