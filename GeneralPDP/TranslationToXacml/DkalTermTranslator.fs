namespace Microsoft.Research.GeneralPDP.Translations.ToXACML

open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.GeneralPDP.XACML.Ast

open System
open System.Collections.Generic

module DkalTermTranslator =

  exception DkalTermTranslatorException of string
  let fail s s' = 
    raise(DkalTermTranslatorException ("Expecting " + s + " when translating DKAL Term to XACML, found: " + s'))

  exception DkalNullTermException of Datatype

  type DkalTermTranslator() = 
    
    let ctx = new Dictionary<Var, AttributeDesignator>()

    member t.TranslateInfon(i: Infon) =
      ctx.Clear()
      let mutable conditions = []
      let rec doTranslate i = 
        match i with
        | App(f, terms) when
            f.name = "and" ->
              List.collect doTranslate terms
        | App(f, [term]) when 
            f.name = "asInfon" ->
              [t.TranslateTerm term]
        | App(f, [reqId; pep]) when
            f.name = "req-*-issued-by-*" ->
              []
        | App(f, [req; pep; cat; aid; term]) when 
            f.name = "req-*-by-*-has-*-attribute-string-*-valued-*" || f.name = "req-*-by-*-has-*-attribute-int-*-valued-*" ->
              // get datatype
              let dt = match f.argTypes.[4].typ.name with
                       | "text" -> StringDatatype
                       | "int" -> IntDatatype
                       | _ -> failwith ("Unrecognized datatype in attribute infon: " + f.argTypes.[3].typ.name)
              // get category
              let cat = match cat with
                        | Const(Text(s)) -> AttributeCategory.FromString(s)
                        | _ -> failwith "Expecting attribute category to be a constant text value"
              // get attribute id
              let aid = match aid with 
                        | Const(Text(s)) -> s
                        | _ -> failwith "Expecting attribute id to be a constant text value"
              // construct designator
              let ad = AttributeDesignatorExp(cat, dt, aid, None, false)
              // see if we have a value or a variable
              match term with
              | Var(v) -> 
                  ctx.[v] <- ad
                  []
              | term -> 
                  [ApplyExp(dt.ToString() + "-equal", [ad; t.TranslateTerm term])]
        | _ -> fail "infon term" (i.ToSX().ToString())
      conditions <- doTranslate i
      ApplyExp("and", conditions)


    member t.TranslateDecision(i: Infon) =
      match i with
      | App(f, [_; _; decision]) when f.name = "req-*-by-*-decision-*" -> 
          match decision with 
          | Const(Text(s)) -> Decision.FromString(s)
          | _ -> fail "constant text term in decision infon" (decision.ToSX().ToString())
      | _ -> fail "a decision infon" (i.ToSX().ToString())


    member t.TranslateFunction(f: Function) =
      match f.name with
               | "not" -> "not"
               | "&&" -> "and"
               | "||" -> "or"
               | "<=" -> "integer-less-than-or-equal"
               |  "<" -> "integer-less-than"
               | ">=" -> "integer-greater-than-or-equal"
               | ">" -> "integer-greater-than"
               | "==" -> match f.retType.typ.name with
                         | "text" -> "string-equal"
                         | "int" -> "integer-equal"
                         | "bool" -> "boolean-equal"
                         | _ -> raise(DkalTermTranslatorException("Unsupported equality type from DKAL on: " + f.retType.typ.name))
               | f -> f
      

    member t.TranslateValue(value: Const) =
      match value with
      | Int(i) -> IntAtomValue(i)
      | Bool(b) -> BoolAtomValue(b)
      | Text(s) -> StringAtomValue(s)
      | _ -> raise(DkalTermTranslatorException("No translation to XACML for " + value.ToString()))


    member t.TranslateTerm (term: Term) =
      match term with
      | App(f, terms) -> 
          if f.name = "false" || f.name = "true" then
            ValueExp(BoolAtomValue(Boolean.Parse(f.name)))
          elif f.name.EndsWith("_null") then
            let datatypeName = f.name.Substring(0, f.name.LastIndexOf("_") - 1)
            raise(DkalNullTermException(Datatype.FromString datatypeName))
          else
            ApplyExp(t.TranslateFunction f, List.map t.TranslateTerm terms)
      | Const(c) -> ValueExp(t.TranslateValue c)
      | Var(v) -> 
          if ctx.ContainsKey(v) then
            ctx.[v]
          else
            raise(DkalTermTranslatorException("Variable not fixed as attribute: " + v.name))

    member t.StripSignatures (term: Term) =
      match term with 
      | App(f, [term; _]) when f.name = "certified" -> t.StripSignatures(term)
      | term -> term
