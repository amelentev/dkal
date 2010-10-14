namespace Microsoft.Research.GeneralPDP.Translations.ToXACML

open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.DkalEngine.Ast

open System

module DkalResponseTranslator =

  exception DkalResponseTranslatorException of string 
  let fail s s' = 
    raise(DkalResponseTranslatorException ("Expecting " + s + " when translating response to XACML, found: " + s'))

  type DkalResponseTranslator () =

    member this.TranslateResponse (infon: Infon) =
      match infon with
      | App(f, [t; _]) when f.name = "certified" -> 
          this.TranslateResponse t
      | App(f, [_; t]) when f.name = "said" -> 
          this.TranslateResponse t
      | App(f, [Const(Int(i)); Const(Principal(pep)); Const(Text(s))]) when f.name = "req-*-by-*-decision-*" -> 
          let pep' = pep.Name
          i, pep', ResponseContext(i, Decision.FromString(s), Status("ok", "", ""), [])
      | _ -> fail "a decision infon" (infon.ToSX().ToString()) 
