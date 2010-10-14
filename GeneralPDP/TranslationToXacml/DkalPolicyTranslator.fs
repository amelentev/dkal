namespace Microsoft.Research.GeneralPDP.Translations.ToXACML

open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.Simplifier
open DkalTermTranslator

module DkalPolicyTranslator =

  exception DkalPolicyTranslatorException of string

  /// Translates DKAL assertions into XACML elements
  type DkalPolicyTranslator() = 
  
    /// Translates a list of DKAL communication rules into an XACML PolicyT 
    /// using "first-applicable" rule combiner.
    member t.TranslateCommRules (pcy: DkalPolicy) = 
      let mutable rules = []
      let mutable unusedAssertions = []

      for assertion in pcy.Assertions do
        try
          match assertion with
          | SendTo(c) -> 
              match t.TranslateCommRule c with
              | Some rule -> 
                  rules <- rules @ [rule]
              | None -> ()
          | _ -> raise(DkalPolicyTranslatorException("Only communication rules can be encoded into XACML policies"))
        with
          | DkalPolicyTranslatorException(s) ->
                printfn "%s" s
                unusedAssertions <- unusedAssertions @ [assertion]

      Policy(pcy.PolicyId, None, "first-applicable", rules, []), unusedAssertions
      
    /// Translates a DKAL communication rule into an XACML RuleT.
    /// If the DKAL comm rule is for not-applicable it is dropped
    member t.TranslateCommRule (c: Communication) =
      let termTranslator = DkalTermTranslator()
      try
        let condition = termTranslator.TranslateInfon c.trigger |> simplifyExp
        let decision = termTranslator.TranslateDecision c.message
        if decision = NotApplicable then
          None 
        else
          Some (Rule (None, Some condition, decision))
      with
        | DkalTermTranslatorException(s) -> raise(DkalPolicyTranslatorException(s))

