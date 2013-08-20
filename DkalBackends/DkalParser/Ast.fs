namespace Microsoft.Research.DkalBackends

module Ast =

  type Term = 
  | AnyTerm
  | ConstTerm of string*string  // value + dkal type
  | VarTerm of string*string    // name + dkal type
    with 
      override t.ToString() = 
        match t with
        | AnyTerm -> "<term>"
        | ConstTerm (c, t) -> c
        | VarTerm (v, t) -> v

  type Speech = SaidSpeech | ImpliedSpeech | AnySpeech
    with 
      override s.ToString() = 
        match s with
        | AnySpeech -> "<speech>"
        | SaidSpeech -> "said"
        | ImpliedSpeech -> "implied"   

  type InfonFormula =
  | AtomFormula of string * Term list
  | AndFormula of InfonFormula * InfonFormula
  | ImpliesFormula of InfonFormula * InfonFormula
  | SpeechFormula of Term * Speech * InfonFormula
    with 
      override f.ToString() = 
        match f with
        | AtomFormula(pred, terms) -> pred + "(" + (String.concat "," (List.map (fun t -> t.ToString()) terms)) + ")"
        | AndFormula(f1, f2) -> "(" + f1.ToString() + " + " + f2.ToString() + ")"
        | ImpliesFormula(f1, f2) -> "(" + f1.ToString() + " -> " + f2.ToString() + ")"
        | SpeechFormula(ppal, speech, f1) -> "(" + ppal.ToString() + " " + speech.ToString() + " " + f1.ToString() + ")"

  type InferenceProblem = (InfonFormula list) * (InfonFormula list)

  type Prefix = 
  | EmptyPrefix
  | SpeechPrefix of Prefix * Term * Speech
    with 
      override p.ToString() = 
        match p with
        | EmptyPrefix -> "<empty>"
        | SpeechPrefix(EmptyPrefix, ppal, speech) -> ppal.ToString() + " " + speech.ToString()
        | SpeechPrefix(p', ppal, speech) -> p'.ToString() + " " + ppal.ToString() + " " + speech.ToString()
  
  type MatterElement = MatterTerm of Term | MatterSpeech of Speech
    with 
      override me.ToString() = 
        match me with
        | MatterTerm(t) -> t.ToString()
        | MatterSpeech(s) -> s.ToString()

  type Matter = MatterElement list * MatterElement list

  type PrefixedInfonFormula = 
  | AtomPrefixedFormula of Prefix * string * Term list
  | AndPrefixedFormula of Prefix * PrefixedInfonFormula * PrefixedInfonFormula
  | ImpliesPrefixedFormula of Prefix * PrefixedInfonFormula * PrefixedInfonFormula
  | SpeechPrefixedFormula of Prefix * Term * Speech * PrefixedInfonFormula
    with 
      override pf.ToString() = 
        match pf with
        | AtomPrefixedFormula(p, pred, terms) -> "-" + p.ToString() + "- " + pred + "(" + (String.concat "," (List.map (fun t -> t.ToString()) terms)) + ")"
        | AndPrefixedFormula(p, f1, f2) -> "-" + p.ToString() + "- (" + f1.ToString() + " + " + f2.ToString() + ")"
        | ImpliesPrefixedFormula(p, f1, f2) -> "-" + p.ToString() + "- (" + f1.ToString() + " -> " + f2.ToString() + ")"
        | SpeechPrefixedFormula(p, ppal, speech, f1) -> "-" + p.ToString() + "- (" + ppal.ToString() + " " + speech.ToString() + " " + f1.ToString() + ")"

