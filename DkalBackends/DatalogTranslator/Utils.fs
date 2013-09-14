namespace Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator

open System
open System.Collections.Generic
open Microsoft.Research.DkalBackends.Ast
open Datalog

module Utils = 
  
  let rec stripPrefixedFormula (f: PrefixedInfonFormula) =
    match f with
    | AtomPrefixedFormula(p, r, ts) -> AtomFormula(r, ts)
    | AndPrefixedFormula(p, pi1, pi2) -> AndFormula(stripPrefixedFormula(pi1), stripPrefixedFormula(pi2))
    | ImpliesPrefixedFormula(p, pi1, pi2) -> ImpliesFormula(stripPrefixedFormula(pi1), stripPrefixedFormula(pi2))
    | SpeechPrefixedFormula(p, ppal, speech, pi) -> SpeechFormula(ppal, speech, stripPrefixedFormula(pi))

  let rec prefixedFormula (p: Prefix) (f: InfonFormula) = 
    match f with
    | AtomFormula(r, ts) -> AtomPrefixedFormula(p, r, ts)
    | AndFormula(f1, f2) -> 
      let pf1 = prefixedFormula p f1
      let pf2 = prefixedFormula p f2
      AndPrefixedFormula(p, pf1, pf2)
    | ImpliesFormula(f1, f2) -> 
      let pf1 = prefixedFormula p f1
      let pf2 = prefixedFormula p f2
      ImpliesPrefixedFormula(p, pf1, pf2)
    | SpeechFormula(ppal, speech, f1) -> 
      let pf1 = prefixedFormula (SpeechPrefix(p, ppal, speech)) f1
      SpeechPrefixedFormula(p, ppal, speech, pf1)

  let rec prefixLength (p: Prefix) = 
    match p with
    | EmptyPrefix -> 0
    | SpeechPrefix(p, _, _) -> 2 + prefixLength p

  let rec matterLengths (pf: PrefixedInfonFormula) =
    match pf with
    | AtomPrefixedFormula(p,_,ts) -> (prefixLength p, ts.Length)
    | AndPrefixedFormula(p,f1,f2) -> 
      let _, m1 = matterLengths f1
      let _, m2 = matterLengths f2
      (prefixLength p, m1 + m2)
    | ImpliesPrefixedFormula(p,f1,f2) -> 
      let _, m1 = matterLengths f1
      let _, m2 = matterLengths f2
      (prefixLength p, m1 + m2)
    | SpeechPrefixedFormula(p,_,_,f1) -> 
      let _, m1 = matterLengths f1
      (prefixLength p, 2 + m1)

  let rec prefixFormMatter (p: Prefix) =
    match p with
    | EmptyPrefix -> EmptyPrefix, []
    | SpeechPrefix(p', ppal, speech) -> 
      let pf, pm = prefixFormMatter p'
      (SpeechPrefix(pf, AnyTerm, AnySpeech), pm @ [MatterTerm ppal; MatterSpeech speech])

  // function to obtain a list of Datalog arguments declaration from a formula matter
  let matterToDatalogArgumentsDeclaration (pm, fm) =
    let args = List<string * Sort>()
    for me in pm @ fm do
      match me with
      | MatterSpeech(_) -> args.Add("x"+args.Count.ToString(), "Q")
      | MatterTerm(_) -> args.Add("x"+args.Count.ToString(), "C")
    args

  // function to obtain a list of Datalog terms from a formula matter
  let matterToDatalogTerms (pm, fm) = 
    let args = List<Term>()
    for me in pm @ fm do
      match me with
      | MatterSpeech(AnySpeech) -> failwith "impossible"
      | MatterSpeech(s) -> args.Add(AtomTerm(s.ToString()))
      | MatterTerm(AnyTerm) -> failwith "impossible"
      | MatterTerm(Microsoft.Research.DkalBackends.Ast.VarTerm(v, t)) -> args.Add(VarTerm(v))
      | MatterTerm(ConstTerm(c, t)) -> args.Add(AtomTerm(c))
    args

