// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator

open System.Collections.Generic
open Microsoft.Research.DkalBackends.Ast
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator.Datalog
open Utils
open Mapping

type DatalogTranslator() =
    let prefixedPointer = Dictionary<InfonFormula, PrefixedInfonFormula>()
    let formPointer = Dictionary<PrefixedInfonFormula, PrefixedInfonFormula>()
    let matterPointer = Dictionary<PrefixedInfonFormula, Matter>()
    let leaderIds = Dictionary<PrefixedInfonFormula, int>()
    // mapping of possible quotations
    let quotationsMapping = Mapping<Speech>()
    // constants mapping contains both the constant value (term) as well as a string representation of its original type. The type is not saved for the translation though
    let constantsMapping = Mapping<Microsoft.Research.DkalBackends.Ast.Term>()
    let relationSignatures= Dictionary<string, string option list>()
    let translatedRelations= Dictionary<string,InfonFormula>()
    
    // for a relation name, give the infontemplate and the term substitution per parameter of the relation
    let infonTemplates= Dictionary<string, InfonFormula*Dictionary<Microsoft.Research.DkalBackends.Ast.Term,int>>()

    do
        quotationsMapping.Add(SaidSpeech)
        quotationsMapping.Add(ImpliedSpeech)

    member tr.QuotationsMapping with get() = quotationsMapping
    member tr.ConstantsMapping with get() = constantsMapping
    member tr.RelationSignatures with get() = relationSignatures
    member tr.TranslatedRelations with get() = translatedRelations

    /// Traverses the parse tree filling the form and matter dictionaries
    member private tr.visitSubformula (pf: PrefixedInfonFormula)=
        match pf with
        | AtomPrefixedFormula(p, r, ts) -> 
            let pform, pMatter = prefixFormMatter p
            let form = AtomPrefixedFormula(pform, r, List.replicate ts.Length AnyTerm)
            let matter = (pMatter, List.map MatterTerm ts)
            let dkalInfon= AtomFormula(r, ts)
            formPointer.[pf] <- form
            matterPointer.[pf] <- matter
            form, matter
        | AndPrefixedFormula(p, pf1, pf2) -> 
            let form1, (_, matter1) = tr.visitSubformula pf1 
            let form2, (_, matter2) = tr.visitSubformula pf2 
            let pform, pMatter = prefixFormMatter p
            let form = AndPrefixedFormula(pform, form1, form2)
            let matter = (pMatter, matter1 @ matter2)
            // let dkalInfon= AndFormula(dkalInf1, dkalInf2)
            formPointer.[pf] <- form
            matterPointer.[pf] <- matter
            form, matter
        | ImpliesPrefixedFormula(p, pf1, pf2) -> 
            let form1, (_, matter1) = tr.visitSubformula pf1 
            let form2, (_, matter2) = tr.visitSubformula pf2 
            let pform, pMatter = prefixFormMatter p
            let form = ImpliesPrefixedFormula(pform, form1, form2)
            let matter = (pMatter, matter1 @ matter2)
            // let dkalInfon= ImpliesFormula(dkalInf1, dkalInf2)
            formPointer.[pf] <- form
            matterPointer.[pf] <- matter
            form, matter
        | SpeechPrefixedFormula(p, ppal, speech, pf1) -> 
            let form1, (_, matter1) = tr.visitSubformula pf1 
            let pform, pMatter = prefixFormMatter p
            let form = SpeechPrefixedFormula(pform, AnyTerm, AnySpeech, form1)
            let matter = (pMatter, MatterTerm ppal :: MatterSpeech speech :: matter1)
            // let dkalInfon= SpeechFormula(ppal, SaidSpeech, dkalInf1)
            formPointer.[pf] <- form
            matterPointer.[pf] <- matter
            form, matter

    member private tr.extractSignature (matter:Matter) =
        let sign= fst(matter) @ snd(matter)
        sign |> List.map (fun arg -> match arg with
                                       | MatterTerm(Microsoft.Research.DkalBackends.Ast.VarTerm(c,t)) -> Some t
                                       | MatterTerm(ConstTerm(c,t)) -> Some t
                                       | _ -> None //failwith "Cannot extract signature from AnyTerm"
                           )

    member tr.visitInfonTemplate (template:InfonFormula) (subst:Dictionary<Microsoft.Research.DkalBackends.Ast.Term,int>) (fact:Relation) =
        match template with
        | AtomFormula(rel, terms) -> AtomFormula(rel, terms |> List.map (fun term -> if subst.ContainsKey(term) then
                                                                                        let argList= fact.Args |> Seq.toList
                                                                                        // argList.[subst.[term]]
                                                                                        term
                                                                                     else
                                                                                        term
                                                                        )
                                                )
        | AndFormula(inf1, inf2) -> AndFormula(tr.visitInfonTemplate inf1 subst fact, tr.visitInfonTemplate inf2 subst fact)
        | ImpliesFormula(inf1, inf2) -> ImpliesFormula(tr.visitInfonTemplate inf1 subst fact, tr.visitInfonTemplate inf2 subst fact)
        | SpeechFormula(ppal, speech, inf) -> SpeechFormula( (if subst.ContainsKey(ppal) then
                                                                let argList= fact.Args |> Seq.toList
                                                                //argList.[subst.[ppal]]
                                                                ppal
                                                              else ppal),
                                                             speech,
                                                             tr.visitInfonTemplate inf subst fact
                                                           )

    member tr.buildBackInfon (fact: Relation) =
        let signature= relationSignatures.[fact.Name]
        let infonTemplate= infonTemplates.[fact.Name]
        tr.visitInfonTemplate (fst(infonTemplate)) (snd(infonTemplate)) fact

    member tr.translateInferenceProblem (problem: InferenceProblem)=
        // convert each formula (hypothesis or thesis) into a prefixed formula
        // calculate form and matter 
        let (hypotheses, theses)= problem
        (hypotheses @ theses) |> Seq.iter (fun f -> prefixedPointer.[f] <- prefixedFormula EmptyPrefix f
                                                    tr.visitSubformula prefixedPointer.[f] |> ignore
                                          )

        // mapping of possible constants
        matterPointer.Values |> Seq.collect (fun (pm, fm) -> pm @ fm) |> Seq.iter (fun me -> match me with
                                                                                             | MatterTerm(ConstTerm(c, t)) -> constantsMapping.Add(ConstTerm(c, t)) |> ignore
                                                                                             | _ -> ()
                                                                                  )

        // assign a unique id to each distinct formula form
        for kv in formPointer do
            if not(leaderIds.ContainsKey kv.Value) then
                leaderIds.Add(kv.Value, leaderIds.Count)
                matterPointer.[kv.Value] <- matterPointer.[kv.Key]

        // construct Datalog program
        let program = new Program()
        program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("Q", quotationsMapping.Count, "quotations.map")))
        program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("C", constantsMapping.Count, "constants.map")))
        program.AddDeclarationPart(NewLineDeclarationPart)

        // create the D* "derivable" relations
        for kv in leaderIds do
            let pl, fl = matterLengths kv.Key
            let matter = matterPointer.[kv.Key]
            let signature= tr.extractSignature(matter)
            let args = matterToDatalogArgumentsDeclaration matter
            let derivableRelationName= "D" + kv.Value.ToString()
            let rd = RelationDeclaration(derivableRelationName, args, Input)
            // TODO caution: stripping prefixes as such will result in nonsense relations / arguments / infons for 
            // introduced relations in the translation. *However* these should not appear in derivations (TODO confirm that!)
            translatedRelations.[derivableRelationName] <- stripPrefixedFormula(kv.Key)
            program.AddDeclarationPart(RelationDeclarationPart(rd))
            relationSignatures.["D" + kv.Value.ToString()] <- signature

        // create a Datalog fact for each hypothesis
        for hypothesis in hypotheses do
            let pf = prefixedPointer.[hypothesis]
            let form, matter = formPointer.[pf], matterPointer.[pf]
            let relation = "D" + leaderIds.[form].ToString()
            let args = matterToDatalogTerms matter
            program.AddRulePart(CommentRulePart(hypothesis.ToString()))
            program.AddRulePart(RulePart(AtomRule(Relation(relation, args))))
            program.AddRulePart(NewLineRulePart)

        // create Datalog rules for each rule in the PIV derivation calculus 
        for pf in HashSet<PrefixedInfonFormula>(formPointer.Values) do
            // basic info used in all the rules
            let u = leaderIds.[pf]
            let pm, fm = matterPointer.[pf]
            let pref, s = manyArgs pm.Length "p", manyArgs fm.Length "s"

            // rules are syntax directed
            match pf with
            | AndPrefixedFormula(p, pf1, pf2) -> 
                let v1, v2 = leaderIds.[pf1], leaderIds.[pf2]
                let (_, fm1), (_, fm2) = matterPointer.[pf1], matterPointer.[pf2]
                let s1, s2 = manyArgs fm1.Length "l", manyArgs fm2.Length "r"
                // rules for /\e
                program.AddRulePart(CommentRulePart("/\e"))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+v1.ToString(), pref @ s1), 
                                                        [Relation("D"+u.ToString(), pref @ s1 @ s2)])))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+v2.ToString(), pref @ s2), 
                                                        [Relation("D"+u.ToString(), pref @ s1 @ s2)])))
                program.AddRulePart(NewLineRulePart)
                // rule for /\i
                program.AddRulePart(CommentRulePart("/\i"))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+u.ToString(), pref @ s1 @ s2), 
                                                        [Relation("D"+v1.ToString(), pref @ s1); 
                                                        Relation("D"+v2.ToString(), pref @ s2)])))
                program.AddRulePart(NewLineRulePart)
            | ImpliesPrefixedFormula(p, pf1, pf2) ->
                let v1, v2 = leaderIds.[pf1], leaderIds.[pf2]
                let (_, fm1), (_, fm2) = matterPointer.[pf1], matterPointer.[pf2]
                let s1, s2 = manyArgs fm1.Length "l", manyArgs fm2.Length "r"
                // rule for ->e
                program.AddRulePart(CommentRulePart("->e"))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+v2.ToString(), pref @ s2), 
                                                        [Relation("D"+v1.ToString(), pref @ s1); 
                                                        Relation("D"+u.ToString(), pref @ s1 @ s2)])))
                program.AddRulePart(NewLineRulePart)
                // rule for ->i
                program.AddRulePart(CommentRulePart("->i"))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+u.ToString(), pref @ s1 @ s2), 
                                                        [Relation("D"+v2.ToString(), pref @ s2)])))
                program.AddRulePart(NewLineRulePart)
            | SpeechPrefixedFormula(p, ppal, speech, pf1) ->
                let v = leaderIds.[pf1]
                let s' = s.Tail.Tail
                // wrapping/unwrapping rules
                program.AddRulePart(CommentRulePart("wrap/unwrap"))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+u.ToString(), pref @ [VarTerm("ppal"); VarTerm("speech")] @ s'), 
                                                        [Relation("D"+v.ToString(), pref @ [VarTerm("ppal"); VarTerm("speech")] @ s')])))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+v.ToString(), pref @ [VarTerm("ppal"); VarTerm("speech")] @ s'), 
                                                        [Relation("D"+u.ToString(), pref @ [VarTerm("ppal"); VarTerm("speech")] @ s')])))
                program.AddRulePart(NewLineRulePart)
                // prefix deflation
                program.AddRulePart(CommentRulePart("deflation"))
                program.AddRulePart(RulePart(ImpliesRule(Relation("D"+u.ToString(), pref @ [VarTerm("ppal"); AtomTerm(ImpliedSpeech.ToString())] @ s'), 
                                                        [Relation("D"+u.ToString(), pref @ [VarTerm("ppal"); AtomTerm(SaidSpeech.ToString())] @ s')])))
                program.AddRulePart(NewLineRulePart)
            | _ -> ()
            
        // create a Datalog "query" for each thesis
        List.iteri (fun i thesis -> 
        let pf = prefixedPointer.[thesis]
        let form, matter = formPointer.[pf], matterPointer.[pf]
        let u = leaderIds.[form]
        let args = matterToDatalogArgumentsDeclaration matter
        let signature = tr.extractSignature(matter)
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration("Query"+i.ToString(), args, PrintTuples)))
        relationSignatures.["Query"+i.ToString()] <- signature
        program.AddRulePart(CommentRulePart(thesis.ToString()))
        program.AddQueryPart(RulePart(ImpliesRule(Relation("Query"+i.ToString(), matterToDatalogTerms matter), 
                                                    [Relation("D"+u.ToString(), matterToDatalogTerms matter)])))
        program.AddRulePart(NewLineRulePart)
        ) theses
        
        let sorts= new Dictionary<Sort, Mapping<System.Object>>()
        let qMap= new Mapping<System.Object>()
        let cMap= new Mapping<System.Object>()
        quotationsMapping.Values |> Seq.iter (fun q -> qMap.Add(q :> System.Object))
        constantsMapping.Values |> Seq.iter (fun c -> cMap.Add(c :> System.Object))
        sorts.["Q"] <- qMap
        sorts.["C"] <- cMap
        program.Sorts <- sorts
        program
