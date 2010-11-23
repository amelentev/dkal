namespace Microsoft.Research.Dkal2Datalog

open Datalog
open Utils
open VarsCounter
open Mapping
open DkalPrimalProver

open System.Collections.Generic

module TranslatorInfonVars = 

  type Names = 
    static member PrimalRel (v: int) = "Primal_" + v.ToString()
    static member AndRel (vl: int) (vr: int) = "And_" + vl.ToString() + "_" + vr.ToString()
    static member ImpliesRel (vl: int) (vr: int) = "Implies_" + vl.ToString() + "_" + vr.ToString()
    static member SaidRel (ppal: string) (v: int) = "Said_" + ppal + "_" + v.ToString()
    static member ImpliedRel (ppal: string) (v: int) = "Implied_" + ppal + "_" + v.ToString()
    static member ConstRel = "IsConst"
    static member VarRel = "IsVar"
    static member CompatibleRel (v: int) = "Unify_" + v.ToString()
    static member QueryRel (i: int) = "Query_" + i.ToString()

  type TranslatorInfonVars(filename: string) = 
    // to accumulate the translated rules, declarations, ...
    let program = new Program()

    // will store information for datalog sorts
    let subformulas = new Mapping<Infon>()
    let quotations = new Mapping<string>()

    // to store the ppal names
    let ppals = new HashSet<string>()

    // stores the maximum quotation depth in all the subformulas
    let mutable maxQuotationDepth = 0

    // to store the number of variables in each syntactic form
    let varsCounter = new VarsCounter(numberOfVars)

    let rec computeSyntaxRules (i: Infon) = 
      match i with
      | :? Plus as p ->
        let vl, vr = List.map VarTerm (vars (p.getLeft())), List.map VarTerm (vars (p.getRight()))
        program.AddRulePart(RulePart(AtomRule(Relation(Names.AndRel vl.Length vr.Length, 
                                                atomArgs [p.getLeft().ToString()] @ vl @ atomArgs [p.getRight().ToString()] @ vr @ atomArgs [p.ToString()]))))
      | :? Implies as i ->
        let vl, vr = List.map VarTerm (vars (i.getLeft())), List.map VarTerm (vars (i.getRight()))
        program.AddRulePart(RulePart(AtomRule(Relation(Names.ImpliesRel vl.Length vr.Length, 
                                                atomArgs [i.getLeft().ToString()] @ vl @ atomArgs [i.getRight().ToString()] @ vr @ atomArgs [i.ToString()]))))
      | :? SaidImplied as si ->
        let ppal = si.getPrincipal().getName()
        ppals.Add(ppal) |> ignore
        quotations.Add("Said-" + ppal)
        quotations.Add("Implied-" + ppal)
        let v = List.map VarTerm (vars (si.getKnowledge()))
        let relName = match si with 
                      | :? Said as s -> Names.SaidRel ppal v.Length
                      | :? Implied as i -> Names.ImpliedRel ppal v.Length
                      | _ -> failwith "impossible"
        program.AddRulePart(RulePart(AtomRule(Relation(relName, 
                                                atomArgs [si.getKnowledge().ToString()] @ v @ atomArgs [si.ToString()]))))
      | :? Function as f ->
        program.AddRulePart(RulePart(AtomRule(Relation(Names.ConstRel, 
                                                atomArgs [f.ToString()]))))
      | :? Variable as v ->
        program.AddRulePart(RulePart(AtomRule(Relation(Names.VarRel, 
                                                atomArgs [v.ToString()]))))
      | _ -> () 


    let addCompatibilityRules () =
      program.AddRulePart(CommentRulePart("Unification rules"))
      for v in [0 .. varsCounter.MaxVars()] do
      
        let wildcards = List.replicate v WildcardTerm
        program.AddRulePart(RulePart(AtomRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ wildcards @ tVarArgs [0]))))
      
        if v = 1 then
          // variable instantiation
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0; 1; 1]),
                                        [Relation(Names.VarRel, tVarArgs [0])])))

        if v >= 1 then
          // and
          for vl, vr in varsCounter.PlusCombinations(v) do
            let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
            let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]

            let compatibleParts = [Relation(Names.CompatibleRel vl, tVarArgs [2] @ varsLeft @ tVarArgs [4]);
                                    Relation(Names.CompatibleRel vr, tVarArgs [3] @ varsRight @ tVarArgs [5])]

            for kl, kr in varsCounter.PlusCombinations() do
              let kVarsLeft = extendArgs kl varsLeft
              let kVarsRight = extendArgs kr varsRight
              program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ varsLeft @ varsRight @ tVarArgs [1]), 
                                            [Relation(Names.AndRel vl vr, tVarArgs [2] @ varsLeft @ tVarArgs [3] @ varsRight @ tVarArgs [0]);
                                              Relation(Names.AndRel kl kr, tVarArgs [4] @ kVarsLeft @ tVarArgs [5] @ kVarsRight @ tVarArgs [1])] @
                                                compatibleParts)))

          // implies
          for vl, vr in varsCounter.ImpliesCombinations(v) do
            let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
            let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]

            let compatibleParts = [Relation(Names.CompatibleRel vl, tVarArgs [2] @ varsLeft @ tVarArgs [4]);
                                    Relation(Names.CompatibleRel vr, tVarArgs [3] @ varsRight @ tVarArgs [5])]

            for kl, kr in varsCounter.ImpliesCombinations() do
              let kVarsLeft = extendArgs kl varsLeft
              let kVarsRight = extendArgs kr varsRight
              program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ varsLeft @ varsRight @ tVarArgs [1]), 
                                            [Relation(Names.ImpliesRel vl vr, tVarArgs [2] @ varsLeft @ tVarArgs [3] @ varsRight @ tVarArgs [0]);
                                              Relation(Names.ImpliesRel kl kr, tVarArgs [4] @ kVarsLeft @ tVarArgs [5] @ kVarsRight @ tVarArgs [1])] @
                                                compatibleParts)))
    
      // said / implied
      for v in varsCounter.SaidCombinations() do
        if v >= 1 then
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          for kv in varsCounter.SaidCombinations() do
            let kVars = extendArgs kv vars 
            for ppal in ppals do
              program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ vars @ tVarArgs [1]),
                                            [Relation(Names.SaidRel ppal v, tVarArgs [2] @ vars @ tVarArgs [0]);
                                              Relation(Names.SaidRel ppal kv, tVarArgs [3] @ kVars @ tVarArgs [1]);
                                              Relation(Names.CompatibleRel v, tVarArgs [2] @ vars @ tVarArgs [3])])))
              program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ vars @ tVarArgs [1]),
                                            [Relation(Names.ImpliedRel ppal v, tVarArgs [2] @ vars @ tVarArgs [0]);
                                              Relation(Names.ImpliedRel ppal kv, tVarArgs [3] @ kVars @ tVarArgs [1]);
                                              Relation(Names.CompatibleRel v, tVarArgs [2] @ vars @ tVarArgs [3])])))
      program.AddRulePart(NewLineRulePart)


    let addDerivationRules () =
      program.AddRulePart(CommentRulePart("Derivability rules"))
      let qArgs = qVarArgs [0 .. maxQuotationDepth-1]
    
      for v in [0 .. varsCounter.MaxVars()] do
      
        for vl, vr in varsCounter.PlusCombinations(v) do
          let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
          let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]
          // And introduction
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, qArgs @ varsLeft @ varsRight @ tVarArgs [5]), 
                                        [Relation(Names.CompatibleRel vl, tVarArgs [0] @ varsLeft @ tVarArgs [3]);
                                          Relation(Names.CompatibleRel vr, tVarArgs [1] @ varsRight @ tVarArgs [4]);
                                          Relation(Names.CompatibleRel v, tVarArgs [2] @ varsLeft @ varsRight @ tVarArgs [5]);
                                          Relation(Names.PrimalRel vl, qArgs @ varsLeft @ tVarArgs [3]); 
                                          Relation(Names.PrimalRel vr, qArgs @ varsRight @ tVarArgs [4]); 
                                          Relation(Names.AndRel vl vr, tVarArgs [0] @ varsLeft @ tVarArgs [1] @ varsRight @ tVarArgs[2])])))
          // And eliminations
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel vl, qArgs @ varsLeft @ tVarArgs [3]), 
                                        [Relation(Names.CompatibleRel vl, tVarArgs [0] @ varsLeft @ tVarArgs [3]);
                                          Relation(Names.CompatibleRel vr, tVarArgs [1] @ varsRight @ tVarArgs [4]);
                                          Relation(Names.CompatibleRel v, tVarArgs [2] @ varsLeft @ varsRight @ tVarArgs [5]);
                                          Relation(Names.PrimalRel v, qArgs @ varsLeft @ varsRight @ tVarArgs [5]); 
                                          Relation(Names.AndRel vl vr, tVarArgs [0] @ varsLeft @ tVarArgs [1] @ varsRight @ tVarArgs[2])])))
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel vr, qArgs @ varsRight @ tVarArgs [4]), 
                                        [Relation(Names.CompatibleRel vl, tVarArgs [0] @ varsLeft @ tVarArgs [3]);
                                          Relation(Names.CompatibleRel vr, tVarArgs [1] @ varsRight @ tVarArgs [4]);
                                          Relation(Names.CompatibleRel v, tVarArgs [2] @ varsLeft @ varsRight @ tVarArgs [5]);
                                          Relation(Names.PrimalRel v, qArgs @ varsLeft @ varsRight @ tVarArgs [5]); 
                                          Relation(Names.AndRel vl vr, tVarArgs [0] @ varsLeft @ tVarArgs [1] @ varsRight @ tVarArgs[2])])))
      
        for vl, vr in varsCounter.ImpliesCombinations(v) do
          let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
          let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]
          // Implies introduction
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, qArgs @ varsLeft @ varsRight @ tVarArgs [5]), 
                                        [Relation(Names.CompatibleRel vl, tVarArgs [0] @ varsLeft @ tVarArgs [3]);
                                          Relation(Names.CompatibleRel vr, tVarArgs [1] @ varsRight @ tVarArgs [4]);
                                          Relation(Names.CompatibleRel v, tVarArgs [2] @ varsLeft @ varsRight @ tVarArgs [5]);
                                          Relation(Names.PrimalRel vr, qArgs @ varsRight @ tVarArgs [4]); 
                                          Relation(Names.ImpliesRel vl vr, tVarArgs [0] @ varsLeft @ tVarArgs [1] @ varsRight @ tVarArgs [2])])))
          // Implies elimination
          for currVl in [vl..v] do
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel vr, qArgs @ varsRight @ tVarArgs [4]), 
                                        [Relation(Names.CompatibleRel vl, tVarArgs [0] @ varsLeft @ tVarArgs [3]);
                                          Relation(Names.CompatibleRel vr, tVarArgs [1] @ varsRight @ tVarArgs [4]);
                                          Relation(Names.CompatibleRel v, tVarArgs [2] @ varsLeft @ varsRight @ tVarArgs [5]);
                                          Relation(Names.PrimalRel vl, qArgs @ varsLeft @ tVarArgs [3]); 
                                          Relation(Names.PrimalRel v, qArgs @ varsLeft @ varsRight @ tVarArgs [5]); 
                                          Relation(Names.ImpliesRel vl vr, tVarArgs [0] @ varsLeft @ tVarArgs [1] @ varsRight @ tVarArgs [2])])))

      // Prefix injection and ejection
      if maxQuotationDepth > 0 then
        for v in varsCounter.SaidCombinations() do
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          let leftArgs = List.toArray qArgs
          let rightArgs = List.toArray (removeLast (AtomTerm("_np") :: qArgs))
          for ppal in ppals do
            leftArgs.[maxQuotationDepth-1] <- AtomTerm("Said-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [0]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [1]); 
                                            Relation(Names.SaidRel ppal v, tVarArgs [0] @ vars @ tVarArgs [1])])))
            leftArgs.[maxQuotationDepth-1] <- AtomTerm("Implied-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [0]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [1]); 
                                            Relation(Names.ImpliedRel ppal v, tVarArgs [0] @ vars @ tVarArgs [1])])))

        for v in varsCounter.SaidCombinations() do
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          let leftArgs = List.toArray (removeLast (AtomTerm("_np") :: qArgs))
          let rightArgs = List.toArray qArgs
          for ppal in ppals do
            rightArgs.[maxQuotationDepth-1] <- AtomTerm("Said-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [1]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [0]);
                                            Relation(Names.SaidRel ppal v, tVarArgs [0] @ vars @ tVarArgs [1])])))
            rightArgs.[maxQuotationDepth-1] <- AtomTerm("Implied-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [1]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [0]);
                                            Relation(Names.ImpliedRel ppal v, tVarArgs [0] @ vars @ tVarArgs [1])])))

      // Said -> Implied
      let leftArgs = List.toArray qArgs
      let rightArgs = List.toArray qArgs
      for v in varsCounter.SaidCombinations() do
        let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
        for i in [0 .. maxQuotationDepth-1] do
          for ppal in ppals do
            leftArgs.[i] <- AtomTerm("Implied-" + ppal)
            rightArgs.[i] <- AtomTerm("Said-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [0]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [0])])))
            leftArgs.[i] <- qArgs.[i]
            rightArgs.[i] <- qArgs.[i]

      // Satisfaction by compatibility
      for v1 in varsCounter.MaxCombinations() do
        for v2 in [v1+1..varsCounter.MaxVars()] do
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v1-1]
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v1, qArgs @ vars @ tVarArgs [0]), 
                                        [Relation(Names.PrimalRel (v2), qArgs @ (List.replicate (v2) WildcardTerm) @ tVarArgs [1]);
                                          Relation(Names.CompatibleRel v1, tVarArgs [0] @ vars @ tVarArgs [1])])))

      // Primal deflation/inflation (consider the case on which substituted term has variables itself)
      if varsCounter.MaxVars() > 0 then
        // Instantiate var -> constant
        program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel 0, qArgs @ tVarArgs [1]),
                                      [Relation(Names.PrimalRel 1, qArgs @ tVarArgs [1; 0]); 
                                        Relation(Names.VarRel, tVarArgs [0]);
                                        Relation(Names.ConstRel, tVarArgs [1])])))

        // Instantiate var -> var (renaming)
        program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel 1, qArgs @ [WildcardTerm] @ tVarArgs [1]),
                                      [Relation(Names.PrimalRel 1, qArgs @ tVarArgs [1; 0]); 
                                        Relation(Names.VarRel, tVarArgs [0]);
                                        Relation(Names.VarRel, tVarArgs [1])])))

        // Instantiate said/implied
        for v in varsCounter.SaidCombinations() do
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          for ppal in ppals do
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, qArgs @ vars @ tVarArgs [1]), 
                                          [Relation(Names.PrimalRel 1, qArgs @ tVarArgs [1; 0]); 
                                            Relation(Names.VarRel, tVarArgs [0]);
                                            Relation(Names.SaidRel ppal v, [WildcardTerm] @ vars @ tVarArgs [1])])))
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, qArgs @ vars @ tVarArgs [1]), 
                                          [Relation(Names.PrimalRel 1, qArgs @ tVarArgs [1; 0]); 
                                            Relation(Names.VarRel, tVarArgs [0]);
                                            Relation(Names.ImpliedRel ppal v, [WildcardTerm] @ vars @ tVarArgs [1])])))
      
        // Instantiate and
        for vl, vr in varsCounter.PlusCombinations() do
          let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
          let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel (vl + vr), qArgs @ varsLeft @ varsRight @ tVarArgs [1]), 
                                        [Relation(Names.PrimalRel 1, qArgs @ tVarArgs [1; 0]); 
                                          Relation(Names.VarRel, tVarArgs [0]);
                                          Relation(Names.AndRel vl vr, [WildcardTerm] @ varsLeft @ [WildcardTerm] @ varsRight @ tVarArgs [1])])))

        // Instantiate implies
        for vl, vr in varsCounter.ImpliesCombinations() do
          let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
          let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel (vl + vr), qArgs @ varsLeft @ varsRight @ tVarArgs [1]), 
                                        [Relation(Names.PrimalRel 1, qArgs @ tVarArgs [1; 0]); 
                                          Relation(Names.VarRel, tVarArgs [0]);
                                          Relation(Names.ImpliesRel vl vr, [WildcardTerm] @ varsLeft @ [WildcardTerm] @ varsRight @ tVarArgs [1])])))
      
      program.AddRulePart(NewLineRulePart)

    let addRuleDeclarations () = 
      // var and const
      program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.VarRel, [("t", "T")], Input)))
      program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.ConstRel, [("t", "T")], Input)))

      // compatible
      for v in varsCounter.MaxCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "T")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.CompatibleRel v, 
                                                                                [("t0", "T")] @ vars @ [("t1", "T")], Input)))

      // and
      for vl, vr in varsCounter.PlusCombinations() do
        let leftVars = List.map (fun i -> ("vl" + i.ToString(), "T")) [0..vl-1]
        let rightVars = List.map (fun i -> ("vr" + i.ToString(), "T")) [0..vr-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.AndRel vl vr, 
                                                                                [("t0", "T")] @ leftVars @ [("t1", "T")] @ rightVars @ [("t01", "T")], Input)))

      // implies
      for vl, vr in varsCounter.ImpliesCombinations() do
        let leftVars = List.map (fun i -> ("vl" + i.ToString(), "T")) [0..vl-1]
        let rightVars = List.map (fun i -> ("vr" + i.ToString(), "T")) [0..vr-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.ImpliesRel vl vr, 
                                                                                [("t0", "T")] @ leftVars @ [("t1", "T")] @ rightVars @ [("t01", "T")], Input)))

      // primal
      let quotations = List.map (fun i -> ("q" + i.ToString(), "Q")) [0..maxQuotationDepth-1]
      for v in varsCounter.MaxCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "T")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.PrimalRel v, 
                                                                                quotations @ vars @ [("t", "T")], Input)))
   
      // said
      for v in varsCounter.SaidCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "T")) [0..v-1]
        for ppal in ppals do
          program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.SaidRel ppal v, 
                                                                                [("t", "T")] @ vars @ [("st", "T")], Input)))

      // implied
      for v in varsCounter.ImpliedCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "T")) [0..v-1]
        for ppal in ppals do
          program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.ImpliedRel ppal v, 
                                                                                [("t", "T")] @ vars @ [("it", "T")], Input)))


    // constructs and returns the datalog program for the derivability check of facts |- queries
    member t.ConstructProgram (assumptions: Infon seq) (queries: Infon seq) = 
      // special quotation for "no quotation" / "no principal"
      quotations.Add("_np")

      // compute the subformulas of each fact and query
      let subfs = new HashSet<Infon>()
      for fact in assumptions do
        subfs.UnionWith(computeSubformulas fact)
      for query in queries do
        subfs.UnionWith(computeSubformulas query)
        
      // calculate the maximum quotation depth
      maxQuotationDepth <- maxQuotationDepthOf subfs

      // load the subformulas into the mapping and collect which free variable formula kinds exists
      for i in subfs do
        subformulas.Add(i) |> ignore
        varsCounter.Add(i) 

      // creates atom rules which encode the syntactic structure of each subformula
      program.AddRulePart(CommentRulePart("Syntactic structure"))
      for i in subfs do
        computeSyntaxRules i
      program.AddRulePart(NewLineRulePart)

      // adds rules for the compatibility between formulas 
      addCompatibilityRules()

      // adds the derivation rules
      addDerivationRules()

      // add fact derivability
      program.AddRulePart(CommentRulePart("Facts"))
      for fact in assumptions do
        let qArgs = List.replicate maxQuotationDepth "_np" |> atomArgs
        let copy = cloneInfon fact
        copy.removePrefix()
        // add a rule with variables only
        let vars = List.map VarTerm (vars copy)
        program.AddRulePart(RulePart(AtomRule(Relation(Names.PrimalRel vars.Length, qArgs @ vars @ atomArgs [copy.ToString()]))))

        // if instantiations are necessary add a rule on which ground terms look like instantiated vars
        let varsConsts = varsConstsIV copy 
        if varsCounter.MaxVars() > 0 && varsConsts.Length <> vars.Length then
          program.AddRulePart(RulePart(AtomRule(Relation(Names.PrimalRel varsConsts.Length, qArgs @ varsConsts @ atomArgs [copy.ToString()]))))
      program.AddRulePart(NewLineRulePart)

      // add sort decls
      program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("Q", quotations.Count, filename + "_quotations.map")))
      program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("T", subformulas.Count, filename + "_subformulas.map")))
      program.AddDeclarationPart(NewLineDeclarationPart)
       
      // add queries as special relations (and declare them)
      program.AddRulePart(CommentRulePart("Queries"))
      Seq.iteri (fun i query -> 
        let copy = cloneInfon query
        copy.removePrefix()
        let vars = List.map VarTerm (vars copy)
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.QueryRel i, [for v in vars -> (v.ToString(), "T")] @ [("t", "T")], PrintTuples)))
        let qArgs = List.replicate maxQuotationDepth "_np" |> atomArgs
        program.AddRulePart(RulePart(ImpliesRule(Relation(Names.QueryRel i, vars @ atomArgs [copy.ToString()]), 
                                      [Relation(Names.PrimalRel vars.Length, qArgs @ vars @ atomArgs [copy.ToString()])])))
      ) queries
      program.AddRulePart(NewLineRulePart)

      // add basic decls
      addRuleDeclarations()
            
      // output mapping files
      subformulas.ToMapFile(filename + "_subformulas.map")
      quotations.ToMapFile(filename + "_quotations.map")

      // return constructed program
      program

