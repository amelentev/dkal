namespace Microsoft.Research.Dkal2Datalog

open Datalog
open Utils
open VarsCounter
open Mapping
open DkalPrimalProver

open System.Collections.Generic

module TranslatorObjectVars = 

  type Names = 
    static member PrimalRel (v: int) = "Primal_" + v.ToString()
    static member FunctionRel (v: int) = "Function_" + v.ToString()
    static member AndRel (vl: int) (vr: int) = "And_" + vl.ToString() + "_" + vr.ToString()
    static member ImpliesRel (vl: int) (vr: int) = "Implies_" + vl.ToString() + "_" + vr.ToString()
    static member SaidRel (v: int) = "Said_" + v.ToString()
    static member ImpliedRel (v: int) = "Implied_" + v.ToString()
    static member CompatibleRel (v: int) = "Unify_" + v.ToString()
    static member QueryRel (i: int) = "Query_" + i.ToString()

  type TranslatorObjectVars(filename: string) = 
    // to accumulate the translated rules, declarations, ...
    let program = new Program()

    // will store information for datalog sorts
    let subformulas = new Mapping<Infon>()
    let quotations = new Mapping<string>()
    let constants = new Mapping<string>()
    let functions = new Mapping<string>()

    // to store the ppal names
    let ppals = new HashSet<string>()

    // stores the maximum quotation depth in all the subformulas
    let mutable maxQuotationDepth = 0

    // to store the number of variables in each syntactic form
    let varsCounter = new VarsCounter(numberOfVarsConsts)

    let rec computeSyntaxRules (i: Infon) = 
      match i with
      | :? Plus as p ->
        let vl, vr = varsConsts (p.getLeft()), varsConsts (p.getRight())
        program.AddRulePart(RulePart(AtomRule(Relation(Names.AndRel vl.Length vr.Length, 
                                                atomArgs [p.getLeft().ToString()] @ vl @ atomArgs [p.getRight().ToString()] @ vr @ atomArgs [p.ToString()]))))
      | :? Implies as i ->
        let vl, vr = varsConsts (i.getLeft()), varsConsts (i.getRight())
        program.AddRulePart(RulePart(AtomRule(Relation(Names.ImpliesRel vl.Length vr.Length, 
                                                atomArgs [i.getLeft().ToString()] @ vl @ atomArgs [i.getRight().ToString()] @ vr @ atomArgs [i.ToString()]))))
      | :? SaidImplied as si ->
        let ppal = si.getPrincipal().getName()
        ppals.Add(ppal) |> ignore
        constants.Add(ppal)
        quotations.Add("Said-" + ppal)
        quotations.Add("Implied-" + ppal)
        let v = varsConsts (si.getKnowledge())
        let relName = match si with 
                      | :? Said as s -> Names.SaidRel v.Length
                      | :? Implied as i -> Names.ImpliedRel v.Length
                      | _ -> failwith "impossible"
        let ppalArg = if si.getPrincipal().isVar() then
                        VarTerm (ppal.ToLower())
                      else
                        AtomTerm ppal
        program.AddRulePart(RulePart(AtomRule(Relation(relName, 
                                                [ppalArg] @ atomArgs [si.getKnowledge().ToString()] @ v @ atomArgs [si.ToString()]))))
      | :? Function as f ->
        if f.getArguments().Count = 0 then
          constants.Add(f.ToString())
        else
          let args = varsConsts f
          program.AddRulePart(RulePart(AtomRule(Relation(Names.FunctionRel (f.getArguments().Count), atomArgs [f.getName()] @ args @ atomArgs [f.ToString()]))))
          functions.Add(f.getName())
          for a in f.getArguments() do
            match a with
            | :? Function as g -> constants.Add(g.getName().ToString())
            | _ -> ()
      | _ -> () 

    let addCompatibilityRules () =
      program.AddRulePart(CommentRulePart("Unification rules"))

      program.AddRulePart(RulePart(AtomRule(Relation(Names.CompatibleRel 0, tVarArgs [0] @ tVarArgs [0]))))

      for v in [1 .. varsCounter.MaxVars()] do
          // functions
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ vars @ tVarArgs [1]), 
                                          [Relation(Names.FunctionRel v, fVarArgs [0] @ vars @ tVarArgs [1]);
                                            Relation(Names.FunctionRel v, fVarArgs [0] @ vars @ tVarArgs [0])])))

          // and
          for vl, vr in varsCounter.PlusCombinations(v) do
            let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
            let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]

            let compatibleParts = [Relation(Names.CompatibleRel vl, tVarArgs [2] @ varsLeft @ tVarArgs [4]);
                                    Relation(Names.CompatibleRel vr, tVarArgs [3] @ varsRight @ tVarArgs [5])]
            
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ varsLeft @ varsRight @ tVarArgs [1]), 
                                          [Relation(Names.AndRel vl vr, tVarArgs [2] @ varsLeft @ tVarArgs [3] @ varsRight @ tVarArgs [0]);
                                            Relation(Names.AndRel vl vr, tVarArgs [4] @ varsLeft @ tVarArgs [5] @ varsRight @ tVarArgs [1])] @
                                              compatibleParts)))

          // implies
          for vl, vr in varsCounter.ImpliesCombinations(v) do
            let varsLeft = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. vl-1]
            let varsRight = List.map (fun i -> VarTerm("var" + (i + vl).ToString())) [0 .. vr-1]

            let compatibleParts = [Relation(Names.CompatibleRel vl, tVarArgs [2] @ varsLeft @ tVarArgs [4]);
                                    Relation(Names.CompatibleRel vr, tVarArgs [3] @ varsRight @ tVarArgs [5])]

            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel v, tVarArgs [0] @ varsLeft @ varsRight @ tVarArgs [1]), 
                                          [Relation(Names.ImpliesRel vl vr, tVarArgs [2] @ varsLeft @ tVarArgs [3] @ varsRight @ tVarArgs [0]);
                                            Relation(Names.ImpliesRel vl vr, tVarArgs [4] @ varsLeft @ tVarArgs [5] @ varsRight @ tVarArgs [1])] @
                                              compatibleParts)))
    
      // said / implied
      for v in varsCounter.SaidCombinations() do
        if v >= 1 then
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel (v+1), tVarArgs [0] @ pVarArgs [0] @ vars @ tVarArgs [1]),
                                        [Relation(Names.SaidRel v, pVarArgs [0] @ tVarArgs [2] @ vars @ tVarArgs [0]);
                                          Relation(Names.SaidRel v, pVarArgs [0] @ tVarArgs [3] @ vars @ tVarArgs [1]);
                                          Relation(Names.CompatibleRel v, tVarArgs [2] @ vars @ tVarArgs [3])])))
          program.AddRulePart(RulePart(ImpliesRule(Relation(Names.CompatibleRel (v+1), tVarArgs [0] @ pVarArgs [0] @ vars @ tVarArgs [1]),
                                        [Relation(Names.ImpliedRel v, pVarArgs [0] @ tVarArgs [2] @ vars @ tVarArgs [0]);
                                          Relation(Names.ImpliedRel v, pVarArgs [0] @ tVarArgs [3] @ vars @ tVarArgs [1]);
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
                                            Relation(Names.SaidRel v, [AtomTerm ppal] @ tVarArgs [0] @ vars @ tVarArgs [1])])))
            leftArgs.[maxQuotationDepth-1] <- AtomTerm("Implied-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [0]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [1]); 
                                            Relation(Names.ImpliedRel v, [AtomTerm ppal] @ tVarArgs [0] @ vars @ tVarArgs [1])])))

        for v in varsCounter.SaidCombinations() do
          let vars = List.map (fun i -> VarTerm("var" + i.ToString())) [0 .. v-1]
          let leftArgs = List.toArray (removeLast (AtomTerm("_np") :: qArgs))
          let rightArgs = List.toArray qArgs
          for ppal in ppals do
            rightArgs.[maxQuotationDepth-1] <- AtomTerm("Said-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [1]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [0]);
                                            Relation(Names.SaidRel v, [AtomTerm ppal] @ tVarArgs [0] @ vars @ tVarArgs [1])])))
            rightArgs.[maxQuotationDepth-1] <- AtomTerm("Implied-" + ppal)
            program.AddRulePart(RulePart(ImpliesRule(Relation(Names.PrimalRel v, Array.toList leftArgs @ vars @ tVarArgs [1]), 
                                          [Relation(Names.PrimalRel v, Array.toList rightArgs @ vars @ tVarArgs [0]);
                                            Relation(Names.ImpliedRel v, [AtomTerm ppal] @ tVarArgs [0] @ vars @ tVarArgs [1])])))

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

     
      program.AddRulePart(NewLineRulePart)

    let addRuleDeclarations () = 
      // compatible
      for v in varsCounter.MaxCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "C")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.CompatibleRel v, 
                                                                                [("t0", "T")] @ vars @ [("t1", "T")], Input)))
      // function
      for v in varsCounter.MaxCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "C")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.FunctionRel v, [("f", "F")] @ vars @ [("t", "T")], Input)))

      // and
      for vl, vr in varsCounter.PlusCombinations() do
        let leftVars = List.map (fun i -> ("vl" + i.ToString(), "C")) [0..vl-1]
        let rightVars = List.map (fun i -> ("vr" + i.ToString(), "C")) [0..vr-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.AndRel vl vr, 
                                                                                [("t0", "T")] @ leftVars @ [("t1", "T")] @ rightVars @ [("t01", "T")], Input)))

      // implies
      for vl, vr in varsCounter.ImpliesCombinations() do
        let leftVars = List.map (fun i -> ("vl" + i.ToString(), "C")) [0..vl-1]
        let rightVars = List.map (fun i -> ("vr" + i.ToString(), "C")) [0..vr-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.ImpliesRel vl vr, 
                                                                                [("t0", "T")] @ leftVars @ [("t1", "T")] @ rightVars @ [("t01", "T")], Input)))

      // primal
      let quotations = List.map (fun i -> ("q" + i.ToString(), "Q")) [0..maxQuotationDepth-1]
      for v in varsCounter.MaxCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "C")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.PrimalRel v, 
                                                                                quotations @ vars @ [("t", "T")], Input)))
   
      // said
      for v in varsCounter.SaidCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "C")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.SaidRel v, 
                                                                                [("p", "C"); ("t", "T")] @ vars @ [("st", "T")], Input)))

      // implied
      for v in varsCounter.ImpliedCombinations() do
        let vars = List.map (fun i -> ("v" + i.ToString(), "C")) [0..v-1]
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.ImpliedRel v, 
                                                                                [("p", "C"); ("t", "T")] @ vars @ [("it", "T")], Input)))


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

        // if instantiations are necessary add a rule on which ground terms look like instantiated vars
        let varsConsts = varsConsts copy
        program.AddRulePart(RulePart(AtomRule(Relation(Names.PrimalRel varsConsts.Length, qArgs @ varsConsts @ atomArgs [copy.ToString()]))))
      program.AddRulePart(NewLineRulePart)

      // add sort decls
      program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("Q", quotations.Count, filename + "_quotations.map")))
      program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("T", subformulas.Count, filename + "_subformulas.map")))
      program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("C", constants.Count, filename + "_constants.map")))
      program.AddDeclarationPart(SortDeclarationPart(SortDeclaration("F", functions.Count, filename + "_functions.map")))
      program.AddDeclarationPart(NewLineDeclarationPart)
       
      // add queries as special relations (and declare them)
      program.AddRulePart(CommentRulePart("Queries"))
      Seq.iteri (fun i query -> 
        let copy = cloneInfon query
        copy.removePrefix()
        let vars = varsConsts copy
        let args = List.mapi (fun i _ -> ("var" + i.ToString(), "C")) vars
        program.AddDeclarationPart(RelationDeclarationPart(RelationDeclaration(Names.QueryRel i, args @ [("t", "T")], PrintTuples)))
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
      constants.ToMapFile(filename + "_constants.map")
      functions.ToMapFile(filename + "_functions.map")

      // return constructed program
      program

