using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace DkalPrimalProver
{
    /* Given a set of assumptions and queries, the solver outputs which subset of the queries can be derived from the assumptions
     * This is an implementation of the primal infon algorithm
     */
    class PrimalSolver
    {
        //We use arrays for efficiency reasons.
        //input parameters of the solving algorithm
        Infon[] assumptions;
        Infon[] queries;

        //Assumptions and queries after the regularization process.
        //Each regularized formula has the same position in the list than the original one
        Infon[] regularAssumptions;
        Infon[] regularQueries;

        //regularAssumptions U regularQueries
        Infon[] inputFormulas;

        //homonymy originals
        HashSet<Infon> subformulas;

        //Table of records to be processed
        Dictionary<Infon, PrimalRecord> table;

        /* Constructs a solver from assumptions and queries
         */
        public PrimalSolver(List<Infon> assumptions, List<Infon> queries) 
        {
            this.assumptions = assumptions.ToArray();
            this.queries = queries.ToArray();
        }

        /* Returns the subset of queries that can be derived from assumptions.
         */
        public List<Infon> solve()
        {
            //Removes T and axioms and expands macros. This fills regularAssumptions and regularQueries
            regularize();

            //We do the union between regular queries and regular assumptions
            inputFormulas = new Infon[regularAssumptions.Length + regularQueries.Length];
            for (int i = 0; i < regularAssumptions.Length; i++)
            {
                inputFormulas[i] = regularAssumptions[i];
            }
            for (int i = regularAssumptions.Length; i < regularAssumptions.Length + regularQueries.Length; i++)
            {
                inputFormulas[i] = regularQueries[i - regularAssumptions.Length];
            }

            //Computes the homonymy originals and graft extra nodes. This fills the subformula instance variable.
            computeSubformulas();

            //We flatten regularAssumptions and regularQueries, to get rid of prefixes. This put them in sync with the entries
            //that are going to be filled in the table. The inputFormulas instance remains with the prefixes for the preprocessing
            //stage
            for (int i = 0; i < regularAssumptions.Length; i++)
            {
                regularAssumptions[i] = (Infon)regularAssumptions[i].Clone();
                regularAssumptions[i].removePrefix();
                //freeze hash here
            }
            for (int i = 0; i < regularQueries.Length; i++)
            {
                regularQueries[i] = (Infon)regularQueries[i].Clone();
                regularQueries[i].removePrefix();
                //freeze hash here
            }

            //We process the entries in the table
            HashSet<Infon> pendingRecords =  preprocess();

            //In this point, regularAssumptions, regularQueries and inputFormulas has no prefix information.
            //The only references to the original parse tree are in the static fields (in the sense that they don't change)
            //of the entries in the table

            //We go through each pending record, while we have.
            while (pendingRecords.Count > 0)
            {
                HashSet<Infon> newPendingRecords = new HashSet<Infon>();

                //We process every pending record of the table
                foreach (Infon pendingInfon in pendingRecords)
                {
                    newPendingRecords.UnionWith(fireInferenceRules(pendingInfon));

                    //After this, the entry is processed
                    table[pendingInfon].status = Status.processed;
                }
                //We update the new pending records
                pendingRecords = newPendingRecords;
            }
            
            List<Infon> result = new List<Infon>();

            //We traverse the regular queries. For each one, we check if its entry in the 
            //table is processed (or if it is True)
            //But we have to return original formulas, not the regularized ones
            for (int i = 0; i < regularQueries.Length; i++)
            {
                if (table[regularQueries[i]].status == Status.processed || regularQueries[i].Equals(new TrueInfon()))
                {
                    result.Add(queries[i]);
                }
            }

            return result;
        }

        /*Removes T for non T-equivalent formulas. Formulas that are equivalent to T are reduced to T.
         * Also expands macros as tdonP and tdonS
         * This methods fills the arrays regularAssumptions and regularQueries
         */
        private void regularize()
        {
            this.regularAssumptions = new Infon[assumptions.Length];

            //We regularize all the assumptions.
            for(int i = 0 ; i < assumptions.Length; i++)
            {
                Infon regular = regularizeFormula(assumptions[i]);
                regularAssumptions[i] = regular;
            }

            //We regularize all the queries. 
            this.regularQueries = new Infon[queries.Length];
            for (int i = 0; i < queries.Length; i++ )
            {
                Infon regular = regularizeFormula(queries[i]);
                regularQueries[i] = regular;
            }
        }

        //Removes the occurrence of T and axiom (pref T). In case the reduced formula 
        //is T or an axiom, it returns T
        //We also expand macros as tdonP and tdonS
        //With the base cases we return new instances to have fresh prefixes (otherwise, aliasing could be a problem)
        private Infon regularizeFormula(Infon i)
        {
            if (i is PlusOrImplies)
            {
                //We regularice each side
                PlusOrImplies p = (PlusOrImplies)i;
                Infon right = regularizeFormula(p.getRight());
                Infon left = regularizeFormula(p.getLeft());

                //If both are true, we return true
                if ((right is TrueInfon) && (left is TrueInfon))
                {
                    return right;
                }
                //If one of the two is True, we return the other one
                else if (p.getLeft() is TrueInfon)
                {
                    return right;
                }
                else if (p.getRight() is TrueInfon)
                {
                    return left;
                }
                //If none of the two is true, we return the conjunt of the regularization of each side
                else
                {
                    if (p is Plus)
                    {
                        return new Plus(left, right);
                    }
                    else
                    {
                        return new Implies(left, right);
                    }
                }
            }
            else if (i is SaidImplied)
            {
                SaidImplied p = (SaidImplied)i;
                Infon knowledge = p.getKnowledge();
                //We regularlice the knowledge
                knowledge = regularizeFormula(knowledge);
                //If the knowledge is True, we transform "told True" in "True"
                if (knowledge is TrueInfon)
                {
                    return knowledge;
                }
                //If not, we just return the previous infon with the knowledge regularized
                else 
                {
                    if (p is Said)
                    {
                        return new Said(p.getPrincipal(), knowledge);
                    }
                    else
                        return new Implied(p.getPrincipal(), knowledge);
                }
            }
            //For now, we only have the macros tdonP and tdonS
            else if (i is Function)
            {
                Function function = (Function)i;
                if (function.getName().Equals(Resources.TdonSName))
                {
                    Principal p = new Principal(((Function)(function.getArguments()[0])).getName());
                    Infon j = (Infon)function.getArguments()[1];
                    return new Implies(new Said(p, j), j);
                }
                if (function.getName().Equals(Resources.TdonIName))
                {
                    Principal p = new Principal(((Function)(function.getArguments()[0])).getName());
                    Infon j = (Infon)function.getArguments()[1];
                    return new Implies(new Implied(p, j), j);
                }
                else 
                {
                    return new Function(function.getName(), function.getArguments());
                }
            }
            else if (i is TrueInfon)
            {
                return new TrueInfon();
            }
            else if (i is Variable)
            {
                Variable var = (Variable)i;
                return new Variable(var.getName());
            }
            
            //This should never happen
            throw new Exception("Unexpected type " + i);
        }

        /* Given an infon i, splits the maximal prefix of i with the form "p told1 p told2..." and the tail (which it is not a "told"). i = prefix tail
         * It does not take into consideration the "prefix" instance of the input infon.
         */
        private void splitPrefixTail(Infon infon, out Infon prefix, out Infon tail)
        {
            //If the infon starts with a told, we recurse into the infons knowledge
            if (infon is SaidImplied)
            {
                SaidImplied saidImplied = (SaidImplied)infon;
                splitPrefixTail(saidImplied.getKnowledge(), out prefix, out tail);
                //We build the prefix
                if (saidImplied is Said)
                {
                    prefix = new Said(saidImplied.getPrincipal(), prefix, new EndPrefix());
                }
                else
                {
                    prefix = new Implied(saidImplied.getPrincipal(), prefix, new EndPrefix());
                }
            }
            else
            {
                //If it is not a told, the the prefix is empty, and all the infon is the tail
                prefix = new EndPrefix();
                tail = infon;
            }
        }

        //Fire each of the inference rules for the primal fragment. Return the new pending records
        private HashSet<Infon> fireInferenceRules(Infon infon)
        {
            HashSet<Infon> pendingRecords = new HashSet<Infon>();
            PrimalRecord record = table[infon];

            //We split the infon in its prefix and tail. The tail is always plus, implies, a function or True
            Infon prefix;
            Infon tail;
            splitPrefixTail(infon, out prefix, out tail);

            //Rul Pref deflation
            HashSet<Infon> derivedPrefixes = Infon.getPrefixes(prefix);
            foreach (Infon derivedPref in derivedPrefixes)
            { 
                Infon infonWithPrefix = tail.getInfonWithThisPrefix(derivedPref);
                if (table[infonWithPrefix].status == Status.raw)
                {
                    table[infonWithPrefix].status = Status.pending;
                    pendingRecords.Add(infonWithPrefix);
                }
            }

            //Rule ^e
            if (tail is Plus)
            {
                Plus plus = (Plus)tail;
                Infon leftWithPrefix = plus.getLeft().getInfonWithThisPrefix(prefix);
                Infon rightWithPrefix = plus.getRight().getInfonWithThisPrefix(prefix);

                if (table[leftWithPrefix].status == Status.raw)
                {
                    table[leftWithPrefix].status = Status.pending;
                    pendingRecords.Add(leftWithPrefix);
                }
                if (table[rightWithPrefix].status == Status.raw)
                {
                    table[rightWithPrefix].status = Status.pending;
                    pendingRecords.Add(rightWithPrefix);
                }
            }

            //Rule ^i left
            foreach (Plus i in record.plusLeft)
            {
                Infon plusWithPrefix = i.getInfonWithPrefix();
                Infon rightWithPrefix = i.getRight().getInfonWithPrefix();

                if ((table[rightWithPrefix].status != Status.raw) && (table[plusWithPrefix].status == Status.raw))
                {
                    table[plusWithPrefix].status = Status.pending;
                    pendingRecords.Add(plusWithPrefix);
                }
            }

            //Rule ^i right
            foreach (Plus i in record.plusRight)
            {
                Infon plusWithPrefix = i.getInfonWithPrefix();
                Infon leftWithPrefix = i.getLeft().getInfonWithPrefix();

                if ((table[leftWithPrefix].status != Status.raw) && (table[plusWithPrefix].status == Status.raw))
                {
                    table[plusWithPrefix].status = Status.pending;
                    pendingRecords.Add(plusWithPrefix);
                }
            }

            //Rule ->e left
            foreach (Implies i in record.impLeft)
            {
                Infon impWithPrefix = i.getInfonWithPrefix();
                Infon rightWithPrefix = i.getRight().getInfonWithPrefix();

                if ((table[impWithPrefix].status != Status.raw) && (table[rightWithPrefix].status == Status.raw))
                {
                    table[rightWithPrefix].status = Status.pending;
                    pendingRecords.Add(rightWithPrefix);
                }
            }

            //Rule ->e right
            if (tail is Implies)
            {
                Implies imp = (Implies)tail;

                Infon leftWithPrefix = imp.getLeft().getInfonWithThisPrefix(prefix);
                Infon rightWithPrefix = imp.getRight().getInfonWithThisPrefix(prefix);

                if ((table[leftWithPrefix].status != Status.raw) && (table[rightWithPrefix].status == Status.raw))
                {
                    table[rightWithPrefix].status = Status.pending;
                    pendingRecords.Add(rightWithPrefix);
                }
            }

            //Rule ->i
            foreach (Implies i in record.impRight)
            {
                Infon impWithPrefix = i.getInfonWithPrefix();

                if (table[impWithPrefix].status == Status.raw)
                {
                    table[impWithPrefix].status = Status.pending;
                    pendingRecords.Add(impWithPrefix);
                }
            }

            return pendingRecords;
        }

        //Fills the set of subformulas, so every subformula is only once
        private void computeSubformulas() 
        {
            HashSet<Infon> subformulasWithPrefixes = new HashSet<Infon>();

            //We iterate all input formulas and for each one explore its subformulas
            //We add all the subformulas to the hashset, so we have a unique copy of each one.
            foreach (Infon i in inputFormulas)
            {
                foreach (Infon i2 in i.getSubformulas())
                    subformulasWithPrefixes.UnionWith(i.getSubformulas());
            }

            //Adds to the subformulas the extra prefixes
            HashSet<Infon> grafts = new HashSet<Infon>();
            foreach (Infon i in subformulasWithPrefixes)
            {
                 foreach (Infon i2 in i.getGraftedInfons())
                    grafts.UnionWith(i.getGraftedInfons());
            }
            subformulasWithPrefixes.UnionWith(grafts);

            //Now we iterate through the subformulas and we flatten the prefixes, so the entries in the table (that are the subforms)
            //are infons with empty prefixes. This is because in the table we are going to use infons with empty prefixes, and the subformulas
            //index the table
            subformulas = new HashSet<Infon>();
            foreach (Infon i in subformulasWithPrefixes)
            {
                Infon j = i.getInfonWithPrefix();
                subformulas.Add(j);

            }
        }

        /*Fills the table with one record per homonymy original. Returns the pending records
         *Returns the initial set of pending records. These records are the assumptions.
         *In the fields of the table the original nodes of the parse tree are stored (with the 
         *appropriate prefixes). After the table is filled, the prefixes are removed from the input formulas
         *to keep them in sync with the entries in the table
         */
        private HashSet<Infon> preprocess() 
        {
            table = new Dictionary<Infon,PrimalRecord>();

            HashSet<Infon> pendingRecords = new HashSet<Infon>();

            //There is a record for each homonymy original
            foreach (Infon i in subformulas)
            {
                PrimalRecord entry = new PrimalRecord();
                table[i] = entry;
            }

            //We compute the static fields of the entries. We traverse the parse tree.
            foreach (Infon rootNode in inputFormulas)
            {
                processInfon(rootNode);
            }

            //We flatten the input formulas, so they are in sync with subformulas
            //We make a copy to keep the table with this information (in the static fields of each entry)
            for (int i = 0; i < inputFormulas.Length; i++)
            {
                inputFormulas[i] = (Infon)inputFormulas[i].Clone();
                inputFormulas[i].removePrefix();
                //freeze hash here
            }

            //We set the assumptions to pending status
            foreach (Infon assumption in regularAssumptions)
            {
                table[assumption].status = Status.pending;
                pendingRecords.Add(assumption);
            }


            return pendingRecords;
        }

        /* Traverses the parse tree of an infon, filling the fields of the table.
         * For the case of a plus or an implies, the grafted prefixes are taken into consideration, 
         * so all the related entries in the table are filled up.
         * The fields are filled with references to the original parse tree, and that includes prefix information
         */
        private void processInfon(Infon i)
        {
            if (i is Plus)
            {
                Plus plus = (Plus)i;
                Infon left = plus.getLeft();
                Infon right = plus.getRight();
                //For each child, we consider all the derived prefixes.
                foreach (Infon j in left.getGraftedInfons()) 
                {
                    table[j.getInfonWithPrefix()].plusLeft.Add(plus);
                }
                foreach (Infon j in right.getGraftedInfons())
                {
                    table[j.getInfonWithPrefix()].plusRight.Add(plus);
                }

                processInfon(left);
                processInfon(right);

            }
            else if (i is Implies)
            {
                Implies implies = (Implies)i;
                Infon left = implies.getLeft();
                Infon right = implies.getRight();
                //For each child, we consider all the derived prefixes.
                foreach (Infon j in left.getGraftedInfons())
                {
                    table[j.getInfonWithPrefix()].impLeft.Add(implies);
                }
                foreach (Infon j in right.getGraftedInfons())
                {
                    table[j.getInfonWithPrefix()].impRight.Add(implies);
                }

                processInfon(left);
                processInfon(right);
            }
            else if (i is SaidImplied)
            {
                SaidImplied saidImplied = (SaidImplied)i;
                processInfon(saidImplied.getKnowledge());
            }

            //If it is not a plus, an implies or a SaidImplied, then there is nothing to do
        }

        public override string ToString()
        {
            String ret = "PrimalSolver";
            if (table != null)
            {
                ret += ":\n";
                foreach (Infon f in table.Keys)
                {
                    ret += table[f].ToString() + ": ";
                    ret += f.ToString() + "\n";
                }
            }
            return ret;
        }
    }

    /* A record can be in status raw, pending of processed
     */
    enum Status { raw, pending, processed  };

    /* This is the value of each table entry. 
     * It is composed by the status of the entry and 4 fields. In these fields the position of the current infon with respect
     * to other infons in the parse tree is stored. These records are filled in the preprocessing stage.
     * This is just a plain structure. It is a class just because fields initialization is handy, but everything is public.
     */
    class PrimalRecord
    {
        public Status status = Status.raw;
        public HashSet<Plus> plusLeft = new HashSet<Plus>();
        public HashSet<Plus> plusRight = new HashSet<Plus>();
        public HashSet<Implies> impLeft = new HashSet<Implies>();
        public HashSet<Implies> impRight = new HashSet<Implies>();

        public override string ToString()
        {
            return status.ToString();
        }
    }
}
