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

using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;

namespace DkalTranslator
{
    /* Translate a list of specifications to Z3 LISP syntax
    */
    public class Z3Translator : DKALTranslator
    {

        public Z3Translator()
        {
        }

        /* To comply with the interface, not really used.
         */
        public String translate(List<Spec> specList)
        {
            return translate(specList, new List<string>(), new Dictionary<string, ArrayList>(), new List<FunctionValDefinition>());
        }

        /* Outputs a String with the translation from a list of assertions (specList) to the Z3 LISP syntax.
         * The additional parameters are for declaration and type checking purposes.
         */
        public String translate(List<Spec> specList, List<string> principalList, Dictionary<string, ArrayList> functionList,
                List<FunctionValDefinition> fixedValues)
        {
            StringBuilder sb = new StringBuilder();

            //Declarations of principals and functions
            sb.Append(":extrafuns (");

            if (principalList.Count > 0)
            {
                foreach (string s in principalList)
                {
                    sb.Append("(" + s + " Principal) ");
                }
            }
            if (functionList.Count > 0)
            {
                foreach (KeyValuePair<string, ArrayList> kv in functionList)
                {
                    sb.Append("(" + kv.Key + " ");
                    for (int i = 0; i < kv.Value.Count; i++)
                    {
                        sb.Append(kv.Value[i] + " ");
                    }
                    sb.Append(") ");
                }

            }
            sb.Append(")\n");

            //Built-in functions for integers
            sb.Append("\n:assumption (forall (i Int) (j Int) (= (less i j) (< i j)) )");
            sb.Append("\n:assumption (forall (i Int) (j Int) (= (greater i j) (> i j)) )");

            //Fixed values for functions
            foreach (FunctionValDefinition f in fixedValues)
            {
                //Checks if matchs the declaration
                if (functionList.ContainsKey(f.getFunctionName()))
                {
                    ArrayList declaredArgs;
                    functionList.TryGetValue(f.getFunctionName(), out declaredArgs);
                    if (declaredArgs.Count != f.getArguments().Count + 1)
                    {
                        throw new Exception("The function " + f.getFunctionName() + " found in Fix declaration was previously specified with a different number of arguments");
                    }
                    else
                    {
                        sb.Append("\n:assumption (= (" + f.getFunctionName() + " ");
                        foreach (string arg in f.getArguments())
                        {
                            sb.Append(arg + " ");
                        }
                        sb.Append(") ");
                        sb.Append(f.getValue() + " )");
                    }
                }
                else
                {
                    throw new Exception("The function " + f.getFunctionName() + " found in Fix declaration was not declared");
                }

            }

            sb.Append("\n");

            //Specification
            foreach (Spec s in specList)
            {
                translate(sb, s);
            }
            return sb.ToString();
        }

        void translate(StringBuilder sb, Spec s)
        {
            string dest = null;
            if (s is Fact)
            {
                sb.Append(":assumption (");
                universalize(sb, s);
                translateFact(sb, (Fact)s);
            }
            if (s is Query)
            {
                sb.Append(":formula (");
                universalize(sb, s);
                translateQuery(sb, (Query)s);
            }
            if (s is Communication)
            {
                sb.Append(":assumption (");
                universalize(sb, s);
                translateComm(sb, (Communication)s);
                //Retrieve the free principal, in case that exists
                if (((Communication)s).getDest().isVar())
                {
                    dest = ((Communication)s).getDest().getName();
                }
            }

            //Closes the last parenthesis in case a forall was added
            List<Variable> varList = getFreeVariables(s);
            if (varList.Count() > 0 || dest != null)
            {
                sb.Append(")");
            }

            sb.Append(")\n");
        }

        /* Puts a universal quantifier in front of the formula for every free variable
         */
        void universalize(StringBuilder sb, Spec s)
        {
            List<Variable> varList = getFreeVariables(s);

            //Retrieve the free principal, in case that exists
            string dest = null;
            if (s is Communication)
            {
                Communication comm = (Communication)s;
                if (comm.getDest().isVar())
                {
                    dest = comm.getDest().getName();
                }
            }

            if (varList.Count() > 0 || dest != null)
            {
                sb.Append("forall ");
                if (dest != null)
                {
                    sb.Append("(" + dest + " Principal) ");
                }
                foreach (Variable var in varList)
                {
                    sb.Append("(" + var.getName() + " " + var.getType() + ") ");
                }
                sb.Append("(");
            }
        }

        List<Variable> getFreeVariables(Spec s)
        {
            List<Variable> varList;
            //Retrieve the free variables of the construction
            if (s is Fact)
            {
                varList = ((Fact)s).getKnowledge().getVariableList();
            }
            else if (s is Query)
            {
                varList = ((Query)s).getFact().getKnowledge().getVariableList();
            }
            else
            {
                Communication comm = (Communication)s;
                varList = comm.getMessage().getVariableList();
                if (comm.hasProviso())
                {
                    varList.AddRange(comm.getProviso().getVariableList());
                }
            }
            return varList;
        }

        void translateFact(StringBuilder sb, Fact s)
        {
            sb.Append("knows ");
            sb.Append(s.getPrincipal().getName());
            sb.Append(" ");
            translateInfon(sb, s.getKnowledge());
        }
        void translateQuery(StringBuilder sb, Query s)
        {
            sb.Append("not (");
            translateFact(sb, s.getFact());
            sb.Append(")");
        }

        void translateComm(StringBuilder sb, Communication s)
        {
            if (s is To)
            {
                sb.Append("to ");
            }
            else
            {
                sb.Append("from ");
            }

            //TO p1 p2 
            //FROM p1 p2
            sb.Append(s.getSource().getName() + " " + s.getDest().getName() + " ");

            //The conditional is always present (trueInfon by default)
            if (s.hasProviso())
            {
                sb.Append("(provisoPresentCond ");
                translateInfon(sb, s.getMessage());
                sb.Append(" ");
                translateInfon(sb, s.getProviso());
                sb.Append(" ");
                translateInfon(sb, s.getConditional());
                sb.Append(")");
            }
            else
            {
                //This is a hack for encoding reasons, avoid instantiation
                if (s.getMessage() is Variable)
                {
                    sb.Append(((Variable)s.getMessage()).getName());
                }
                else
                {
                    sb.Append("(provisoFreeCond ");
                    translateInfon(sb, s.getMessage());
                    sb.Append(" ");
                    translateInfon(sb, s.getConditional());
                    sb.Append(")");
                }
            }

        }

        void translateInfon(StringBuilder sb, Infon s)
        {
            if (s is TrueInfon)
            {
                sb.Append("trueInfon");
            }
            if (s is Plus)
            {
                sb.Append("(plus ");
                translateInfon(sb, ((Plus)s).getLeft());
                sb.Append(" ");
                translateInfon(sb, ((Plus)s).getRight());
                sb.Append(")");
            }
            if (s is Implies)
            {
                sb.Append("(imp ");
                translateInfon(sb, ((Implies)s).getLeft());
                sb.Append(" ");
                translateInfon(sb, ((Implies)s).getRight());
                sb.Append(")");
            }
            if (s is Function)
            {
                Function f = (Function)s;
                sb.Append("(");
                sb.Append(f.getName());
                sb.Append(" ");
                foreach (Infon infon in f.getArguments())
                {
                    translateInfon(sb, infon);
                    sb.Append(" ");
                }
                sb.Append(")");
            }
            if (s is SaidPut)
            {
                SaidPut sp = (SaidPut)s;
                if (sp is Said)
                {
                    sb.Append("(said");
                }
                else
                {
                    sb.Append("(put");
                }
                sb.Append(" " + sp.getPrincipal().getName() + " ");
                translateInfon(sb, sp.getKnowledge());
                sb.Append(")");
            }
            if (s is Variable)
            {
                sb.Append(((Variable)s).getName());
            }
        }
    }
}
