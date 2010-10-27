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
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TUVienna.CS_CUP.Runtime;

/* Class definitions for the data structure needed when parsing a dkal spec. They represent
 * each type of dkal constructions.
 * For now, this objects are almost not used, since the primal fragment does not include dkal constructions
 */
namespace DkalPrimalProver
{

    /* Base class for facts, queries and communications
     */
    public abstract class Spec
    {
    }

    /* Principal knows knowledge
     */
    public class Fact : Spec
    {
        Principal principal;
        Infon knowledge;

        public Fact(Principal principal, Infon knowledge)
        {
            this.principal = principal;
            this.knowledge = knowledge;
        }

        public Fact(Infon knowledge)
        {
            this.knowledge = knowledge;
        }

        public Principal getPrincipal()
        {
            return this.principal;
        }

        public Infon getKnowledge()
        {
            return this.knowledge;
        }

        public override String ToString()
        {
            return getPrincipal().ToString() + " knows " + getKnowledge().ToString();
        }
    }

    /*?Principal knows knowledge
     */
    public class Query : Spec
    {
        Fact fact;

        public Query(Fact fact)
        {
            this.fact = fact;
        }

        public Fact getFact()
        {
            return this.fact;
        }

        public override String ToString()
        {
            return "?" + getFact().ToString();
        }
    }

    /*Base class for TO and FROM communication assertions
     */
    public class Communication : Spec
    {

        Principal principalSource;
        Principal principalDest;
        Proviso body;
        Infon conditional;

        public Communication() { }

        public Communication(Principal principalSource, Principal principalDest, Proviso body, Infon conditional)
        {
            this.principalSource = principalSource;
            this.principalDest = principalDest;
            this.body = body;
            this.conditional = conditional;
        }

        public Communication(Principal principalSource, Principal principalDest, Proviso body)
        {
            this.principalSource = principalSource;
            this.principalDest = principalDest;
            this.body = body;
            this.conditional = new TrueInfon();
        }

        public Principal getSource()
        {
            return principalSource;
        }

        public Principal getDest()
        {
            return principalDest;
        }

        public Infon getConditional()
        {
            return conditional;
        }

        public Infon getMessage()
        {
            return body.getMessage();
        }

        public bool hasProviso()
        {
            return body.hasProviso();
        }

        public Infon getProviso()
        {
            return this.body.getProviso();
        }
    }

    /*Principal TO Principal:[message] <= condition
     */
    public class To : Communication
    {
        public To(Principal principalSource, Principal principalDest, Proviso body, Infon conditional)
            : base(principalSource, principalDest, body, conditional)
        {
        }

        public To(Principal principalSource, Principal principalDest, Proviso body)
            : base(principalSource, principalDest, body)
        {
        }

        public override String ToString()
        {
            return getSource().ToString() + " to " + getDest().ToString() +
                ": " + getMessage() + (hasProviso() ? " <= " + getProviso() : "");
        }
    }

    /*Principal FROM Principal:[message] <= condition
     */
    public class From : Communication
    {
        public From(Principal principalSource, Principal principalDest, Proviso body, Infon conditional)
            : base(principalSource, principalDest, body, conditional)
        {
        }

        public From(Principal principalSource, Principal principalDest, Proviso body)
            : base(principalSource, principalDest, body)
        {
        }

        public override String ToString()
        {
            return getSource().ToString() + " from " + getDest().ToString() +
                ": " + getMessage() + (hasProviso() ? " <= " + getProviso() : "");
        }
    }

    /* A message, with an optional proviso
     */
    public class Proviso
    {
        Infon message;
        Infon proviso;

        public Proviso(Infon main, Infon proviso)
        {
            this.message = main;
            this.proviso = proviso;
        }

        public Proviso(Infon main)
        {
            this.message = main;
        }

        public Infon getMessage()
        {
            return message;
        }

        public Infon getProviso()
        {
            return proviso;
        }

        public bool hasProviso()
        {
            return proviso != null;
        }

        public override String ToString()
        {
            return getMessage().ToString() + ", " + getProviso().ToString();
        }
    }

    /*A principal, with a flag that indicates whether it is a free variable (for communication assertions)
     * 
     */
    public class Principal
    {

        String name;
        bool isVariable;

        public Principal(String name)
        {
            this.name = name;
            this.isVariable = false;
        }

        public Principal(String name, bool isVariable)
        {
            this.name = name;
            this.isVariable = isVariable;
        }

        public string getName()
        {
            return name;
        }

        public bool isVar()
        {
            return isVariable;
        }

        public override String ToString()
        {
            return getName();
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Principal))
            {
                return false;
            }
            else
            {
                Principal ppal = (Principal)obj;
                return ppal.getName().Equals(this.getName()) && ppal.isVar() == this.isVar();
            }
        }

        public override int GetHashCode()
        {
            return getName().GetHashCode();
        }
    }


    /* This class is here because C# CUP does not allow ">" and "<" in the specification file.
     * Acts only as a synonym
     */
    public class SpecList : System.Collections.Generic.List<Spec>
    {
    }

    /* Idem
     */
    public class StringList : System.Collections.Generic.List<string>
    {
    }

    /* Idem
    */
    public class FunctionDictionary : System.Collections.Generic.Dictionary<string, ArrayList>
    {
    }

    /* Idem
    */
    public class FixedFunctionList : System.Collections.Generic.List<FunctionValDefinition>
    {
    }

    /* Class that represents the definition of the value of a function for a fixed set of arguments.
     */
    public class FunctionValDefinition
    {
        string functionName;
        List<string> arguments;
        string value;

        public FunctionValDefinition(string functionName, List<string> arguments, string value)
        {
            this.functionName = functionName;
            this.arguments = arguments;
            this.value = value;
        }

        public string getFunctionName()
        {
            return functionName;
        }

        public List<string> getArguments()
        {
            return this.arguments;
        }

        public string getValue()
        {
            return this.value;
        }
    }

    /* This is to correct a bug in C# CUP. After generating the CUP files, mStack must be replaced
     * with dstack in parser.cs
     */
    public class dStack : System.Collections.Stack
    {
        public dStack(Stack origin) : base(origin) { }

        public object elementAt(int index)
        {
            /*            string tmp = (string)((Symbol)(base.ToArray()[index])).value;*/
            return base.ToArray()[index];
        }
    }

}

