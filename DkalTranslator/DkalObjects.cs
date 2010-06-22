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
 * These objects are then taken by a some translator (Z3 or Formula) to produce the final translation.
 */
namespace DkalTranslator
{
    
    /* Base class for facts, queries and communications
     */
    public abstract class Spec { 
    
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

        public Principal getPrincipal() {
            return this.principal;
        }

        public Infon getKnowledge() {
            return this.knowledge;
        }
    }

    /*?Principal knows knowledge
     */
    public class Query : Spec {
        Fact fact;

        public Query(Fact fact) {
            this.fact = fact;
        }

        public Fact getFact() {
            return this.fact;
        }
    }

    /*Base class for TO and FROM communication assertions
     */
    public class Communication : Spec {
        Principal principalSource;
        Principal principalDest;
        Proviso body;
        Infon conditional;

        public Communication() { }

        public Communication(Principal principalSource, Principal principalDest, Proviso body, Infon conditional) {
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

        public Principal getSource() {
            return principalSource;
        }

        public Principal getDest()
        {
            return principalDest;
        }

        public Infon getConditional() {
            return conditional;
        }

        public Infon getMessage() {
            return body.getMessage();
        }

        public bool hasProviso() {
            return body.hasProviso();
        }

        public Infon getProviso() {
            return this.body.getProviso();
        }
    }

    /*Principal TO Principal:[message] <= condition
     */
    public class To : Communication {

        public To(Principal principalSource, Principal principalDest, Proviso body, Infon conditional) 
            : base(principalSource, principalDest, body, conditional){
        }

        public To(Principal principalSource, Principal principalDest, Proviso body)
            : base(principalSource, principalDest, body)
        {
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
    }

    /* A message, with an optional proviso
     */
    public class Proviso {
        Infon message;
        Infon proviso;

        public Proviso(Infon main, Infon proviso) {
            this.message = main;
            this.proviso = proviso;
        }

        public Proviso(Infon main)
        {
            this.message = main;
        }

        public Infon getMessage() { 
            return message;
        }

        public Infon getProviso(){
            return proviso;
        }

        public bool hasProviso(){
            return proviso != null;
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

        public string getName() {
            return name;
        }

        public bool isVar() {
            return isVariable;
        }
    }

    /* Base class for Infons. An infon has a list of free variables that are used in the translation
     */
    public abstract class Infon
    {
        //Free variables of an Infon
        protected System.Collections.Generic.List<Variable> varList = new System.Collections.Generic.List<Variable>();

        public Infon() { 
        
        }

        public System.Collections.Generic.List<Variable> getVariableList()
        {
            return varList;
        }
        
    }


    public class TrueInfon : Infon
    {
    }

    /* The type of a variable is filled during parsing.
     */
    public class Variable : Infon
    {
        string name;
        string type;

        public Variable(string name) {
            this.name = name;
            this.getVariableList().Add(this);
        }

        public string getName() {
            return this.name;
        }

        public void setType(string type) {
            this.type = type;
        }

        public string getType() {
            return this.type;
        }
    }
    
    public class Plus : Infon
    {
        Infon left;
        Infon right;
    
        public Plus(Infon left, Infon right) {
            this.left = left;
            this.right = right;
            this.getVariableList().AddRange(left.getVariableList());
            this.getVariableList().AddRange(right.getVariableList());
        }

        public Infon getLeft(){
            return this.left;
        }

        public Infon getRight(){
            return this.right;
        }

    }

    public class Implies : Infon
    {
        Infon left;
        Infon right;

        public Implies(Infon left, Infon right) {
            this.left = left;
            this.right = right;
            this.getVariableList().AddRange(left.getVariableList());
            this.getVariableList().AddRange(right.getVariableList());
        }

        public Infon getLeft()
        {
            return this.left;
        }

        public Infon getRight()
        {
            return this.right;
        }

    
    }

    /*Base class for Said and Put
     */
    public class SaidPut : Infon { 
        Principal principal;
        Infon knowledge;

        public SaidPut() { }

        public SaidPut(Principal principal, Infon knowledge)
        {
            this.principal = principal;
            this.knowledge = knowledge;
            this.getVariableList().AddRange(knowledge.getVariableList());
        }

        public SaidPut(Infon knowledge)
        {
            this.knowledge = knowledge;
        }
        
        public void setPrincipal(Principal principal) {
            this.principal = principal;
        
        }

        public Principal getPrincipal() {
            return this.principal;
        }
        public Infon getKnowledge() {
            return this.knowledge;
        }
    
    }

    public class Said : SaidPut
    {
        public Said(){}

        public Said(Principal principal, Infon knowledge)
            : base(principal, knowledge)
        {
            
        }
        public Said(Infon knowledge)
            : base(knowledge)
        {

        }
    }
    
    public class Put : SaidPut
    {
        public Put(){}

        public Put(Principal principal, Infon knowledge)
            : base(principal, knowledge)
        {
            
        }
        public Put(Infon knowledge)
            : base(knowledge)
        {
            
        }
    }
    
    /*A function has a name and a list of arguments
     */
    public class Function : Infon {
        string name;
        System.Collections.ArrayList arguments;

        public Function(string constant) {
            this.name = constant;
            this.arguments = new System.Collections.ArrayList();
        }

        public Function(string name, System.Collections.ArrayList arguments) {
            this.name = name;
            this.arguments = arguments;
            foreach (Infon arg in arguments) {
                this.getVariableList().AddRange(arg.getVariableList());
            }
        }

        public string getName() {
            return this.name;
        }
        public ArrayList getArguments() {
            return this.arguments;
        }
    }

    /* This class is here because C# CUP does not allow ">" and "<" in the specification file.
     * Acts only as a synonym
     */
    public class SpecList : System.Collections.Generic.List<Spec> { 
    }

    /* Idem
     */
    public class StringList : System.Collections.Generic.List<string> {
    }

    /* Idem
    */
    public class FunctionDictionary : System.Collections.Generic.Dictionary<string, ArrayList> {
    }

    /* Idem
    */
    public class FixedFunctionList : System.Collections.Generic.List<FunctionValDefinition> {
    }

    /* This is to correct a bug in C# CUP. After generating the CUP files, mStack must be replaced
     * with dstack in parser.cs
     */
    public class dstack : System.Collections.Stack
    {
        public dstack(Stack origin) : base(origin) { }
        public object elementAt(int index)
        {
            /*            string tmp = (string)((Symbol)(base.ToArray()[index])).value;*/
            return base.ToArray()[index];
        }
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

}
