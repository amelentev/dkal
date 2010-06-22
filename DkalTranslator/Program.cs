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
using System.IO;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using TUVienna.CS_CUP.Runtime;

/* This class takes a dkal spec file and translate it to either Z3 or formula syntax, adding the appropriate background theory
 * (which should be provided)
 */
namespace DkalTranslator
{

    /* Parse a dkal specification and generates a translation to Z3 or formula syntax, adding the necesary background theory.
     */
    class DkalParser
    {

        Parser parser;
        Lexicon lexicon;

        static void Main(string[] args)
        {

            if (args.Length < 3)
            {
                Console.WriteLine("Not enough arguments");
                printUsage();
                return;
            }

            try
            {
                String encodingFile;
                String inputFile;
                String outputFile;
                bool isZ3Format = true;

                //parse the argument line
                if (args[0].ToString().StartsWith("--")){
                    //options are present
                    //until now, the only option is "format"
                    if (args[0] != "--format"){
                        Console.WriteLine("Invalid option " + args[0]);
                        return;
                    }
                    if (args[1]=="formula"){
                        isZ3Format = false;
                    }
                    else if (args[1] != "Z3"){
                        Console.WriteLine("The format options are Z3 or formula");
                        return;
                    }
                    if (args.Length < 5){
                        Console.WriteLine("Not enough arguments");
                        printUsage();
                        return;
                    }
                    encodingFile = args[2];
                    inputFile = args[3];
                    outputFile = args[4];
                }
                else{
                    encodingFile = args[0];
                    inputFile = args[1];
                    outputFile = args[2];
                }

                //Parse the file
                TextReader re = new StreamReader(inputFile);
                DkalParser tr = new DkalParser(re);

                List<Spec> specList = tr.parse();

                
                DKALTranslator translator;
                if (isZ3Format){
                    translator = new Z3Translator();
                }
                else{
                    translator = new FormulaTranslator();
                }

                string translation = translator.translate(specList, tr.getPrincipalList(), tr.getFunctionList(), tr.getFixedFunctions());

                //Now we retrieve the encoding and put both things together
                TextReader encodingReader = new StreamReader(encodingFile);
                string encoding = encodingReader.ReadToEnd();



                //delete the last parenthesis
                encoding = encoding.Trim();
                encoding = encoding.Remove(encoding.Length - 1);

                //insert the translation
                encoding = encoding + "\n" + translation + ")";

                TextWriter writer = new StreamWriter(outputFile);
                writer.Write(encoding);
                
                writer.Close();
                re.Close();
                encodingReader.Close();

                Console.ReadKey();

            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
                Console.ReadKey();
            }

        }

        static void printUsage()
        {
            Console.WriteLine("Usage: DkalTranslator [options] <encoding-file> <input-file> <output-file>");
            Console.WriteLine("options: --format {Z3, formula} (default is Z3)");
        }

        
        public DkalParser(TextReader sr)
        {

            lexicon = new Lexicon(sr);
            this.parser = new Parser(lexicon);
            
        }

        /* Parse the input file and returns a list of specifications
         */
        public List<Spec> parse()
        {

            try
            {
                Symbol result = this.parser.parse();
                return (List<Spec>)result.value;
            }
            catch (Exception e) {
                throw new Exception("Syntax error in line " + lexicon.getCurrentLineNumber(), e);
            }

        }

        /*This is the list of declared principal, for translation purposes
         */
        public List<string> getPrincipalList(){
            return parser.getPrincipalList();
        }

        /*This is the list of declared funcions, for translation purposes
         */
        public Dictionary<string, ArrayList> getFunctionList() {
            return parser.getFunctions();
        }

        /* This represents the value that the specified functions should return for
         * a set of fixed arguments (the Fix declaration in dkal syntax)
         */
        public List<FunctionValDefinition> getFixedFunctions() {
            return parser.getFixedFunctions();
        }

    }



    /*This is the interface of a translator.
     */
    public interface DKALTranslator
    {
        /*Arguments: 
         * specList: list of assertions (facts, queries or comms), 
         */
        String translate(List<Spec> specList);
        
        /*Arguments: 
         * specList: list of assertions (facts, queries or comms), 
         * principalList: list of principals declared, 
         * functionList: dictionary that maps functionName -> list of types of declared functions
         * fixedValues: list of FunctionValDefinition, that represent the defined value of functions for a fixed set of arguments
         */
        String translate(List<Spec> specList, List<string> principalList, Dictionary<string, ArrayList> functionList, 
                        List<FunctionValDefinition> fixedValues);

    }
    
}
