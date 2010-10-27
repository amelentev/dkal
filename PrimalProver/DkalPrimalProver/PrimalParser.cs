using System;
using System.IO;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using TUVienna.CS_CUP.Runtime;

namespace DkalPrimalProver
{
    /* The parser for the primal fragment.
     * For now, much of the Dkal parser is reused here, so the parsing is a bit lousy (and not very robust in terms of types)
     * For testing is fine, but it should be defined first what type of things we would like to have in the functions
     * For now, principals and infons are allowed.
     */
    class PrimalParser
    {
        Parser parser;
        Lexicon lexicon;

        /* Parses the input file and creates a PrimalSolver to find out which queries can be
         * inferred from the assumptions. Outputs in the console the result.
         */
        static void Main(string[] args)
        {
            try
            {
                String inputFile;
                inputFile = args[0];

                //Parse the file
                TextReader re = new StreamReader(inputFile);
                PrimalParser tr = new PrimalParser(re);

                List<Spec> specList = tr.parse();

                //For now, we just split Facts and Queries and drop everything else
                List<Infon> assumptions = new List<Infon>();
                List<Infon> queries = new List<Infon>();

                foreach (Spec spec in specList){
                    if (spec is Fact) 
                    {
                        Fact fact = (Fact)spec;

                        assumptions.Add(fact.getKnowledge());
                    }
                    if (spec is Query)
                    {
                        Query query = (Query)spec;
                
                        queries.Add(query.getFact().getKnowledge());
                    }
                }

                //We create the solver and we run it
                PrimalSolver solver = new PrimalSolver(assumptions, queries);
                List<Infon> result;
                result = solver.solve();

                //We print out the assumptions
                Console.WriteLine("Assumptions:");
                foreach (Infon i in assumptions)
                {
                    Console.WriteLine(i);
                }

                //We outputs the queries. For each case we indicate whether or not
                //they could be proved
                Console.WriteLine("\nQueries:");
                foreach (Infon i in queries)
                {

                    Console.Write(i);
                    if (result.Contains(i))
                    {
                        Console.WriteLine(" : ok!");
                    }
                    else 
                    {
                        Console.WriteLine(" : not ok");
                    }
                    
                }

            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }

        }



        public PrimalParser(TextReader sr)
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
                checkConformance();

                return (List<Spec>)result.value;
            }
            catch (Exception e)
            {
                throw new Exception("Syntax error in line " + lexicon.getCurrentLineNumer(), e);
            }
        }

        //Checks types and constraints of the spec
        private void checkConformance()
        {
            //We check the declared function types
            //For now, functions declarations can only have types Principal and Infon
            foreach (ArrayList arguments in parser.getFunctions().Values)
            {
                foreach (string o in arguments) 
                {
                    if (!Resources.AllowedTypes.Contains(o))
                    {
                        throw new Exception("The type " + o + " is not allowed");
                    }
                }
            }
        }


        /*This is the list of declared principal, for translation purposes
         */
        public List<string> getPrincipalList()
        {
            return parser.getPrincipalList();
        }



        /*This is the list of declared funcions, for translation purposes
         */
        public Dictionary<string, ArrayList> getFunctionList()
        {
            return parser.getFunctions();
        }
    }
}
