namespace Microsoft.Research.Dkal2Datalog

open Datalog
open DkalPrimalProver
open Translator

open System.Collections.Generic
open System.IO
open System

module Main = 
  
  do
    try
      let args = System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail
      
      let sr = new StreamReader(args.[0] + ".primal")
      let lexicon = new Lexicon(sr)
      let parser = new Parser(lexicon)

      let assumptions = new List<Infon>()
      let queries = new List<Infon>()

      let result = parser.parse()
      match result.value with
      | :? List<Spec> as specs ->
        for spec in specs do
          match spec with 
          | :? Fact as fact ->
            assumptions.Add(fact.getKnowledge())  
          | :? Query as query ->
            queries.Add(query.getFact().getKnowledge())
          | _ -> failwith "found an unsupported spec element"
      | _ -> failwith "expecting spec list"

      let program = Translator(args.[0]).ConstructProgram assumptions queries
      printfn "%O" program
    
    with e -> printfn "Unhandled exception: %O" e
