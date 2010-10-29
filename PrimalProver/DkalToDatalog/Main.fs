namespace Microsoft.Research.Dkal2Datalog

open Datalog
open DkalPrimalProver
open TranslatorInfonVars
open TranslatorObjectVars

open System.Collections.Generic
open System.IO
open System

module Main = 
  
  do
    try
      let args = System.Environment.GetCommandLineArgs() |> Seq.toList
      
      match args with
      | [_; mode; file] ->

        if not (File.Exists (file + ".primal")) then
          printfn "File not found: %O.primal" file
        else
          let sr = new StreamReader(file + ".primal")
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

          let program = match mode.ToLower() with
                        | "infonvars" -> TranslatorInfonVars(file).ConstructProgram assumptions queries
                        | "objectvars" -> TranslatorObjectVars(file).ConstructProgram assumptions queries
                        | _ -> failwith "Expecting either InfonVars or ObjectVars for the <mode> argument"
          printfn "%O" program
      | p :: _ -> printfn "Usage: %O <mode> <file>\n\nWhere <mode> is either InfonVars or ObjectVars.\nThe file argument must NOT include the extension '.primal'"  p
      | _ -> failwith "impossible"

    with e -> printfn "Unhandled exception: %O" e
