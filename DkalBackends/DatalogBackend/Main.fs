namespace Microsoft.Research.DkalBackends.DatalogBackend

open Microsoft.Research.DkalBackends.Ast
open Microsoft.Research.DkalBackends.Parsing
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator
open Utils
open Datalog

open System.Collections.Generic
open System.IO
open System

module Main = 
  
  do
    try
      let args = System.Environment.GetCommandLineArgs() |> Seq.toList
      let translator= new DatalogTranslator()
      match args with
      | [_; file] ->
        if not (File.Exists (file)) then
          printfn "File not found: %O" file
        else
          let contents = File.ReadAllText(file)
          let (hypotheses, theses) = parseInferenceProblem contents
          let datalogProgram= translator.translateInferenceProblem( (hypotheses, theses) )

          // output Datalog program and mapping files            
          printfn "%O" datalogProgram
          translator.QuotationsMapping.ToMapFile("quotations.map")
          translator.ConstantsMapping.ToMapFile("constants.map")

      | p :: _ -> printfn "Usage: %O <file>"  p
      | _ -> failwith "impossible"

    with e -> printfn "Unhandled exception: %O" e
