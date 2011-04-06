module Microsoft.Research.Dkal.Main

open System.IO
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Factories

do
  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  try  
    match args with
    | [_; file] ->

      if not (File.Exists (file)) then
        printfn "File not found: %O" file
      else
        let s = File.ReadAllText(file)
        let parser, printer = ParserFactory.Parser "simple", PrettyPrinterFactory.Printer "simple"
        let assembly = parser.ParseAssembly s
        printfn "%O" <| printer.PrintAssembly assembly

    | _ -> failwith "Expecting a single parameter"
  with e -> 
    printfn "%O" e

