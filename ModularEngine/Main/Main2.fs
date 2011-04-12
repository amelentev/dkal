namespace Microsoft.Research.Dkal.SimpleRouter

open System.IO

open Microsoft.Research.Dkal.Factories
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Primitives

module Main =

  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  try  
    match args with
    | [_; file] ->

      if not (File.Exists (file)) then
        printfn "File not found: %O" file
      else
        let parser, printer = ParserFactory.Parser "typed", PrettyPrinterFactory.Printer "typed"
        let router = RouterFactory.Router "simple" file parser printer
        router.Receive (fun mt from -> printfn "%A said: %A" from mt)
        router.Start()

        router.Send (App(primitives.["asInfon"], [Const(BoolConstant(true))])) (Const(PrincipalConstant("Lars")))

        System.Console.ReadLine() |> ignore
        router.Stop()

    | _ -> failwith "Expecting a single parameter"
  with e -> 
    printfn "%O" e





