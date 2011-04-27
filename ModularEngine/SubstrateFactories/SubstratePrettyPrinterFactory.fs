namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SqlSubstrate

open System.Collections.Generic

type SubstratePrettyPrinterFactory() =
  
  static let printers = new Dictionary<System.Type*string, System.Type>()

  static member RegisterPrettyPrinter (substrateType: System.Type) (kind: string) (printerType: System.Type) =
    printers.[(substrateType, kind)] <- printerType

  static member SubstratePrettyPrinter (s: ISubstrate) (kind: string) =  
    if printers.ContainsKey (s.GetType(), kind) then
      let spt = printers.[(s.GetType(), kind)]
      let sp = spt.GetConstructor([||]).Invoke([||]) :?> ISubstratePrettyPrinter
      sp
    else
      failwithf "Error while creating a substrate pretty printer: unknown substrate type/kind combination %O %O" (s.GetType()) kind
