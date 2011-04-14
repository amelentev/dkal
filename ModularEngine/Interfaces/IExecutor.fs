namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

  type IExecutor =
    abstract member InstallPolicy: Policy -> unit
    
    abstract member Start: unit -> unit 
    abstract member Stop: unit -> unit 
    // TODO
    