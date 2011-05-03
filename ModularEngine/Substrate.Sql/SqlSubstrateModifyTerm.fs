namespace Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces

type SqlSubstrateModifyTerm(ns: string, query: ITerm, colsMapping) = 

  interface ISubstrateUpdateTerm with
    ()
    
