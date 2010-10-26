namespace Microsoft.Research.GeneralPDP.XACML.PDP

open Microsoft.Research.GeneralPDP.XACML.Ast
open Basics
open Functions

open System.Collections.Generic

module Matchs =

  /// An match function environment which stores XACML match definitions used in targets
  let matchEnv = new Dictionary<string, FunctionF>()

  // copy the functions from the function environemnt
  for kvp in funcEnv do
    matchEnv.Add(kvp.Key, kvp.Value)


