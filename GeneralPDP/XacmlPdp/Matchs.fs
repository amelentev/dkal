namespace Microsoft.Research.GeneralPDP.XACML.PDP

open Microsoft.Research.GeneralPDP.XACML.Ast
open Basics
open Functions

open System.Collections.Generic

module Matchs =

  /// An match function environment which stores XACML match definitions used in targets
  let matchEnv = new Dictionary<string, FunctionF>()

  // ------------ Do the match environment filling ------------
  matchEnv.Add("string-equal", valueEq)
  matchEnv.Add("boolean-equal", valueEq)
  matchEnv.Add("integer-equal", valueEq)


