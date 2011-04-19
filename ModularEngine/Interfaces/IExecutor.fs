﻿namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

/// IExecutor provides an interface for the engine driver. IExecutor 
/// implementations are in charge of keeping a policy and apply all the rules
/// as their conditions are triggered
type IExecutor =

  /// Must be called to initialize the executor
  abstract member Start: unit -> unit 

  /// Must be called when the executor is no longer needed
  abstract member Stop: unit -> unit 

  /// Installs the given policy on the executor, all the policy rules are added
  /// and followed from this point onwards
  abstract member InstallPolicy: Policy -> unit
    
    