// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Interfaces

/// IExecutor provides an interface for the engine driver. IExecutor 
/// implementations are in charge of keeping a policy and apply all the rules
/// as their conditions are triggered
type IExecutor =

  /// Must be called to initialize the executor
  abstract member Start: unit -> unit 

  /// Must be called when the executor is no longer needed
  abstract member Stop: unit -> unit 

  /// Installs the given rule ITerm on the executor. The rule is followed from 
  /// this point onwards. Returns false if the rule was already installed
  abstract member InstallRule: ITerm -> bool
    
  /// Uninstalls the given rule ITerm on the executor. Returns true if the rule 
  /// was not present
  abstract member UninstallRule: ITerm -> bool

  // ----- Callbacks ----- 

  /// Registers a callback to be invoked whenever the executor reaches a local
  /// fixed-point
  abstract member FixedPointCallback: cb: (unit -> unit) -> unit

  /// Registers a callback to be invoked whenever the executor wakes up after
  /// a fixed-point
  abstract member WakeUpCallback: cb: (unit -> unit) -> unit

  /// Registers a callback to be invoked whenever an action is about to be executed
  abstract member ActionCallback: cb: (ITerm -> unit) -> unit

  /// Registers a callback to be invoked whenever a message is received
  abstract member ReceiveCallback: cb: (ITerm -> ITerm -> unit) -> unit

  /// Registers a callback to be invoked whenever an execution round is about to start
  abstract member RoundStartCallback: cb: (unit -> unit) -> unit
