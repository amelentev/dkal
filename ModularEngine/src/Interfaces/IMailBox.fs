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

// Provides an interface for a repository of incoming infon ITerms
type IMailBox =
  
  /// Adds the given infon ITerm to the mailbox
  abstract member Add: infon: ITerm -> from: ITerm -> unit

  /// Removes the given infon ITerm from the mailbox
  abstract member Remove: ITerm * ?a: ITerm list -> unit

  /// Eliminates "old" messages, each mailbox decides how to implement this
  /// operation, which should be invoked at the end of each execution round
  abstract member Prune: unit -> unit

  /// Match an infon to messages in the mailbox. It returns a subset of
  /// (possibly specialized) substitutions. 
  abstract member Matches: infon: ITerm -> from: ITerm -> substs: ISubstitution seq -> ISubstitution seq
