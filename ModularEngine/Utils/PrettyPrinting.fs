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

namespace Microsoft.Research.Dkal.Utils.PrettyPrinting

  /// Pretty printing tokens are fed to the PrettyPrinter in order to produce
  /// a string representation
  type PrettyPrintToken = 
  | TabToken 
  | UntabToken 
  | NewLineToken 
  | TextToken of string 
  | ManyTokens of List<PrettyPrintToken>

  /// The PrettyPrinter provides functionality to print a list of PrettyPrintTokens
  type PrettyPrinter() =
    static member PrettyPrint ps = 
      let rec doPrettyPrint ps i = 
        match ps, i with
          | [], _ -> ""
          | TabToken::ps, i -> doPrettyPrint ps (i+1)
          | UntabToken::ps, i -> doPrettyPrint ps (i-1)
          | NewLineToken::ps, i -> "\r\n" + (String.replicate i "    ") + doPrettyPrint ps i
          | (TextToken s)::ps, i -> s + doPrettyPrint ps i
          | (ManyTokens ps1)::ps2, i -> doPrettyPrint ps1 i + doPrettyPrint ps2 i
      doPrettyPrint ps 0

