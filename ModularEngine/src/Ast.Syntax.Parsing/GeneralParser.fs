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

namespace Microsoft.Research.Dkal.Ast.Syntax.Parsing

  open Microsoft.FSharp.Text.Lexing

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Utils.Exceptions

  /// A general parser provides a unified interface to parse and throw parsing
  /// exceptions
  type GeneralParser() =

    /// Attempt to use the given target parser on the string either producing 
    /// a successful output, or raising a ParseException
    static member TryParse (target: LexBuffer<char> -> 'b) (s: string) = 
      let lexbuf = LexBuffer<char>.FromString(s)
      try
        let ret = target lexbuf
        ret
      with 
      | ParseException(msg, s, line, column) -> // from some inner parser (substrate)
        let pos = lexbuf.StartPos
        raise <| ParseException(msg, s, pos.Line+line, pos.Column)
      | e -> // from fsyacc
        let pos = lexbuf.StartPos
        raise <| ParseException(e.Message, s, pos.Line+1, pos.Column+1)
