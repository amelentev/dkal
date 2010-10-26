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

namespace Microsoft.Research.DkalEngine

module PreToken =
  type Tok0 =
    | Int of int
    | Float of float
    | Id of string  // non-capitalized
    | Var of string // capitalized
    | StringLiteral of string
    | LParen of char
    | RParen of char
    | Spaces of int
    | NewLine
    | Invalid of string
    | Eof
    | Late of (unit -> obj)
