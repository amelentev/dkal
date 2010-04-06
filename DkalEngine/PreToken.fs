namespace Microsoft.Research.DkalEngine

module PreToken =
  type Tok0 =
    | Int of int
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
