namespace Microsoft.Research.DkalBackends

open Microsoft.FSharp.Text.Lexing

module Parsing =
  let parseInfonFormula s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.InfonFormula Lexer.tokenize lexbuff

  let parseInferenceProblem s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.InferenceProblem Lexer.tokenize lexbuff
