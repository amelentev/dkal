namespace Microsoft.Research.GeneralPDP.XACML

open Microsoft.FSharp.Text.Lexing

module Parsing =
  let parseRequest s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.Request Lexer.tokenize lexbuff

  let parseResponse s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.Response Lexer.tokenize lexbuff

  let parsePolicy s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.Policy Lexer.tokenize lexbuff

  let parsePolicyRequest s =
    let lexbuff = LexBuffer<char>.FromString(s)
    Parser.PolicyRequest Lexer.tokenize lexbuff
