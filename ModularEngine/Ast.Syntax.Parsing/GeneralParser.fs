namespace Microsoft.Research.Dkal.Ast.Syntax.Parsing

  open Microsoft.FSharp.Text.Lexing

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Utils.Exceptions

  type GeneralParser() =

    static member TryParse (target: LexBuffer<char> -> 'b) (s: string) = 
      let lexbuf = LexBuffer<char>.FromString(s)
      try
        let ret = target lexbuf
        ret
      with 
      | ParseException(_) as e -> 
        //let pos = lexbuf.StartPos
        //let col = if line = 1 then col - pos.Column else pos.Column + 1
        raise e
      | e ->
        let pos = lexbuf.StartPos
        raise <| ParseException(e.Message, s, pos.Line+1, pos.Column+1)
