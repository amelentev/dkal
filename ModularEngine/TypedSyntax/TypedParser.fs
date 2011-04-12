namespace Microsoft.Research.Dkal.TypedSyntax

open System.IO
open Microsoft.FSharp.Text.Lexing

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type TypedParser() = 

  let lexbuff s = LexBuffer<char>.FromString(s)

  interface IParser with
    member sp.ParseInfon s = 
      let mt : MetaTerm = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      if mt.Typ() <> Infon then
        failwith <| "Expecting infon and found " + mt.Typ().ToString()
      else
        mt

    member sp.ParseRule s = 
      let mt : MetaTerm = Parser.MetaTerm Lexer.tokenize (lexbuff s)
      if mt.Typ() <> Rule then
        failwith <| "Expecting rule and found " + mt.Typ().ToString()
      else
        mt
    
    member sp.ParsePolicy s = 
      Parser.Policy Lexer.tokenize (lexbuff s)

    member sp.ParseSignature s =
      { Tables = []; Relations = [] }

    member sp.ParseAssembly s =
      let sp = (sp :> IParser)
      { Policy = sp.ParsePolicy s; Signature = sp.ParseSignature s }
