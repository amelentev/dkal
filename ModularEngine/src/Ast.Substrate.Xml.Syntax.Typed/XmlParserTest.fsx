#I @".\bin\Debug\"
#r "Ast.Substrate.Xml.Syntax.Typed.dll"
#r "Ast.Syntax.Parsing.dll"
#r "Ast.dll"
#r "Interfaces.dll"
#r "Substrate.Xml.dll"
#r "FSharp.PowerPack.dll"
#r "System.Xml.Linq.dll"

open Microsoft.Research.Dkal.Ast.Substrate.Xml.Syntax.Typed
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.FSharp.Text.Lexing
open System.Text

let parse = GeneralParser.TryParse (Parser.XmlSubstrateTerm Lexer.tokenize)

//Lexer.tokenize (LexBuffer<char>.FromString("update"))

let shouldEqual (a:string) (b:string) =
  if a <> b then
    let sb = StringBuilder()
    let mutable i = 0
    while i < min a.Length b.Length && a.[i] = b.[i] do
      i <- i+1
    failwith (sprintf " expected: <%A> \n actual: <%A> \n common: <%s> \n differs: \n <%s> \n <%s>" a b (a.Substring(0, i)) (a.Substring(i)) (b.Substring(i)))
  else printfn "Ok"

// query
(parse " \"xpath expr\" | OUT1: System.String, OUT2: System.Int32 <-> \"attr\" | IN1: System.String, IN2: System.Int32 ").ToString()
  |> shouldEqual "{| \"\" | \"xpath expr\" | OUT1, OUT2<->\"attr\" |}"
// insert
(parse " insert \"xpath expr\" | VAL1: System.String, VAL2: System.Int32 <-> \"attr\" | IN1: System.String, IN2: System.Int32 ").ToString()
  |> shouldEqual "{| \"\" | insert \"xpath expr\" | VAL1, VAL2<->\"attr\" |}"
// delete
(parse " delete \"xpath expr\" | IN1: System.String, IN2: System.Int32 ").ToString()
  |> shouldEqual "{| \"\" | delete \"xpath expr\" |}"
// update
(parse " update \"xpath expr\" | VAL1: System.String, VAL2: System.Int32<->\"attr\", delete \"attr2\" | IN1: System.String, IN2: System.Int32 ").ToString()
  |> shouldEqual "{| \"\" | update \"xpath expr\" | VAL1, VAL2<->\"attr\", delete \"attr2\" |}"
