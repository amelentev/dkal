namespace Microsoft.Research.Dkal.SimpleParser.SimpleAst

  open System.Collections.Generic

  type SimpleFunction = string
  type SimpleVariable = string
  type SimpleConstant = 
  | BoolSimpleConstant of bool
  | IntSimpleConstant of int
  | FloatSimpleConstant of float
  | StringSimpleConstant of string

  type SimpleMetaTerm = 
  | SimpleApp of SimpleFunction * SimpleMetaTerm list
  | SimpleConst of SimpleConstant
  | SimpleVar of SimpleVariable

  type SimpleType = string

  type SimpleRelationDeclaration = 
    { Name: string;
      ArgsTyp: SimpleType list }
  with 
    static member infonAnd = {Name = "and"; ArgsTyp = ["infon"; "infon"]}
    static member infonImplies = {Name = "implies"; ArgsTyp = ["infon"; "infon"]}
    static member infonSaid = {Name = "said"; ArgsTyp = ["principal"; "infon"]}
    static member asInfon = {Name = "asInfon"; ArgsTyp = ["bool"]}

  type SimpleFunctionDeclaration = 
    { Name: string;
      RetTyp: SimpleType;
      Args: (string * SimpleType) list; }

  type SimplePolicy() =
    let relationDeclarations = new List<SimpleRelationDeclaration>()
    let functionDeclarations = new List<SimpleFunctionDeclaration>()
    let infons = new List<SimpleMetaTerm>()
    member sp.RelationDeclarations = relationDeclarations
    member sp.FunctionDeclarations = functionDeclarations
    member sp.Infons = infons
