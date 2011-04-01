module Microsoft.Research.Dkal.Ast.Primitives

  open System.Collections.Generic
  open Microsoft.Research.Dkal.Ast

  let primitives = new Dictionary<string, Function>()
  do
    let addPrimitive func = primitives.Add(func.Name, func)
    
    // infon constructs
    addPrimitive {Name = "asInfon"; RetTyp = Infon; ArgsTyp = [Bool]; Body = None}
    addPrimitive {Name = "infonAnd"; RetTyp = Infon; ArgsTyp = [Infon; Infon]; Body = None}
    addPrimitive {Name = "infonImplies"; RetTyp = Infon; ArgsTyp = [Infon; Infon]; Body = None}
    addPrimitive {Name = "infonSaid"; RetTyp = Infon; ArgsTyp = [Principal; Infon]; Body = None}
    
    // boolean constructs
    addPrimitive {Name = "boolAnd"; RetTyp = Bool; ArgsTyp = [Bool; Bool]; Body = None}
    addPrimitive {Name = "boolOr"; RetTyp = Bool; ArgsTyp = [Bool; Bool]; Body = None}
    addPrimitive {Name = "boolNot"; RetTyp = Bool; ArgsTyp = [Bool]; Body = None}
    addPrimitive {Name = "intEq"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}

    // int constructs
    addPrimitive {Name = "intSum"; RetTyp = Int; ArgsTyp = [Int; Int]; Body = None}

