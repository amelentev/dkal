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
    
    // equality constructs
    addPrimitive {Name = "eqBool"; RetTyp = Bool; ArgsTyp = [Bool; Bool]; Body = None}
    addPrimitive {Name = "eqInt"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "eqFloat"; RetTyp = Bool; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "eqString"; RetTyp = Bool; ArgsTyp = [String; String]; Body = None}
    addPrimitive {Name = "neqBool"; RetTyp = Bool; ArgsTyp = [Bool; Bool]; Body = None}
    addPrimitive {Name = "neqInt"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "neqFloat"; RetTyp = Bool; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "neqString"; RetTyp = Bool; ArgsTyp = [String; String]; Body = None}

    // inequality constructs
    addPrimitive {Name = "ltInt"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "ltFloat"; RetTyp = Bool; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "lteInt"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "lteFloat"; RetTyp = Bool; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "gtInt"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "gtFloat"; RetTyp = Bool; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "gteInt"; RetTyp = Bool; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "gteFloat"; RetTyp = Bool; ArgsTyp = [Float; Float]; Body = None}
    
    // int constructs
    addPrimitive {Name = "plusInt"; RetTyp = Int; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "minusInt"; RetTyp = Int; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "uminusInt"; RetTyp = Int; ArgsTyp = [Int]; Body = None}
    addPrimitive {Name = "timesInt"; RetTyp = Int; ArgsTyp = [Int; Int]; Body = None}
    addPrimitive {Name = "divInt"; RetTyp = Int; ArgsTyp = [Int; Int]; Body = None}

    // float constructs
    addPrimitive {Name = "plusFloat"; RetTyp = Float; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "minusFloat"; RetTyp = Float; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "uminusFloat"; RetTyp = Float; ArgsTyp = [Float]; Body = None}
    addPrimitive {Name = "timesFloat"; RetTyp = Float; ArgsTyp = [Float; Float]; Body = None}
    addPrimitive {Name = "divFloat"; RetTyp = Float; ArgsTyp = [Float; Float]; Body = None}

    // string constructs
    addPrimitive {Name = "plusString"; RetTyp = String; ArgsTyp = [String; String]; Body = None}

    // principal constructs
    addPrimitive {Name = "eqPrincipal"; RetTyp = Bool; ArgsTyp = [Principal; Principal]; Body = None}
