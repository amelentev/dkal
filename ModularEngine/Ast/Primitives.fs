module Microsoft.Research.Dkal.Ast.Primitives

  open System.Collections.Generic
  open Microsoft.Research.Dkal.Ast

  let primitives = new Dictionary<string, Function>()
  let associatives = new HashSet<string>()

  // n-ary functions 
  let infonAnd (mts: MetaTerm list) = 
    App({ Name = "infonAnd"; 
          RetTyp = Infon; 
          ArgsTyp = List.replicate mts.Length Infon }, mts)
  let boolAnd (mts: MetaTerm list) = 
    App({ Name = "boolAnd"; 
          RetTyp = Bool; 
          ArgsTyp = List.replicate mts.Length Bool }, mts)
  let boolOr (mts: MetaTerm list) = 
    App({ Name = "boolOr"; 
          RetTyp = Bool; 
          ArgsTyp = List.replicate mts.Length Bool }, mts)

  do
    let addPrimitive (func: Function) = primitives.Add(func.Name, func)
    
    // infon constructs
    addPrimitive {Name = "asInfon"; RetTyp = Infon; ArgsTyp = [Bool]}
    addPrimitive {Name = "infonAnd"; RetTyp = Infon; ArgsTyp = [Infon; Infon]}; 
    addPrimitive {Name = "infonImplies"; RetTyp = Infon; ArgsTyp = [Infon; Infon]}
    addPrimitive {Name = "infonSaid"; RetTyp = Infon; ArgsTyp = [Principal; Infon]}
    
    // boolean constructs
    addPrimitive {Name = "boolAnd"; RetTyp = Bool; ArgsTyp = [Bool; Bool]}
    addPrimitive {Name = "boolOr"; RetTyp = Bool; ArgsTyp = [Bool; Bool]}
    addPrimitive {Name = "boolNot"; RetTyp = Bool; ArgsTyp = [Bool]}
    
    // equality constructs
    addPrimitive {Name = "eqBool"; RetTyp = Bool; ArgsTyp = [Bool; Bool]}
    addPrimitive {Name = "eqInt32"; RetTyp = Bool; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "eqDouble"; RetTyp = Bool; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "eqString"; RetTyp = Bool; ArgsTyp = [Type.String; Type.String]}
    addPrimitive {Name = "eqPrincipal"; RetTyp = Bool; ArgsTyp = [Principal; Principal]}
    addPrimitive {Name = "neqBool"; RetTyp = Bool; ArgsTyp = [Bool; Bool]}
    addPrimitive {Name = "neqInt32"; RetTyp = Bool; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "neqDouble"; RetTyp = Bool; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "neqString"; RetTyp = Bool; ArgsTyp = [Type.String; Type.String]}
    addPrimitive {Name = "neqPrincipal"; RetTyp = Bool; ArgsTyp = [Principal; Principal]}

    // inequality constructs
    addPrimitive {Name = "ltInt32"; RetTyp = Bool; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "ltDouble"; RetTyp = Bool; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "lteInt32"; RetTyp = Bool; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "lteDouble"; RetTyp = Bool; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "gtInt32"; RetTyp = Bool; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "gtDouble"; RetTyp = Bool; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "gteInt32"; RetTyp = Bool; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "gteDouble"; RetTyp = Bool; ArgsTyp = [Type.Float; Type.Float]}
    
    // int constructs
    addPrimitive {Name = "plusInt32"; RetTyp = Type.Int; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "minusInt32"; RetTyp = Type.Int; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "uminusInt32"; RetTyp = Type.Int; ArgsTyp = [Type.Int]}
    addPrimitive {Name = "timesInt32"; RetTyp = Type.Int; ArgsTyp = [Type.Int; Type.Int]}
    addPrimitive {Name = "divInt32"; RetTyp = Type.Int; ArgsTyp = [Type.Int; Type.Int]}

    // float constructs
    addPrimitive {Name = "plusDouble"; RetTyp = Type.Float; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "minusDouble"; RetTyp = Type.Float; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "uminusDouble"; RetTyp = Type.Float; ArgsTyp = [Type.Float]}
    addPrimitive {Name = "timesDouble"; RetTyp = Type.Float; ArgsTyp = [Type.Float; Type.Float]}
    addPrimitive {Name = "divDouble"; RetTyp = Type.Float; ArgsTyp = [Type.Float; Type.Float]}

    // string constructs
    addPrimitive {Name = "plusString"; RetTyp = Type.String; ArgsTyp = [Type.String; Type.String]}

    // associative functions
    associatives.Add("infonAnd") |> ignore
    associatives.Add("boolAnd") |> ignore
    associatives.Add("boolOr") |> ignore