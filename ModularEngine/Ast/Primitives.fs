﻿module Microsoft.Research.Dkal.Ast.Primitives

  open System.Collections.Generic
  open Microsoft.Research.Dkal.Ast

  let primitives = new Dictionary<string, Function>()
  let associatives = new HashSet<string>()

  // n-ary functions 
  let andInfon (mts: MetaTerm list) = 
    App({ Name = "andInfon"; 
          RetTyp = Infon; 
          ArgsTyp = List.replicate mts.Length Infon }, mts)
  let andBool (mts: MetaTerm list) = 
    App({ Name = "andBool"; 
          RetTyp = Bool; 
          ArgsTyp = List.replicate mts.Length Bool }, mts)
  let orBool (mts: MetaTerm list) = 
    App({ Name = "orBool"; 
          RetTyp = Bool; 
          ArgsTyp = List.replicate mts.Length Bool }, mts)

  let trueBool = Const(BoolConstant(true))
  let trueInfon = App({Name = "asInfon"; RetTyp = Infon; ArgsTyp = [Bool]}, [Const(BoolConstant(true))])

  do
    let addPrimitive (func: Function) = primitives.Add(func.Name, func)
    
    // rule constructs
    addPrimitive {Name = "rule"; RetTyp = Rule; ArgsTyp = [Infon; Infon; Action]}

    // action constructs
    addPrimitive {Name = "seq"; RetTyp = Action; ArgsTyp = [Action; Action]}
    addPrimitive {Name = "send"; RetTyp = Action; ArgsTyp = [Principal; Infon]}
    addPrimitive {Name = "learn"; RetTyp = Action; ArgsTyp = [Infon]}

    // infon constructs
    addPrimitive {Name = "asInfon"; RetTyp = Infon; ArgsTyp = [Bool]}
    addPrimitive {Name = "andInfon"; RetTyp = Infon; ArgsTyp = [Infon; Infon]}; 
    addPrimitive {Name = "impliesInfon"; RetTyp = Infon; ArgsTyp = [Infon; Infon]}
    addPrimitive {Name = "saidInfon"; RetTyp = Infon; ArgsTyp = [Principal; Infon]}
    
    // boolean constructs
    addPrimitive {Name = "andBool"; RetTyp = Bool; ArgsTyp = [Bool; Bool]}
    addPrimitive {Name = "orBool"; RetTyp = Bool; ArgsTyp = [Bool; Bool]}
    addPrimitive {Name = "notBool"; RetTyp = Bool; ArgsTyp = [Bool]}
    
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
    associatives.Add("andInfon") |> ignore
    associatives.Add("andBool") |> ignore
    associatives.Add("orBool") |> ignore