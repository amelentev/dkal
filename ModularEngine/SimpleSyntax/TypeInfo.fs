namespace Microsoft.Research.Dkal.SimpleSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst
  open Microsoft.Research.Dkal.Ast

  type TypeInfo() = 
    let levels = new List<Dictionary<SimpleVariable, Type>>()
    let types = new Dictionary<string, Type>()
    do
      // primitive types
      types.Add("bool", Type.Bool)
      types.Add("int", Type.Int)
      types.Add("float", Type.Float)
      types.Add("string", Type.String)
      types.Add("principal", Type.Principal)
      types.Add("infon", Type.Infon)
      types.Add("rule", Type.Rule)

    member ti.AddTypeRename (newType: string) (targetType: string) =
      types.[newType] <- ti.LiftType targetType

    member ti.LiftType (simpleType: string) =
      let found, typ = types.TryGetValue simpleType
      if found then
        typ
      else
        failwith <| "Undefined type: " + simpleType

    member ti.AddLevel (vars: SimpleArg list) =
      let newLevel = new Dictionary<SimpleVariable, Type>()
      for argName, argTyp in vars do
        if ti.VariableType argName = None then
          newLevel.[argName] <- ti.LiftType argTyp
        else
          failwith <| "Redefined variable " + argName
      levels.Insert(0, newLevel)

    member ti.AddToCurrentLevel ((var, typ): SimpleArg) =
      levels.[0].[var] <- ti.LiftType typ
    
    member ti.PopLevel () =
      levels.RemoveAt 0 

    member ti.VariableType(varName: string) =
      let mutable i = 0
      let mutable ret = None
      while ret = None && i < levels.Count do
        let found, typ = levels.[i].TryGetValue varName
        if found then
          ret <- Some typ
        i <- i + 1
      ret