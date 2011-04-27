namespace Microsoft.Research.Dkal.Ast.SimpleSyntax
  
open System.Collections.Generic

open Microsoft.Research.Dkal.Ast.SimpleSyntax.SimpleAst
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

/// Stores type information for the SimpleVariables in a given Context
type TypeInfo() = 
  
  /// List of variable declaration levels. Each level contains a mapping of 
  /// variables to its corresponding types
  let levels = new List<Dictionary<SimpleVariable, IType>>()

  /// Type information that maps BasicSimpleTypes to Types
  let types = new Dictionary<string, IType>()
//  do
//    // Load primitive type renames
//    types.Add("bool", Type.Boolean)
//    types.Add("int32", Type.Int32)
//    types.Add("double", Type.Double)
//    types.Add("string", Type.String)
//    types.Add("principal", Type.Principal)
//    types.Add("infon", Type.Infon)
//    types.Add("rule", Type.Rule)

  /// Adds a new type rename, extending the types information accordingly
  member ti.AddTypeRename (newType: string) (targetType: SimpleType) =
    types.[newType] <- ti.LiftType targetType

  /// Returns the corresponding Type given a SimpleType
  member ti.LiftType (st: SimpleType) =
    match st with
    | BasicSimpleType(name) -> 
      let found, typ = types.TryGetValue name
      if found then
        typ
      else
        Type.FromFullName name
//    | SeqSimpleType(st') ->
//      Type.Sequence(ti.LiftType st')
//    | TupleSimpleType(st1, st2) ->
//      Type.Tuple(ti.LiftType st1, ti.LiftType st2)

  /// Adds a new typing level, using the type information found in the 
  /// SimpleArgs given in vars. If a variable already existed in a 
  /// previous level, then it must be declared with the same type
  member ti.AddLevel (vars: SimpleArg list) =
    let newLevel = new Dictionary<SimpleVariable, IType>()
    for argName, argTyp in vars do
      match ti.VariableType argName with
      | Some t when t <> ti.LiftType argTyp ->
        failwith <| "Redefined variable with different types " + argName
      | _ ->
        newLevel.[argName] <- ti.LiftType argTyp
    levels.Insert(0, newLevel)

  /// Adds a variable to the current level
  member ti.AddToCurrentLevel ((var, typ): SimpleArg) =
    levels.[0].[var] <- ti.LiftType typ
    
  /// Removes the latest level and forgets about all the variables declared there
  member ti.PopLevel () =
    levels.RemoveAt 0 

  /// Given a SimpleVariable it returns its Type, if found; None otherwise
  member ti.VariableType(varName: SimpleVariable) =
    let mutable i = 0
    let mutable ret = None
    while ret = None && i < levels.Count do
      let found, typ = levels.[i].TryGetValue varName
      if found then
        ret <- Some typ
      i <- i + 1
    ret

  /// Returns a Dictionary containing the type information from all the levels
  member ti.AllLevels () =
    let ret = new Dictionary<SimpleVariable, IType>()
    for level in levels do
      for kv in level do
        ret.[kv.Key] <- kv.Value
    ret