namespace Microsoft.Research.Dkal.Ast.Syntax.ParsingContext
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast
  
  /// A LocalParsingContext refers to a parent IParsingContext for extra information
  type LocalParsingContext(vars: IVar list, parent: IParsingContext) =
    
    let localVars = new Dictionary<string, IType>()
    do
      for var in vars do
        if parent.HasVariable var.Name && parent.VariableType var.Name <> var.Type then
          failwithf "Duplicate variable definition with different type: %O" var.Name
        localVars.[var.Name] <- var.Type

    interface IParsingContext with
      
      member lpc.Me = parent.Me

      member lpc.HasVariable(varName: string) = 
        localVars.ContainsKey varName || parent.HasVariable varName

      member lpc.VariableType(varName: string) = 
        let found, typ = localVars.TryGetValue varName
        if found then
          typ
        else
          parent.VariableType varName

      member lpc.AddTypeRename(newType: string, targetType: string) =
        parent.AddTypeRename(newType, targetType)

      member lpc.TypeFromName(typeName: string) =
        parent.TypeFromName typeName

      member lpc.HasMacro(macroName: string) =
        parent.HasMacro macroName

      member lpc.AddMacro(macroName: string, retType: IType, body: ISubstrateTerm, args: IVar list) =
        parent.AddMacro(macroName, retType, body, args)

      member lpc.GetMacroArgs(macroName: string) =
        parent.GetMacroArgs(macroName)

      member lpc.GetMacroRetType(macroName: string) = 
        parent.GetMacroRetType(macroName)

      member lpc.ApplyMacro(macroName: string, concreteArgs: ITerm list) =
        parent.ApplyMacro(macroName, concreteArgs)
