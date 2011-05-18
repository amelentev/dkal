// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Ast.Syntax.Parsing
  
  open System.Collections.Generic
  open Microsoft.FSharp.Text.Lexing

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

      member lpc.AddTypeRename (newTypeName: string, targetType: IType) =
        parent.AddTypeRename(newTypeName, targetType)

      member lpc.TypeFromName(typeName: string) =
        parent.TypeFromName typeName

      member lpc.HasMacro(macroName: string) =
        parent.HasMacro macroName

      member lpc.AddMacro(macroName: string, retType: IType, body: ISubstrateQueryTerm, args: IVar list) =
        parent.AddMacro(macroName, retType, body, args)

      member lpc.GetMacroArgs(macroName: string) =
        parent.GetMacroArgs(macroName)

      member lpc.GetMacroRetType(macroName: string) = 
        parent.GetMacroRetType(macroName)

      member lpc.ApplyMacro(macroName: string, concreteArgs: ITerm list) =
        parent.ApplyMacro(macroName, concreteArgs)
