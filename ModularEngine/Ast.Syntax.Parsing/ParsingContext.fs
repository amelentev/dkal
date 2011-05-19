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
  
  /// A ParsingContext stores information that helps the parsing process such as
  /// macros, variable types, fresh variable ids, etc.
  type ParsingContext(me: string) =

    let variables = new Dictionary<string, IType>()
    let macros = new Dictionary<string, IType * ISubstrateQueryTerm * IVar list>()
    let types = new Dictionary<string, IType>()

    /// Holds a parsing position
    let mutable pos = Position.Empty

    /// Holds fresh variable ids that are used when solving macros
    let mutable freshVarId = 1

    interface IParsingContext with

      member c.Me = me

      member c.HasVariable (varName: string) = 
        variables.ContainsKey varName

      member c.VariableType (varName: string) =
        let found, typ = variables.TryGetValue varName
        if found then
          typ
        else
          failwithf "Undefined variable %O" varName

      member c.AddTypeRename (newTypeName: string, targetType: IType) =
        types.[newTypeName] <- targetType

      member c.TypeFromName (typeName: string) =
        let found, typ = types.TryGetValue typeName
        if found then
          typ
        else
          Type.FromFullName typeName

      member c.HasMacro (macroName: string) = 
        macros.ContainsKey macroName

      member c.AddMacro (macroName: string, retType: IType, body: ISubstrateQueryTerm, args: IVar list) =
        macros.[macroName] <- (retType, body, args)

      member c.GetMacroArgs (macroName: string) =
        let _, _, args = macros.[macroName]
        args

      member c.GetMacroRetType (macroName: string) =
        let retType, _, _ = macros.[macroName]
        retType

      member c.ApplyMacro (macroName: string, concreteArgs: ITerm list) =
        let retTyp, body, args = macros.[macroName]
        let mutable subst = Substitution.Id
        let mutable accumSolvedMacros = []
        for concreteArg, arg in List.zip concreteArgs args do
          subst <- subst.Extend ({Name = arg.Name; Type = concreteArg.Type}, concreteArg)
        let newRet = Var((c :> IParsingContext).FreshVar retTyp)
        subst <- subst.Extend ({Name = "Ret"; Type = retTyp}, newRet)
        newRet, (body :> ITerm).Apply subst :?> ISubstrateQueryTerm

      /// Returns a fresh Variable of the given type
      member c.FreshVar (t: IType) =
        freshVarId <- freshVarId + 1
        { Name = "Tmp" + freshVarId.ToString(); 
          Type = t } :> IVar
