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

namespace Microsoft.Research.Dkal.Interfaces

/// IParsingContext provides an interface for contexts that keep information
/// necessary during the parsing process such as variable types, macro 
/// definitions, etc.
type IParsingContext =

  /// Name of the principal for which the policy is being parsed
  abstract Me: string 

  /// Returns true if there is a variable in the context with the given name
  abstract HasVariable: name: string -> bool

  /// For a variable name in the context, it returns its type
  abstract VariableType: name: string -> IType 

  /// Adds a type rename by giving a newTypeName to a given targetType
  abstract AddTypeRename: newTypeName: string * targetType: IType -> unit

  /// Given a type fullname it returns the AST type for it
  abstract TypeFromName: string -> IType 
  
  /// Returns true iff the contexts has a macro definition with the given name
  abstract HasMacro: name: string -> bool

  /// Given a macro name, a return type, a body and arguments, it incorporates
  /// these as a new macro in the context. 
  abstract AddMacro: macroName: string * retType: IType * body: ISubstrateQueryTerm * args: IVar list -> unit

  /// Given a macro name in the context it returns the macro arguments
  abstract GetMacroArgs: macroName: string -> IVar list 

  /// Given a macro name in the context it returns the macro return type
  abstract GetMacroRetType: macroName: string -> IType

  /// Given a macro name and concrete arguments, it returns a tuple (t, sqt)
  /// where t is the element that should be inlined (the macro return variable)
  /// and sqt is the macro body with the concrete arguments (which should be 
  /// incorporated as an asInfon(sqt) somewhere before or after t is used)
  abstract ApplyMacro: macroName: string * concreteArgs: ITerm list -> ITerm * ISubstrateQueryTerm

  /// Given a type it returns a fresh variable of that type
  abstract FreshVar: IType -> IVar
