namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.FSharp.Text.Lexing

/// IParsingContext provides an interface for contexts that keep information
/// necessary during the parsing process such as variable types, macro 
/// definitions, etc.
type IParsingContext =

  abstract Me: string 

  abstract HasVariable: string -> bool
  abstract VariableType: string -> IType 

  abstract AddTypeRename: newTypeName: string * targetType: IType -> unit
  abstract TypeFromName: string -> IType 
  
  abstract HasMacro: string -> bool
  abstract AddMacro: macroName: string * retType: IType * body: ISubstrateQueryTerm * args: IVar list -> unit
  abstract GetMacroArgs: macroName: string -> IVar list 
  abstract GetMacroRetType: macroName: string -> IType
  abstract ApplyMacro: macroName: string * concreteArgs: ITerm list -> ITerm * ISubstrateQueryTerm
