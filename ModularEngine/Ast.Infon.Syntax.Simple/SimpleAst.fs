namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple.SimpleAst

open System.Collections.Generic

/// To easily store parsed functions
type SimpleFunction = string

/// To easily store parsed variables
type SimpleVariable = string

/// To easily store parsed types
type SimpleType = string

/// To easily store parsed arguments
type SimpleArg = string * SimpleType

/// To easily store parsed constants
type SimpleConstant = 
| BoolSimpleConstant of bool
| Int32SimpleConstant of int32
| DoubleSimpleConstant of double
| StringSimpleConstant of string
| PrincipalSimpleConstant of string

/// The SimpleMetaTerms are used as an untyped intermediate representation for 
/// parsed elements before they are transformed (lifted) to typed MetaTerms by
/// the Context class
type SimpleMetaTerm = 
| SimpleApp of SimpleArg list * SimpleFunction * SimpleMetaTerm list
| SimpleConst of SimpleConstant
| SimpleVar of SimpleVariable
| SimpleSubstrate of string * string 

/// To easily store parsed substrate declarations
type SimpleSubstrateDeclaration = 
  { Kind: string;
    Args: string list;
    Namespaces: string list }

/// To easily store parsed type renames
type SimpleTypeDeclaration = 
  { NewTyp: string;
    TargetTyp: SimpleType }

/// To easily store parsed relation declarations
type SimpleRelationDeclaration = 
  { Name: string;
    Args: SimpleArg list }

/// To easily store parsed macro declarations
type SimpleMacroDeclaration = 
  { Name: string;
    RetTyp: SimpleType;
    Namespace: string;
    Args: SimpleArg list; 
    Body: string }

/// To easily store parsed signatures
type SimpleSignature() = 
  let substrateDeclarations = new List<SimpleSubstrateDeclaration>()
  let typeDeclarations = new List<SimpleTypeDeclaration>()
  let relationDeclarations = new List<SimpleRelationDeclaration>()
  let macroDeclarations = new List<SimpleMacroDeclaration>()
  member ss.SubstrateDeclarations = substrateDeclarations
  member ss.TypeDeclarations = typeDeclarations
  member ss.RelationDeclarations = relationDeclarations
  member ss.MacroDeclarations = macroDeclarations
   
/// To easily store parsed policies
type SimplePolicy() =
  let rules = new List<SimpleMetaTerm>()
  member sp.Rules = rules

/// To easily store parsed assemblies
type SimpleAssembly = { Signature: SimpleSignature; Policy: SimplePolicy }
