namespace Microsoft.Research.Dkal.SimpleSyntax.SimpleAst

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
| IntSimpleConstant of int
| FloatSimpleConstant of float
| StringSimpleConstant of string
| PrincipalSimpleConstant of string

/// The SimpleMetaTerms are used as an untyped intermediate representation for 
/// parsed elements before they are transformed (lifted) to typed MetaTerms by
/// the Context class
type SimpleMetaTerm = 
| SimpleApp of SimpleFunction * SimpleMetaTerm list
| SimpleConst of SimpleConstant
| SimpleVar of SimpleVariable

/// To easily store parsed substrate declarations
type SimpleSubstrateDeclaration = 
  { Name: string;
    Kind: string;
    Args: SimpleConstant list }

/// To easily store parsed type renames
type SimpleTypeDeclaration = 
  { NewTyp: SimpleType;
    TargetTyp: SimpleType }

/// To easily store parsed table declarations
type SimpleTableDeclaration = 
  { Name: string;
    Cols: SimpleArg list }

/// To easily store parsed relation declarations
type SimpleRelationDeclaration = 
  { Name: string;
    Args: SimpleArg list }

/// To easily store parsed macro declarations
type SimpleMacroDeclaration = 
  { Name: string;
    RetTyp: SimpleType;
    Args: SimpleArg list; 
    Body: SimpleMetaTerm }

/// To easily store parsed signatures
type SimpleSignature() = 
  let substrateDeclarations = new List<SimpleSubstrateDeclaration>()
  let typeDeclarations = new List<SimpleTypeDeclaration>()
  let tableDeclarations = new List<SimpleTableDeclaration>()
  let relationDeclarations = new List<SimpleRelationDeclaration>()
  let macroDeclarations = new List<SimpleMacroDeclaration>()
  member ss.SubstrateDeclarations = substrateDeclarations
  member ss.TypeDeclarations = typeDeclarations
  member ss.TableDeclarations = tableDeclarations
  member ss.RelationDeclarations = relationDeclarations
  member ss.MacroDeclarations = macroDeclarations
    
/// To easily store parsed policies
type SimplePolicy() =
  let rules = new List<SimpleMetaTerm>()
  member sp.Rules = rules

/// To easily store parsed assemblies
type SimpleAssembly = { Signature: SimpleSignature; Policy: SimplePolicy }
