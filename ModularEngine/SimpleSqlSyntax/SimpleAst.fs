namespace Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax.SimpleAst

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
/// parsed elements 
type SimpleMetaTerm = 
| SimpleApp of SimpleFunction * SimpleMetaTerm list
| SimpleConst of SimpleConstant
| SimpleVar of SimpleVariable
| SimpleSubstrate of string * string 

