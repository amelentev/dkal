namespace Microsoft.Research.Dkal.SimpleSyntax.SimpleAst

  open System.Collections.Generic

  type SimpleFunction = string
  type SimpleVariable = string
  type SimpleType = string

  type SimpleArg = string * SimpleType

  type SimpleConstant = 
  | BoolSimpleConstant of bool
  | IntSimpleConstant of int
  | FloatSimpleConstant of float
  | StringSimpleConstant of string
  | PrincipalSimpleConstant of string

  type SimpleMetaTerm = 
  | SimpleApp of SimpleFunction * SimpleMetaTerm list
  | SimpleConst of SimpleConstant
  | SimpleVar of SimpleVariable

  type SimpleSubstrateDeclaration = 
    { Name: string;
      Kind: string;
      Args: SimpleConstant list }

  type SimpleTypeDeclaration = 
    { NewTyp: SimpleType;
      TargetTyp: SimpleType }

  type SimpleTableDeclaration = 
    { Name: string;
      Cols: SimpleArg list }

  type SimpleRelationDeclaration = 
    { Name: string;
      Args: SimpleArg list }

  type SimpleFunctionDeclaration = 
    { Name: string;
      RetTyp: SimpleType;
      Args: SimpleArg list; 
      Body: SimpleMetaTerm }

  type SimpleKnowledge =  { Args: SimpleArg list; 
                            Fact: SimpleMetaTerm }
  type SimpleCommunicationRule =  { Args: SimpleArg list; 
                                    Trigger: SimpleMetaTerm;
                                    Target: SimpleMetaTerm;
                                    Content: SimpleMetaTerm }
  type SimpleAssertion = 
  | SimpleKnow of SimpleKnowledge
  | SimpleCommRule of SimpleCommunicationRule

  type SimpleSignature() = 
    let substrateDeclarations = new List<SimpleSubstrateDeclaration>()
    let typeDeclarations = new List<SimpleTypeDeclaration>()
    let tableDeclarations = new List<SimpleTableDeclaration>()
    let relationDeclarations = new List<SimpleRelationDeclaration>()
    let functionDeclarations = new List<SimpleFunctionDeclaration>()
    member ss.SubstrateDeclarations = substrateDeclarations
    member ss.TypeDeclarations = typeDeclarations
    member ss.TableDeclarations = tableDeclarations
    member ss.RelationDeclarations = relationDeclarations
    member ss.FunctionDeclarations = functionDeclarations
    
  type SimplePolicy() =
    let rules = new List<SimpleMetaTerm>()
    member sp.Rules = rules

  type SimpleAssembly = { Signature: SimpleSignature; Policy: SimplePolicy }
