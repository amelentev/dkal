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

/// Each IType implementation represents a type for AST elements. For instance
/// infons, principals, integers, rules, evidence, ...
type IType = 
  
  /// Short name for this type
  abstract member Name: string

  /// Full name for this type
  abstract member FullName: string

  /// Base type
  abstract member BaseType: IType option

  /// Returns true if this type can be seen as the typed passed as parameter
  abstract member IsSubtypeOf: IType -> bool


/// ITerm implementations are the AST elements. They encode everything from 
/// variables and constants to asInfon queries, rules, etc.
type ITerm =

  /// The type of this AST element
  abstract member Type: IType

  /// The free variables in this AST element
  abstract member Vars: IVar list

  /// The bound variables in this AST element
  abstract member BoundVars: IVar list

  /// Returns a new ITerm which results from applying the given substitution
  abstract member Apply: ISubstitution -> ITerm

  /// Returns a new ITerm which is a normalized version of this one
  abstract member Normalize: unit -> ITerm

  /// It tries to unify this ITerm with the given one, starting from the 
  /// given substitution. If the unification is successful, a new substitution
  /// (which is a specialization of the given one) is returned. None is returned
  /// if unification fails
  abstract member UnifyFrom: ISubstitution -> ITerm -> ISubstitution option

  /// It tries to unify this ITerm with the given one. If unification is successful
  /// a substitution that guarantees syntactic equality is returned, otherwise 
  /// None is returned
  abstract member Unify: ITerm -> ISubstitution option


/// IVar is used to represent any AST element that is to be treated like a 
/// variable
and IVar =
  inherit ITerm

  /// Variable name, which is used as variable identifier
  abstract member Name: string


/// IConst is an interface that is implemented by all the AST nodes that represent
/// ground values
and IConst =
  inherit ITerm

  /// The value of this constant
  abstract member Value: obj


/// ISubstitution implementations model substitutions that map variables to AST 
/// elements (IVar --> ITerm). They are used as results of unification
and ISubstitution =

  /// Applies this substitution to the given IVar
  abstract member Apply: IVar -> ITerm

  /// Returns a new substitution that results from extending the current 
  /// substitution so that it maps v to t and leaves the rest unchanged
  abstract member Extend: v: IVar * t: ITerm -> ISubstitution

  /// Returns a new substitution that results from first applying s' and 
  /// then applying the current Substitution
  abstract member ComposeWith: s': ISubstitution -> ISubstitution

  /// Returns true iff v is affected by this substitution
  abstract member DomainContains: v: IVar -> bool

  /// Returns the vars affected by this substitution
  abstract member Domain: IVar list

  /// Returns a new substitution that results from restricting the current 
  /// one to only modify the variables given in the list
  abstract member RestrictTo: IVar list -> ISubstitution

  /// Returns a new substitution that results from forgetting the current
  /// mapping to the variables given in the list
  abstract member Forget: IVar list -> ISubstitution

  /// Returns true if this substitution only renames variables
  abstract member IsVariableRenaming: bool
