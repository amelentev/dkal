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

namespace Microsoft.Research.Dkal.LogicEngine.ML

open System.Collections.Generic
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Substrate // for SubstrateDispatcher
open Microsoft.Research.Dkal.Interfaces // for ISignatureProvider and IInfostrate

module MLType =
  // type substrateTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateTerm
  // in F*, import this type abstractly from another module
  type substrateQueryTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateQueryTerm
  type substrateUpdateTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateUpdateTerm

  type principal = string

  and var = // IVar
    { typ : typ; name : string }

  and typ =  // IType
  // from Types.fs, see also Ast/ActivePatterns.fs
  // BasicType types
    | Infon
    | Principal
    | SubstrateUpdate
    | SubstrateQuery
    | Action
    | Condition
    | RuleT
    | Evidence
  // Substrate types
    | Boolean
    | Int32
    | Double
    | String

  and constant =
    | True
    | False
    | SubstrateConstant of obj // of obj in the original code
    | PrincipalConstant of principal // of string in original code

  and relationInfon = // form TreeTerm.fs, def of type Function
    { name : string; retType: typ; argsType : typ list; identity : term option }

  and func =
  // from Ast.Infon/ActivePatterns.fs
    // Rule: <condition> do <action>
    | SeqRule
    | EmptyRule
    | Rule
    | RuleOnce
    // Condition
    | SeqCondition
    | EmptyCondition
    | WireCondition // upon in concrete syntax
    | KnownCondition // if in concrete syntax
    // Action
    | SeqAction
    | EmptyAction
    | Send // ppal (destination), msg
    | JustifiedSend // ppal (destination), msg
    | JustifiedSay // ppal (destination), msg
    | Learn
    | Forget
    | Install // add a rule to set of rules
    | Uninstall // regarding a rule
    | Apply //of substrateUpdateTerm // apply this update to the substrate
    | Drop // regarding an infon that came in as a message
    // Infon
    | EmptyInfon
    | AsInfon //of substrateQueryTerm
    | AndInfon // may be easier to consider just binary case
    | ImpliesInfon // infon, infon
    | SaidInfon  // ppal (sender), msg
    | JustifiedInfon // infon, evidence
    // Evidence
    | EmptyEvidence
    | SignatureEvidence // ppal, term, int
      // might want to change the third type to dsig, signature for .Net
    | ModusPonensEvidence
    | AndEvidence
    | AsInfonEvidence //of substrateQueryTerm
    | RelationInfon of relationInfon
    // no active pattern for it, base case for infons

  and term = // ITerm
  // from Ast.Tree/ActivePatterns.fs
  // from Ast/ActivePatterns.fs
    | Var of var
    | Const of constant
    | Forall of var * term
    | App of func * (term list)
    | ConcretizationEvidence of term * substitution // ExplicitSubstitutionTerm.fs
    | SubstrateQueryTerm of substrateQueryTerm
    | SubstrateUpdateTerm of substrateUpdateTerm

  and substitution = Dictionary< var, term >
  // wrap the functions of dictionary used inside
  // other functions and only use them there.
  // use Guido's functional wrapper from Substitution








