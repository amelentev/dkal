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

namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic

/// A relation declaration with typed arguments. This is used in a Signature
/// in order to keep this information
type RelationDeclaration = { Name: string; 
                              Args: IVar list }

/// A Signature holds all the substrate, tables and relation declarations 
/// found in an assembly
type Signature =  { Substrates: ISubstrate list;
                    Relations: RelationDeclaration list }
 
/// A Policy contains a list of rules (in the order they were found in the 
/// Assembly)
type Policy = { Rules: ITerm list }

/// An Assembly is composed of a Signature and a Policy
type Assembly = { Signature: Signature; Policy: Policy }
