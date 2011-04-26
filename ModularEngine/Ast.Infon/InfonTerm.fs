namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic

/// Principal constants
type PrincipalConstant(name: string) =
  inherit Constant()
  override pc.Type = Type.Principal
  override pc.Equals (o: obj) =
    match o with
    | :? PrincipalConstant as pc' -> pc.Name.Equals pc'.Name
    | _ -> false
  override pc.GetHashCode() = pc.Name.GetHashCode()
  override pc.ToString() = name
  member pc.Name = name

/// Substrate constants
type SubstrateConstant(elem: obj) =
  inherit Constant()
  override sc.Type = Type.Substrate(elem.GetType()) :> IType
  override sc.Equals (o: obj) =
    match o with
    | :? SubstrateConstant as sc' -> sc.Elem.Equals sc'.Elem
    | _ -> false
  override sc.GetHashCode() = sc.Elem.GetHashCode()
  override sc.ToString() = 
    match elem with
    | :? string as s -> "\"" + s + "\""
    | _ -> elem.ToString()
  member sc.Elem = elem

/// A table declaration with typed columns. This is used in a Signature in
/// order to keep this information
type TableDeclaration = { Name: string; 
                          Cols: IVar list }
/// A relation declaration with typed arguments. This is used in a Signature
/// in order to keep this information
type RelationDeclaration = { Name: string; 
                              Args: IVar list }

/// A Signature holds all the substrate, tables and relation declarations 
/// found in an assembly
type Signature =  { Substrates: ISubstrate list;
                    Tables: TableDeclaration list;
                    Relations: RelationDeclaration list }
 
/// A Policy contains a list of rules (in the order they were found in the 
/// Assembly)
type Policy = { Rules: ITerm list }

/// An Assembly is composed of a Signature and a Policy
type Assembly = { Signature: Signature; Policy: Policy }

