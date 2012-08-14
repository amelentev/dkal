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

namespace Microsoft.Research.Dkal.Ast.Tree

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

/// Functions are used in App ITerms to indicate what function is applied.
/// They have an arbitrary-sized typed list of arguments. They return a typed
/// single value. The Identity field is None if the function is not 
/// associative; it points to the ITerm that behaves as identity otherwise
type Function = { Name: string; 
                  RetType: IType; 
                  ArgsType: IType list;
                  Identity: ITerm option }

/// Application ITerms are the inner nodes of the AST tree. They have a Function
/// and a list of arguments
[<CustomEqualityAttribute; NoComparisonAttribute>]
type Application = 
  { Function: Function; Args: ITerm list }
  interface ITerm with
    member f.Vars = new HashSet<_>(List.collect (fun (a: ITerm) -> a.Vars) f.Args) |> Seq.toList
    member f.BoundVars = new HashSet<_>(List.collect (fun (a: ITerm) -> a.BoundVars) f.Args) |> Seq.toList
    member f.Type = f.Function.RetType
    member f.Apply s = 
      { Function = f.Function; Args = List.map (fun (a: ITerm) -> a.Apply s) f.Args } :> ITerm
    
    member f.Normalize () = 
      /// Returns the children of mt, if mt encodes the application of function
      /// f; returns an empty list if it founds an identity term for the given 
      /// function; finally, it returns a singleton [t] in any other case
      let children (f: Function) (identity: ITerm) (t: ITerm) =
        match t with
        | t when t = identity -> []
        | :? Application as app -> 
          match app with 
          | { Function=f'; Args=ts } when f = f' -> ts
          | _ -> [t]        
        | _ -> [t]
          
      // Normalize recursively
      let args = List.map (fun (a: ITerm) -> a.Normalize()) f.Args
      // If f.Function is associative, get the children of each child
      let args =  match f.Function.Identity with
                  | Some identity ->
                    List.collect (children f.Function identity) args
                  | None ->
                    args
      
      // Remove malformed cases such as conjunction of zero elements, etc.
      match args, f.Function.Identity with
      | [t], Some _ -> t
      | [], Some id -> id
      | _ -> 
        // Return application of normalized function
        let f' =  { Name = f.Function.Name; 
                    RetType = f.Function.RetType; 
                    ArgsType = List.map (fun (a: ITerm) -> a.Type) args
                    Identity = f.Function.Identity }
        {Function = f'; Args = args } :> ITerm

    member f.UnifyFrom s t =
      match t with
      | :? IVar as v -> v.UnifyFrom s f
      | :? Application as f' 
        when f'.Function = f.Function 
          && f'.Args.Length = f.Args.Length -> 
        let mutable okSoFar = true
        let mutable ret = s
        let mutable i = 0
        while okSoFar && i < f.Args.Length do
          match f.Args.[i].Apply(ret).UnifyFrom ret (f'.Args.[i].Apply(ret)) with
          | Some s ->
            ret <- s
          | None -> 
            okSoFar <- false
          i <- i + 1
        if okSoFar then
          Some ret
        else
          None
      | _ -> None
    member f.Unify t = 
      (f :> ITerm).UnifyFrom (Substitution.Id) t
  override f.ToString() =
    f.Function.Name + "(" + 
      (String.concat ", " (List.map (fun a -> a.ToString()) f.Args)) + ")"
  override f.Equals (o: obj) =
    match o with
    | :? Application as f' ->
      f.Function.Name = f'.Function.Name && f.Args = f'.Args
    | _ -> 
      false
  override f.GetHashCode() =
    (f.Function.Name, f.Args).GetHashCode()