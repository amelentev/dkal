namespace Microsoft.Research.Dkal.Ast.Tree

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

/// Variables are typed
type Variable = 
  { Name: string; Type: IType }
  interface IVar with
    member v.Name = v.Name
    member v.Type = v.Type
    member v.Vars = [v]
    member v.Apply s = s.Apply v
    member v.Normalize () = v :> ITerm
    member v.UnifyFrom s t = 
      match (v :> IVar), t with
      | v1, v2 when (v1 :> ITerm) = v2 -> Some s
      | v, t when not(List.exists (fun v' -> v = v') t.Vars) -> Some (s.Extend (v, t))
      | _ -> None
    member v.Unify t = 
      (v :> ITerm).UnifyFrom (Substitution.Id) t
  override v.ToString() = v.Name    

/// Constants are implicitly typed (they have the type of the wrapped
/// element)
[<AbstractClassAttribute>]
type Constant() = 
  abstract member Type : IType

  interface ITerm with
    member c.Vars = []
    member c.Type = c.Type
    member c.Apply s = c :> ITerm
    member c.Normalize () = c :> ITerm
    member c.UnifyFrom s t =
      match (c :> ITerm), t with
      | c1, c2 when c1 = c2 -> Some s
      | c1, (:? IVar as v2) -> (v2 :> ITerm).UnifyFrom s c1
      | _ -> None
    member c.Unify t = 
      (c :> ITerm).UnifyFrom (Substitution.Id) t

/// Functions are used in App ITerms to indicate what function is applied.
/// They have an arbitrary-sized typed list of arguments. They return a typed
/// single value. The Identity field is None if the function is not 
/// associative; it points to the ITerm that behaves as identity otherwise
type Function = { Name: string; 
                  RetType: IType; 
                  ArgsType: IType list;
                  Identity: ITerm option }

[<CustomEqualityAttribute; NoComparisonAttribute>]
type Application = 
  { Function: Function; Args: ITerm list }
  interface ITerm with
    member f.Vars = new HashSet<_>(List.collect (fun (a: ITerm) -> a.Vars) f.Args) |> Seq.toList
    member f.Type = f.Function.RetType
    member f.Apply s = 
      { Function = f.Function; Args = List.map (fun (a: ITerm) -> a.Apply s) f.Args } :> ITerm
    
    member f.Normalize () = 
      /// Returns the children of mt, if mt encodes the application of function
      /// f; returns an empty list if it founds an identity term for the given 
      /// function; finally, it returns a singleton [t] in any other case
      let children (f: Function) (identity: ITerm) (t: ITerm) =
        match t with
        | :? Application as app -> 
          match app with 
          | { Function=f'; Args=ts } when f = f' -> ts
          | _ -> [t]
        | t when t = identity -> []
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
      f.Function = f'.Function && f.Args = f'.Args
    | _ -> 
      false
  override f.GetHashCode() =
    (f.Function, f.Args).GetHashCode()