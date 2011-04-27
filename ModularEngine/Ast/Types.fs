namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

module Type = 
  let Infon = { new IType with 
                  member t.Name = "Infon" 
                  member t.FullName = "Dkal.Infon" }
  let Principal = { new IType with 
                      member t.Name = "Principal" 
                      member t.FullName = "Dkal.Principal" }
  let Action =  { new IType with 
                    member t.Name = "Action"
                    member t.FullName = "Dkal.Action" }
  let Rule =  { new IType with 
                 member t.Name = "Rule" 
                 member t.FullName = "Dkal.Rule" }
  
  type Substrate(typ: System.Type) = 
    interface IType
      with 
        member t.FullName = typ.FullName
        member t.Name = typ.Name
    member s.Type = typ
    override s.Equals t' = match t' with
                           | :? Substrate as t' -> typ.Equals(t'.Type)
                           | _ -> false
    override s.GetHashCode() = typ.GetHashCode()

  // type shortcuts
  let Boolean = Substrate(typeof<bool>) :> IType
  let Int32 = Substrate(typeof<int32>) :> IType
  let Double = Substrate(typeof<double>) :> IType
  let String = Substrate(typeof<string>) :> IType

  let FromFullName fn = 
    match fn with
    | "Dkal.Infon" -> Infon
    | "Dkal.Principal" -> Principal
    | "Dkal.Action" -> Action
    | "Dkal.Rule" -> Rule
    | fn -> Substrate(System.Type.GetType(fn)) :> IType

/// Type is used to represent AST types
//type Type = 
//| Bool
//| Principal
//| Infon
//| Action
//| Rule
//| Substrate
//| Sequence of Type
//| Tuple of Type * Type
//| SubstrateElem of System.Type
//
//  interface IType with
//    member t.Name = t.ToString()
//
//  static member Int = SubstrateElem(typeof<int>)
//  static member Float = SubstrateElem(typeof<float>)
//  static member String = SubstrateElem(typeof<string>)
//  override t.ToString() = 
//    match t with
//    | SubstrateElem(typ) -> typ.Name
//    | t -> sprintf "%A" t
//      
//  member t.hasEquality () = 
//    match t with
//    | Type.Bool
//    | Type.Principal -> true
//    | Type.SubstrateElem t' -> 
//      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IEquatable"))
//    | Type.Sequence(t') -> t'.hasEquality()
//    | Type.Tuple(t1, t2) -> t1.hasEquality() && t2.hasEquality()
//    | _ -> false
//
//  member t.hasOrdering () = 
//    match t with
//    | Type.SubstrateElem t' -> 
//      t'.GetInterfaces() |> Seq.exists (fun i -> i.Name.StartsWith("IComparable"))
//    | Type.Sequence(t') ->  t'.hasOrdering()
//    | Type.Tuple(t1, t2) -> t1.hasOrdering() && t2.hasOrdering()
//    | _ -> false
//  
//  member t.hasSum () =
//    match t with
//    | Type.SubstrateElem t' when t' = typeof<int> || t' = typeof<float> -> true
//    | Type.Sequence(_) -> true
//    | _ -> false
//
//  member t.hasSubstraction () =
//    match t with
//    | Type.SubstrateElem t' when t' = typeof<int> || t' = typeof<float> -> true
//    | _ -> false
//
//  member t.hasArithmeticNegation () =
//    match t with
//    | Type.SubstrateElem t' when t' = typeof<int> || t' = typeof<float> -> true
//    | _ -> false
//
//  member t.hasMultiplication () =
//    match t with
//    | Type.SubstrateElem t' when t' = typeof<int> || t' = typeof<float> -> true
//    | _ -> false
//
//  member t.hasDivision () =
//    match t with
//    | Type.SubstrateElem t' when t' = typeof<int> || t' = typeof<float> -> true
//    | _ -> false
//
//  member t.hasConjunction () =
//    match t with
//    | Type.Bool | Type.Infon -> true
//    | _ -> false
//
//  member t.hasDisjunction () =
//    match t with
//    | Type.Bool -> true
//    | _ -> false
//
//  member t.hasImplication () =
//    match t with
//    | Type.Infon -> true
//    | _ -> false
//
//  member t.hasLogicalNegation () =
//    match t with
//    | Type.Bool -> true
//    | _ -> false
//
//
