namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

module Type = 

  type private BasicType(fullName: string, name: string) = 
    interface IType with
      member bt.FullName = fullName
      member bt.Name = name
    override bt.GetHashCode() = (bt :> IType).FullName.GetHashCode()
    override bt.Equals (o: obj) = 
      match o with
      | :? BasicType as bt' -> (bt :> IType).FullName.Equals((bt' :> IType).FullName)
      | _ -> false

  let Infon = new BasicType("Dkal.Infon", "Infon") :> IType
  let Principal = new BasicType("Dkal.Principal", "Principal") :> IType
  let SubstrateUpdate = new BasicType("Dkal.SubstrateUpdate", "SubstrateUpdate") :> IType
  let SubstrateQuery = new BasicType("Dkal.SubstrateQuery", "SubstrateQuery") :> IType
  let Action = new BasicType("Dkal.Action", "Action") :> IType
  let Condition = new BasicType("Dkal.Condition", "Condition") :> IType
  let Rule = new BasicType("Dkal.Rule", "Rule") :> IType
  let Evidence = new BasicType("Dkal.Evidence", "Evidence") :> IType

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
    | "Dkal.SubstrateUpdate" -> SubstrateUpdate
    | "Dkal.SubstrateQuery" -> SubstrateQuery
    | "Dkal.Condition" -> Condition
    | "Dkal.Action" -> Action
    | "Dkal.Rule" -> Rule
    | "Dkal.Evidence" -> Evidence
    | fn -> 
      let t = System.Type.GetType(fn)
      if t <> null then
        Substrate(t) :> IType
      else
        failwithf "Unknown type: %O, check spelling and make sure to use fully qualified names (e.g., Dkal.Principal, System.Int32)" fn

