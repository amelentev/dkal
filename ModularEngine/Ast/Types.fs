namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

module Type = 
  let Infon = { new IType with 
                  member t.Name = "Infon" 
                  member t.FullName = "Dkal.Infon" }
  let Principal = { new IType with 
                      member t.Name = "Principal" 
                      member t.FullName = "Dkal.Principal" }
  let SubstrateUpdate = { new IType with 
                            member t.Name = "SubstrateUpdate"
                            member t.FullName = "Dkal.SubstrateUpdate" }
  let SubstrateQuery = { new IType with 
                            member t.Name = "SubstrateQuery"
                            member t.FullName = "Dkal.SubstrateQuery" }
  let Action =  { new IType with 
                    member t.Name = "Action"
                    member t.FullName = "Dkal.Action" }
  let Condition = { new IType with 
                      member t.Name = "Condition" 
                      member t.FullName = "Dkal.Condition" }
  let Rule = { new IType with 
                 member t.Name = "Rule" 
                 member t.FullName = "Dkal.Rule" }
  let Evidence = { new IType with 
                    member t.Name = "Evidence" 
                    member t.FullName = "Dkal.Evidence" }

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

