[<AutoOpen>]
module Microsoft.Research.Dkal.Substrate.Basic.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Tree

  // Bool builders
  let AndBool (bools: ITerm list) = 
    App({ Name = BasicPrimitives.And; 
          RetType = Type.Boolean; 
          ArgsType = List.replicate bools.Length Type.Boolean;
          Identity = Some True }, bools)

  let OrBool (bools: ITerm list) = 
    App({ Name = BasicPrimitives.Or; 
          RetType = Type.Boolean; 
          ArgsType = List.replicate bools.Length Type.Boolean;
          Identity = Some False }, bools)
     
