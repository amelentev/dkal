[<AutoOpen>]
module Microsoft.Research.Dkal.Substrate.Sql.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast.Infon

  // Bool builders
  let AndBool (bools: ITerm list) = 
    App({ Name = "and"; 
          RetType = Type.Boolean; 
          ArgsType = List.replicate bools.Length Type.Boolean;
          Identity = Some True }, bools)
    

  let OrBool (bools: ITerm list) = 
    App({ Name = "or"; 
          RetType = Type.Boolean; 
          ArgsType = List.replicate bools.Length Type.Boolean;
          Identity = Some False }, bools)
 
