[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Interfaces

  // Variables
  let Var (v: Variable) =
    v :> ITerm

  // Constants
  let Const (c: IConst<_>) =
    c :> ITerm
  
  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(Constant<bool>(true))
  let False = Const(Constant<bool>(false))