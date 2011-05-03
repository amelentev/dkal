[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Interfaces

  // Variables
  let Var (v: Variable) =
    v :> ITerm

  // Constants
  let Const (c: IConst) =
    c :> ITerm
  
  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(Constant(true))
  let False = Const(Constant(false))