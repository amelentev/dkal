[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Interfaces

  // Variables
  let Var (v: IVar) =
    v :> ITerm

  // Constants
  let Const (c: IConst) =
    c :> ITerm
  
  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(Constant(true))
  let False = Const(Constant(false))

  // Quantified builders
  let Forall (var: IVar, t: ITerm) = 
    { Var = var; Term = t } :> ITerm

  let ForallMany (vars: IVar list, t: ITerm) = 
    List.foldBack (fun v i -> Forall(v,i)) vars t 
