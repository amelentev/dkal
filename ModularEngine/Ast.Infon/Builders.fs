[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Rule builders
  let RuleRule (cs: ITerm, cw: ITerm, a: ITerm) = 
    App(Primitives.SolveFunction "rule" |> Option.get, [cs; cw; a])

  // Action builders
  let SeqAction (a1: ITerm, a2: ITerm) = 
    App(Primitives.SolveFunction "seq" |> Option.get, [a1; a2])

  let SendAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction "send" |> Option.get, [ppal; i])
    
  let LearnAction (i: ITerm) = 
    App(Primitives.SolveFunction "learn" |> Option.get, [i])
    
  let ForgetAction (i: ITerm) = 
    App(Primitives.SolveFunction "forget" |> Option.get, [i])
    
  let InstallAction (r: ITerm) = 
    App(Primitives.SolveFunction "install" |> Option.get, [r])

  let UninstallAction (r: ITerm) = 
    App(Primitives.SolveFunction "uninstall" |> Option.get, [r])

//  // Substrate builders
//  let Sql (cs: ITerm) = 
//    App(Primitives.SolveFunction "sql" |> Option.get, [cs])
//
//  let Xml (file: ITerm) = 
//    App(Primitives.SolveFunction "xml" |> Option.get, [file])

  // Infon builders
  let EmptyInfon = 
    App(Primitives.SolveFunction "emptyInfon" |> Option.get, [])
    
  let AsInfon (query: ISubstrateTerm) = 
    App(Primitives.SolveFunction "asInfon" |> Option.get, [query])
    
  let AndInfon (infons: ITerm list) = 
    App({ Name = "and"; 
          RetType = Type.Infon; 
          ArgsType = List.replicate infons.Length Type.Infon }, infons)
    
  let ImpliesInfon (i1: ITerm, i2: ITerm) = 
    App(Primitives.SolveFunction "implies" |> Option.get, [i1; i2])

  let SaidInfon (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction "said" |> Option.get, [ppal; i])
    
//  // Bool builders
//  let AndBool (bools: ITerm list) = 
//    App({ Name = "and"; 
//          RetType = Bool; 
//          ArgsType = List.replicate bools.Length Bool }, bools)
//    
//
//  let OrBool (bools: ITerm list) = 
//    App({ Name = "or"; 
//          RetType = Bool; 
//          ArgsType = List.replicate bools.Length Bool }, bools)

//  // Sequence builders
//  let Nil (t: Type) =
//    App({ Name = "nil";
//          RetType = Sequence(t);
//          ArgsType = [] }, [])
//
//  let Cons (e: ITerm) (es: ITerm) =
//    App({ Name = "cons";
//          RetType = Sequence(e.Type :?>  Type) ;
//          ArgsType = [e.Type :?> Type; Sequence(e.Type :?> Type)] }, [e; es])

  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(SubstrateConstant(true))
  let False = Const(SubstrateConstant(false))

