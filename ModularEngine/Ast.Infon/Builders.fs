[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Rule builders
  let SeqRule (rs: ITerm list) = 
    App({ Name = "seqRule"; 
          RetType = Type.Rule; 
          ArgsType = List.replicate rs.Length Type.Rule;
          Identity = (Primitives.SolveFunction "seqRule").Value.Identity }, rs)

  let EmptyRule = 
    App(Primitives.SolveFunction "emptyRule" |> Option.get, [])

  let RuleRule (c: ITerm, a: ITerm) = 
    App(Primitives.SolveFunction "rule" |> Option.get, [c; a])

  // Condition builders
  let SeqCondition (conds: ITerm list) = 
    App({ Name = "seqCondition"; 
          RetType = Type.Condition; 
          ArgsType = List.replicate conds.Length Type.Condition;
          Identity = (Primitives.SolveFunction "seqCondition").Value.Identity }, conds)

  let EmptyCondition = 
    App(Primitives.SolveFunction "emptyCondition" |> Option.get, [])

  let WireCondition (i: ITerm) = 
    App(Primitives.SolveFunction "wireCondition" |> Option.get, [i])

  let KnownCondition (i: ITerm) = 
    App(Primitives.SolveFunction "knownCondition" |> Option.get, [i])

  // Action builders
  let SeqAction (actions: ITerm list) = 
    App({ Name = "seqAction"; 
          RetType = Type.Action; 
          ArgsType = List.replicate actions.Length Type.Action;
          Identity = (Primitives.SolveFunction "seqAction").Value.Identity }, actions)

  let EmptyAction = 
    App(Primitives.SolveFunction "emptyAction" |> Option.get, [])

  let SendAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction "send" |> Option.get, [ppal; i])
    
  let SayAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction "say" |> Option.get, [ppal; i])

  let LearnAction (i: ITerm) = 
    App(Primitives.SolveFunction "learn" |> Option.get, [i])
    
  let ForgetAction (i: ITerm) = 
    App(Primitives.SolveFunction "forget" |> Option.get, [i])
    
  let InstallAction (r: ITerm) = 
    App(Primitives.SolveFunction "install" |> Option.get, [r])

  let UninstallAction (r: ITerm) = 
    App(Primitives.SolveFunction "uninstall" |> Option.get, [r])

  // Infon builders
  let EmptyInfon = 
    App(Primitives.SolveFunction "emptyInfon" |> Option.get, [])
    
  let AsInfon (query: ISubstrateTerm) = 
    App(Primitives.SolveFunction "asInfon" |> Option.get, [query])
    
  let AndInfon (infons: ITerm list) = 
    App({ Name = "and"; 
          RetType = Type.Infon; 
          ArgsType = List.replicate infons.Length Type.Infon;
          Identity = (Primitives.SolveFunction "and").Value.Identity }, infons)
    
  let ImpliesInfon (i1: ITerm, i2: ITerm) = 
    App(Primitives.SolveFunction "implies" |> Option.get, [i1; i2])

  let SaidInfon (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction "said" |> Option.get, [ppal; i])
  
  let PrefixedInfon (ppals: ITerm list, i: ITerm) =
    List.foldBack (fun ppal i -> SaidInfon(ppal, i)) ppals i
    
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

