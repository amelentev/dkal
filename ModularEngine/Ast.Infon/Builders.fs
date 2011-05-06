[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Rule builders
  let SeqRule (rs: ITerm list) = 
    App({ Name = Primitives.SeqRule; 
          RetType = Type.Rule; 
          ArgsType = List.replicate rs.Length Type.Rule;
          Identity = (Primitives.SolveFunction Primitives.SeqRule).Value.Identity }, rs)

  let EmptyRule = 
    App(Primitives.SolveFunction Primitives.EmptyRule |> Option.get, [])

  let RuleRule (c: ITerm, a: ITerm) = 
    App(Primitives.SolveFunction Primitives.Rule |> Option.get, [c; a])

  let RuleOnceRule (c: ITerm, a: ITerm) = 
    App(Primitives.SolveFunction Primitives.RuleOnce |> Option.get, [c; a])

  // Condition builders
  let SeqCondition (conds: ITerm list) = 
    App({ Name = Primitives.SeqCondition; 
          RetType = Type.Condition; 
          ArgsType = List.replicate conds.Length Type.Condition;
          Identity = (Primitives.SolveFunction Primitives.SeqCondition).Value.Identity }, conds)

  let EmptyCondition = 
    App(Primitives.SolveFunction Primitives.EmptyCondition |> Option.get, [])

  let WireCondition (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.WireCondition |> Option.get, [i])

  let KnownCondition (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.KnownCondition |> Option.get, [i])

  // Action builders
  let SeqAction (actions: ITerm list) = 
    App({ Name = Primitives.SeqAction; 
          RetType = Type.Action; 
          ArgsType = List.replicate actions.Length Type.Action;
          Identity = (Primitives.SolveFunction Primitives.SeqAction).Value.Identity }, actions)

  let EmptyAction = 
    App(Primitives.SolveFunction Primitives.EmptyAction |> Option.get, [])

  let SendAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Send |> Option.get, [ppal; i])
    
  let SayAction (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Say |> Option.get, [ppal; i])

  let LearnAction (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Learn |> Option.get, [i])
    
  let ForgetAction (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Forget |> Option.get, [i])
    
  let InstallAction (r: ITerm) = 
    App(Primitives.SolveFunction Primitives.Install |> Option.get, [r])

  let UninstallAction (r: ITerm) = 
    App(Primitives.SolveFunction Primitives.Uninstall |> Option.get, [r])

  let ApplyAction (su: ITerm) = 
    App(Primitives.SolveFunction Primitives.Apply |> Option.get, [su])

  let DropAction (i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Drop |> Option.get, [i])

  // Infon builders
  let EmptyInfon = 
    App(Primitives.SolveFunction Primitives.EmptyInfon |> Option.get, [])
    
  let AsInfon (query: ISubstrateQueryTerm) = 
    App(Primitives.SolveFunction Primitives.AsInfon |> Option.get, [query])
    
  let AndInfon (infons: ITerm list) = 
    App({ Name = Primitives.And; 
          RetType = Type.Infon; 
          ArgsType = List.replicate infons.Length Type.Infon;
          Identity = (Primitives.SolveFunction Primitives.And).Value.Identity }, infons)
    
  let ImpliesInfon (i1: ITerm, i2: ITerm) = 
    App(Primitives.SolveFunction Primitives.Implies |> Option.get, [i1; i2])

  let SaidInfon (ppal: ITerm, i: ITerm) = 
    App(Primitives.SolveFunction Primitives.Said |> Option.get, [ppal; i])
  
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


