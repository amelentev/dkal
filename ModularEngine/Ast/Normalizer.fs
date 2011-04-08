module Microsoft.Research.Dkal.Ast.Normalizer

  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Primitives

  let rec normalize (mt: MetaTerm) =
    let children f mt =
      match mt with
      | App(f', mts) when f'.Name = f.Name -> mts
      | mt -> 
        if f.Name = "andInfon" && mt = Primitives.trueInfon then
          []
        elif f.Name = "andBool" && mt = Primitives.trueBool then
          []
        else
          [mt]
    
    match mt with
    | App(f, mts) -> 
      let mts = List.map normalize mts
      let mts = if associatives.Contains(f.Name) then
                  List.collect (children f) mts
                else
                  mts
      if f.Name = "andInfon" && mts = [] then
        Primitives.trueInfon
      elif f.Name = "andBool" && mts = [] then
        Primitives.trueBool
      else
        let f' =  { Name = f.Name; 
                    RetTyp = f.RetTyp; 
                    ArgsTyp = List.map (fun (mt: MetaTerm) -> mt.Typ()) mts }
        App(f', mts)
    | mt -> mt

  