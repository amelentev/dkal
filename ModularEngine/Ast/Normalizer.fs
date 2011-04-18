module Microsoft.Research.Dkal.Ast.Normalizer

  open Microsoft.Research.Dkal.Ast

  let rec normalize (mt: MetaTerm) =
    let children (f: Function) (mt: MetaTerm) =
      match mt with
      | App(f', mts) when f.Name = f'.Name -> mts
      | EmptyInfon when f.Name = "andInfon" -> []
      | True when f.Name = "andBool" -> []
      | _ -> [mt]
          
    match mt with
    | App(f, mts) -> 
      let mts = List.map normalize mts
      let mts = if associatives.Contains(f.Name) then
                  List.collect (children f) mts
                else
                  mts
      
      match mts with
      | True::_ when f.Name = "asInfon" -> EmptyInfon
      | [] when f.Name = "andInfon" -> EmptyInfon
      | [mt] when f.Name = "andInfon" -> mt
      | [] when f.Name = "andBool" -> True
      | [mt] when f.Name = "andBool" -> mt
      | _ -> 
        let f' =  { Name = f.Name; 
                    RetTyp = f.RetTyp; 
                    ArgsTyp = List.map (fun (mt: MetaTerm) -> mt.Typ()) mts }
        App(f', mts)
    | mt -> mt

  