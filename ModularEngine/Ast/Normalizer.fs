module Microsoft.Research.Dkal.Ast.Normalizer

  open Microsoft.Research.Dkal.Ast

  /// Recursively traverses the MetaTerm in order to simplify it and make
  /// it into a normal form. Consecutive associative functions, such as 
  /// conjunction, are flattened into one level
  let rec normalize (mt: MetaTerm) =

    /// Returns the children of mt, if mt encodes the application of function
    /// f; returns a singleton [mt] otherwise
    let children (f: Function) (mt: MetaTerm) =
      match mt with
      | App(f', mts) when f.Name = f'.Name -> mts
      | EmptyInfon when f.Name = "andInfon" -> []
      | True when f.Name = "andBool" -> []
      | _ -> [mt]
          
    match mt with
    | App(f, mts) -> 
      // Normalize recursively
      let mts = List.map normalize mts
      // If f is associative, get the children of each child
      let mts = if associatives.Contains(f.Name) then
                  List.collect (children f) mts
                else
                  mts
      
      // Remove malformed cases such as conjunction of zero elements, etc.
      match mts with
      | True::_ when f.Name = "asInfon" -> EmptyInfon
      | [] when f.Name = "andInfon" -> EmptyInfon
      | [mt] when f.Name = "andInfon" -> mt
      | [] when f.Name = "andBool" -> True
      | [mt] when f.Name = "andBool" -> mt
      | _ -> 
        // Return application of normalized function
        let f' =  { Name = f.Name; 
                    RetTyp = f.RetTyp; 
                    ArgsTyp = List.map (fun (mt: MetaTerm) -> mt.Typ()) mts }
        App(f', mts)

    // Constants and variables are not normalized
    | mt -> mt

  