module Microsoft.Research.Dkal.Ast.Infon.Normalizer

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree

  // TODO: move this normalization to the AST implementation of ITerm 

  /// Recursively traverses the MetaTerm in order to simplify it and make
  /// it into a normal form. Consecutive associative functions, such as 
  /// conjunction, are flattened into one level
  let rec normalize (t: ITerm) = t

//    /// Returns the children of mt, if mt encodes the application of function
//    /// f; returns a singleton [mt] otherwise
//    let children (f: Function) (t: ITerm) =
//      match t with
//      | App(f', ts) when f.Name = f'.Name -> ts
//      | EmptyInfon when f.Name = "and" && f.RetType = Infon -> []
//      | True when f.Name = "and" && f.RetType = Bool -> []
//      | _ -> [t]
//          
//    match t with
//    | App(f, ts) -> 
//      // Normalize recursively
//      let ts = List.map normalize ts
//      // If f is associative, get the children of each child
//      let ts = if Primitives.IsAssociative(f.Name) then
//                 List.collect (children f) ts
//               else
//                 ts
//      
//      // Remove malformed cases such as conjunction of zero elements, etc.
//      match ts with
//      | True::_ when f.Name = "asInfon" -> EmptyInfon
//      | [] when f.Name = "and" && f.RetType = Infon -> EmptyInfon
//      | [t] when f.Name = "and" && f.RetType = Infon -> t
//      | [] when f.Name = "and" && f.RetType = Bool -> True
//      | [t] when f.Name = "and" && f.RetType = Bool -> t
//      | _ -> 
//        // Return application of normalized function
//        let f' =  { Name = f.Name; 
//                    RetType = f.RetType; 
//                    ArgsType = List.map (fun (t: ITerm) -> t.Type :?> Type) ts }
//        App(f', ts)
//
//    // Constants and variables are not normalized
//    | mt -> mt
//
//  