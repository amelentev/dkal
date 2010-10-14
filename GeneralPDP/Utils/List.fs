namespace Microsoft.Research.GeneralPDP.Utils

open System.Collections.Generic

module List =

  let rec elementsInAll (elemLists: 'a seq seq) =
    match Seq.toList elemLists with
    | [] -> failwith "intersection of empty list"
    | elemList :: elemLists -> 
    let set = HashSet<'a>(elemList)
    for elemList in elemLists do
      set.IntersectWith(elemList)
    seq set
    

