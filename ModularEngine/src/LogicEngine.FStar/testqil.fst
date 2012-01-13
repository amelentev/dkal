module TestQIL
open Types
open QIL

let said p i = App SaidInfon [(Const (PrincipalConstant p)); i]
let implies i j = App ImpliesInfon [i;j]
let andi i j = App AndInfon [i;j]
let x = App (RelationInfon ({fname="X";
                             retType=Infon;
                             argsType=[];
                             identity=None})) []

let y = App (RelationInfon ({fname="Y";
                             retType=Infon;
                             argsType=[];
                             identity=None})) []

let bless i = 
  map (fun (i:term) -> 
         let j = MonoTerm i in 
           assume (CheckedInfon j); 
           (j:infon)) i

val fst : 'a::* -> 'b::('a => *) -> (x:'a * 'b x) -> 'a
let fst xy = let (x, _) = xy in x

;; 

  let infostrate = [(andi (said "p" x) (said "p" y))] in  
  let sf = collect subformulas (append infostrate ([(said "p" (andi x y))])) in 
  let _ = println "Got subformulas" in
  let k = bless infostrate in 
  let _ = println "Got infostrate ... go!" in
  let facts = deriveAllWrapper sf ["p"; "q"; "r"] k in
  let _ = println "All derivable facts: " in
  let _ = iterate (fun (i : dterm [] k) -> let (j, _) = i in  println (string_of_any_for_coq j); ()) facts in
     ()


  (* let infostrate = [(said "p" (said "q" (implies (said "r" x) y))); *)
  (*                   (said "p" (said "q" (said "r" x)))] in *)
  (* let sf = collect subformulas infostrate in *)
  (* let _ = println "Got subformulas" in  *)
  (* let k = bless infostrate in *)
  (* let _ = println "Got infostrate ... go!" in  *)
  (* let facts = deriveAllWrapper sf ["p"; "q"; "r"] k in *)
  (* let _ = println "All derivable facts: " inlet _ = iterate (fun (i : dterm [] k) -> let (j, _) = i in  println (string_of_any_for_coq j); ()) facts in  *)
  (*    () *)





                    

