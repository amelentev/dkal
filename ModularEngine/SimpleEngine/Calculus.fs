module Microsoft.Research.Dkal.SimpleEngine.Calculus

open Microsoft.Research.Dkal.Ast

type SequentRule = SequentRule of MetaTerm list * MetaTerm list

let andElimL = SequentRule([AndInfon [Var({Name = "X1"; Typ = Infon}); Var({Name = "X2"; Typ = Infon})]], 
                            [Var({Name = "X1"; Typ = Infon})])
let andElimR = SequentRule([AndInfon [Var({Name = "X1"; Typ = Infon}); Var({Name = "X2"; Typ = Infon})]], 
                            [Var({Name = "X2"; Typ = Infon})])
let andIntro = SequentRule([Var({Name = "X1"; Typ = Infon}); Var({Name = "X2"; Typ = Infon})], 
                            [AndInfon [Var({Name = "X1"; Typ = Infon}); Var({Name = "X2"; Typ = Infon})]])

let impliesElim = SequentRule([ImpliesInfon(Var({Name = "X1"; Typ = Infon}), Var({Name = "X2"; Typ = Infon})); Var({Name = "X1"; Typ = Infon})], 
                              [Var({Name = "X2"; Typ = Infon})])
let impliesIntro = SequentRule([Var({Name = "X2"; Typ = Infon})], 
                                [ImpliesInfon(Var({Name = "X1"; Typ = Infon}), Var({Name = "X2"; Typ = Infon}))])

