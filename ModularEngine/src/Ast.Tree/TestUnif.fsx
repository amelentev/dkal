#I @"..\Main\bin\debug\"
#r "Interfaces.dll"
#r "Ast.dll"
#r "Ast.Tree.dll"

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Builders
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Tree.Builders

let f = {Name="f"; RetType=Type.Infon; ArgsType=[Type.Infon; Type.Infon]; Identity=None};;
let g = {Name="g"; RetType=Type.Infon; ArgsType=[Type.Infon]; Identity=None};;
let x = Var({Name="X"; Type=Type.Infon});;
let y = Var({Name="Y"; Type=Type.Infon});;

let t1 = App(f, [x; App(g, [x])]);;
let t2 = App(f, [App(g, [y]); y]);;

t1.Unify t2;;
t2.Unify t1;;
