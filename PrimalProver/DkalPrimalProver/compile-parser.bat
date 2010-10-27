@echo After running this script rename uniformly "mStack" by "dstack" in the file parser.cs. This is due to a bug in CUP.

C#Lex.exe primal.lex

C#Cup.exe -parser Parser -symbols Sym primal.cup
