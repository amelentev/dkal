type %1.txt && ..\DatalogBackend\bin\Debug\DatalogBackend.exe %1.txt > %1.datalog && type %1.datalog && z3 %1.datalog
