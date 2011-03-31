@echo off
type %1.txt
echo ------------------------------------------------------
..\DatalogBackend\bin\Debug\DatalogBackend.exe %1.txt > %1.datalog 
type %1.datalog
echo ------------------------------------------------------
z3 %1.datalog
