@echo off
type %1.txt
echo ------------------------------------------------------
..\DatalogBackend\bin\Debug\DatalogBackend.exe %1.txt > %1.datalog 
type %1.datalog
echo ------------------------------------------------------
C:\Users\t-espav\Documents\z3-4.3.0-x64\bin\z3 %1.datalog
