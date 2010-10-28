#!/bin/bash
../DkalToDatalog/bin/Debug/DkalToDatalog ObjectVars $1 | tee $1.datalog
echo
echo "-------------------------------------------------------"
echo
time ../DkalToDatalog/bin/Debug/z3_3_dbg.exe $1.datalog | grep -E -e \(Tuples\|=\) -

