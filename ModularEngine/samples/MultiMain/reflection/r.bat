@echo off
csc /target:library ACL.cs
set DIR="..\..\..\src\MultiMain\bin\Debug\"
copy ACL.dll %DIR%
%DIR%\DkalMulti.exe acl.mdkal 100000 1000 %2