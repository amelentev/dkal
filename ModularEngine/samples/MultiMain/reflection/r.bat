@echo off
csc /target:library ACL.cs
copy ACL.dll ..\..\..\src\MultiMain\bin\Debug\
..\..\..\src\MultiMain\bin\Debug\DkalMulti.exe acl.mdkal 100000 1000 %2