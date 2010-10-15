GeneralPDP release notes:
-------------------------

1) Compile the DkalEngine project, found in the dkal\DkalEngine directory

2) Copy the following file to dkal\GeneralPDP\
	- dkal\DkalEngine\bin\Debug\DkalEngine.dll

3) Edit line 7 in dkal\GeneralPDP\DkalEngineInterface\DkalCommon.fs so that it points to a SQL server DB of your choice. This is used to solve DKAL asInfon expressions

4) Edit the properties of the dkal\GeneralPDP\Main project, go to the 'Debug' pane and point the 'Working directory' to any of the subfolders in dkal\GeneralPDP\Samples
