GeneralPDP release notes:
-------------------------

1) Compile the DkalEngine project, found in the dkal\DkalEngine directory

2) Copy the following file to dkal\GeneralPDP\
	- dkal\DkalEngine\bin\Debug\DkalEngine.dll

3) Create a file called connection_string.txt with the following contents, 
   replacing SERVER, PORT, DB, USER and PASSWORD so that they point to a 
   SQL Server database on which you have permission to execute queries:

Server=tcp:SERVER,PORT;Database=DB;User ID=USER;Password=PASSWORD;TrustServerCertificate=true;Trusted_Connection=False;Encrypt=True;

4) 
a) Copy the connection_string.txt file to each subfolder of dkal\GeneralPDP\Samples
b) Some subfolders of dkal\GeneralPDP\Samples contain a 'readme.txt' file. 
   If that is the case, follow the instructions in case you want to run that particular example.

5) Edit the properties of the dkal\GeneralPDP\Main project, go to the 'Debug' pane 
   and point the 'Working directory' to any of the subfolders in dkal\GeneralPDP\Samples
