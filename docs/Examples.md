# DKAL Examples 

This section is divided in two: 
* How to run the [examples for the single principal front ends](#examplesSingle) ([Dkal.exe](UsingDKAL#usingDkalMain) and [DkalGui.exe](UsingDKAL#usingDkalGuiMain))
* How to run the [examples for the multi principal front end](#examplesMulti) ([DkalMulti.exe](UsingDKAL#usingDkalMultiMain))

## {anchor:examplesSingle} Single Principal Examples  
Currently there is one single principal example, the Clinical Trials example from the paper on [Evidential Authorization using DKAL](http://research.microsoft.com/en-us/um/people/gurevich/Opera/203.pdf).

### {anchor:examplesSingleClinical} Clinical Trials Example
This example is located in the "ModularEngine\Samples\ClinicalTrials" folder. 

Before running this example you will need to setup a SQL Server database. If you don't have access to a SQL Server consider running the [examples for the multi principal front end](#examplesMulti), which don't have this requirement.

In order to set up the SQL Server database you first need to do three things:
# Create a SQL database and create a SQL user that can read and write from this database.
# Run the "ModularEngine\Samples\ClinicalTrials\Setup.sql" script in this database.
# Adjust the connection string on the substrate definition of "org1.dkal", "site1.dkal", "phys1.dkal" and "keyMgr.dkal" to point to your newly created database. You will possibly need to modify the location of the SQL Server (host and port), the name of the database, the name of the SQL user and its password. Optionally, you can add any other parameters you may need to the connection string.

Once the database is configured, you need to assign rights to your Windows username so that you can open a few ports. In an administrative shell, execute the following commands:
{{netsh http add urlacl url=http://+:55330/ user=<DOMAIN>\<Username>}}
{{netsh http add urlacl url=http://+:55331/ user=<DOMAIN>\<Username>}}
{{netsh http add urlacl url=http://+:55332/ user=<DOMAIN>\<Username>}}
{{netsh http add urlacl url=http://+:55333/ user=<DOMAIN>\<Username>}}

With a configured database and the ports assigned you may execute "runConsole.bat" to run the example using the command-line frontend (with enabled step by step pauses); alternatively, execute "runGui.bat" to run the example using the GUI front end.

## {anchor:examplesMulti} Multi Principal Examples  
The examples for the multi principal front end (DkalMulti.exe) are located in the "ModularEngine\Samples\MultiMain" folder.

Each of the examples is contained in a single file with the ".mdkal" file extension (e.g., "calc.mdkal" or "clinical_trials.mdkal"). You may run any of these by invoking "r.bat" passing the mdkal file as the only parameter.
