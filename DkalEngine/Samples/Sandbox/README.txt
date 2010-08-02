Runing the policy:

../../DkalShell/bin/Debug/DkalShell.exe -i passwords.dkalsx -i common.dkalsx org1.dkalsx site1.dkalsx phys1.dkalsx keymgr.dkalsx

This requires passwords.dkalsx file, which contains a line like (at least for the SQL Azure substrate):

(set private_sql "Server=tcp:<...>.database.windows.net;Database=<...>;User ID=<...>;Password=<...>;Trusted_Connection=False;Encrypt=True;")

The database should be initilized according to setup.sql script.

