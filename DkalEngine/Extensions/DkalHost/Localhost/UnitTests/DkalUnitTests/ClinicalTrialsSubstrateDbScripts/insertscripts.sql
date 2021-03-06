-- *********************************************************
--
--    Copyright (c) Microsoft. All rights reserved.
--    This code is licensed under the Apache License, Version 2.0.
--    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
--    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
--    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
--    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
--
-- *********************************************************
-- Clinical Trials Scenario

--patients table data     
INSERT into [ECMDB].[dbo].[patients]
(id,	name)
select 10, 'John Smith'  

--principals table data          
INSERT into [ECMDB].[dbo].[principals]
(id,	name)
select 1, '_cro'
union
select 2, '_site' 
union
select 3, '_physician' 
union
select 4, '_kgc' 

--records table data
insert into [ECMDB].[dbo].[records]
     ( [id]
      ,[patient]
      ,[value]
      ,[is_clarification]
      ,[trial] )
     select 0,	10,	'test rcord for 0',	1,	1
     union
     select 1,	10,	'test rcord for 1',	0,	1
     union
     select 2,	10,	'test rcord for 2',	1,	1    
     
--trials table data    
insert into [ECMDB].[dbo].[trials](
       [id]
      ,[cro]
      ,[name])
      select 1,	1,	'Common Flu Trial'          