use SubstrateData

BEGIN TRY

IF OBJECT_ID('dbo.physPatients','U') IS NOT NULL drop table physPatients
IF OBJECT_ID('dbo.siteAssignments','U') IS NOT NULL drop table siteAssignments
IF OBJECT_ID('dbo.physAssignments','U') IS NOT NULL drop table physAssignments
IF OBJECT_ID('dbo.keys','U') IS NOT NULL drop table keys
IF OBJECT_ID('dbo.records','U') IS NOT NULL drop table records
IF OBJECT_ID('dbo.trials','U') IS NOT NULL drop table trials
IF OBJECT_ID('dbo.dkal_principals','U') IS NOT NULL drop table dkal_principals

create table dkal_principals (
  id int primary key,
  name text)

insert into dkal_principals 
select 1, 'keyManager'
union
select 2, 'site1'
union
select 3, 'org1'
union
select 4, 'phys1'

create table trials (
  id int primary key,
  cro int,
  name text)

insert into trials
select 42, 3, 'trial1'

create table records (
  id int primary key,
  patient int,
  trial int references trials,
  data text)
  
insert into records
select 101, 1005, 42, 'ala ma kota'
union
select 102, 1015, 42, 'kot ma ale'
union
select 103, 1016, 42, 'abecadlo'
union
select 104, 1100, 42, 'z pieca spadlo'


create table keys (
  record int primary key references records,
  thekey int)
  
insert into keys
select 101, 13
union
select 102, 1313
union
select 103, 131313
union
select 104, 13131313

create table physAssignments (
  phys int references dkal_principals,
  trial int references trials,
  unnotified bit,
  n1 int,
  n2 int)
  
create clustered index physAssignmentsI1 on physAssignments ( phys, trial )

insert into physAssignments 
select 4, 42, 1, 1010, 1050

create table siteAssignments (
  thesite int references dkal_principals,
  trial int references trials,
  unnotified bit,
  n1 int,
  n2 int)
  
create clustered index siteAssignmentsI1 on siteAssignments ( thesite, trial )

insert into siteAssignments
select 2, 42, 1, 1000, 1250

create table physPatients (
  id int primary key,
  needInfo bit)
  
insert into physPatients
select 1015, 1
union
select 1016, 1
union
select 1100, 1

END TRY
BEGIN CATCH
    -- Execute error retrieval routine.
    SELECT
    ERROR_NUMBER() AS ErrorNumber
    ,ERROR_SEVERITY() AS ErrorSeverity
    ,ERROR_STATE() AS ErrorState
    ,ERROR_PROCEDURE() AS ErrorProcedure
    ,ERROR_LINE() AS ErrorLine
    ,ERROR_MESSAGE() AS ErrorMessage 
END CATCH


