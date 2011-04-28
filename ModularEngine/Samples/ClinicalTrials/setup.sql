-- the same as old EDKAL-Paper sample
IF OBJECT_ID('dbo.physPatients','U') IS NOT NULL drop table physPatients;
IF OBJECT_ID('dbo.siteAssignments','U') IS NOT NULL drop table siteAssignments;
IF OBJECT_ID('dbo.physAssignments','U') IS NOT NULL drop table physAssignments;
IF OBJECT_ID('dbo.keys','U') IS NOT NULL drop table keys;
IF OBJECT_ID('dbo.records','U') IS NOT NULL drop table records;
IF OBJECT_ID('dbo.trials','U') IS NOT NULL drop table trials;
IF OBJECT_ID('dbo.dkal_principals','U') IS NOT NULL drop table dkal_principals;

create table dkal_principals (
  id int primary key,
  name text 
);
insert into dkal_principals values
  ( 1, 'keyManager'),
  ( 2, 'site1'),
  ( 3, 'org1' ),
  ( 4, 'phys1' );

create table trials (
  id int primary key,
  cro int,
  name text
);
insert into trials values
  ( 42, 3, 'trial1' );

create table records (
  id int primary key,
  patient int,
  trial int references trials,
  data text
);
insert into records values
  ( 101, 1005, 42, 'ala ma kota' ),
  ( 102, 1015, 42, 'kot ma ale' ),
  ( 103, 1016, 42, 'abecadlo' ),
  ( 104, 1100, 42, 'z pieca spadlo' );


create table keys (
  record int primary key references records,
  thekey int
);
insert into keys values
  ( 101, 13),
  ( 102, 1313),
  ( 103, 131313 ),
  ( 104, 13131313 );

create table physAssignments (
  phys int references dkal_principals,
  trial int references trials,
  unnotified bit,
  n1 int,
  n2 int
);
create clustered index physAssignmentsI1 on physAssignments ( phys, trial );
insert into physAssignments values
  ( 4, 42, 1, 1010, 1050 );

create table siteAssignments (
  thesite int references dkal_principals,
  trial int references trials,
  unnotified bit,
  n1 int,
  n2 int
);
create clustered index siteAssignmentsI1 on siteAssignments ( thesite, trial );
insert into siteAssignments values
  ( 2, 42, 1, 1000, 1250 );

create table physPatients (
  id int primary key,
  needInfo bit
);
insert into physPatients values
  ( 1015, 1 ),
  ( 1016, 1 ),
  ( 1100, 1 );
