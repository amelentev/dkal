use master
GO

declare
@dbName varchar(20)
begin
set @dbName='SubstrateData'
end

if not exists(select 1 from master.dbo.sysdatabases where name =@dbName)
begin
	exec('CREATE DATABASE '+@dbName)
end
else
begin
	print @dbName + ' already exists'
end