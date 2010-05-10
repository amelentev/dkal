drop table dkal_messages_demo;
create table dkal_messages_demo (
sentAt datetime primary key default getutcdate(),
readAt datetime,
sender int,
reciver int,
msg text);

