drop table dkal_messages_demo;
create table dkal_messages_demo (
  sentAt Datetime primary key DEFAULT (current_timestamp) ,
  readAt datetime,
  sender int,
  reciver int,
  msg text
);
