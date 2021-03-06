/****** Object:  Table [dbo].[principals]    Script Date: 11/25/2009 12:50:21 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[principals]') AND type in (N'U'))
DROP TABLE [dbo].[principals]
GO
/****** Object:  Table [dbo].[trials]    Script Date: 11/25/2009 12:50:21 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[trials]') AND type in (N'U'))
DROP TABLE [dbo].[trials]
GO
/****** Object:  Table [dbo].[records]    Script Date: 11/25/2009 12:50:21 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[records]') AND type in (N'U'))
DROP TABLE [dbo].[records]
GO
/****** Object:  Table [dbo].[patients]    Script Date: 11/25/2009 12:50:21 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[patients]') AND type in (N'U'))
DROP TABLE [dbo].[patients]
GO
/****** Object:  Schema [ftouser]    Script Date: 11/25/2009 12:50:21 ******/
IF  EXISTS (SELECT * FROM sys.schemas WHERE name = N'ftouser')
DROP SCHEMA [ftouser]
GO
/****** Object:  Role [ftouser]    Script Date: 11/25/2009 12:50:21 ******/
IF NOT EXISTS (SELECT * FROM sys.database_principals WHERE name = N'ftouser')
BEGIN
IF NOT EXISTS (SELECT * FROM sys.database_principals WHERE name = N'ftouser' AND type = 'R')
CREATE ROLE [ftouser]

END
GO
/****** Object:  Schema [ftouser]    Script Date: 11/25/2009 12:50:21 ******/
IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = N'ftouser')
EXEC sys.sp_executesql N'CREATE SCHEMA [ftouser] AUTHORIZATION [ftouser]'
GO
/****** Object:  Table [dbo].[patients]    Script Date: 11/25/2009 12:50:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[patients]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[patients](
	[id] [int] NOT NULL,
	[name] [nchar](32) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
END
GO
CREATE CLUSTERED INDEX [IX_patients] ON [dbo].[patients] 
(
	[id] ASC
)WITH ( STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
INSERT [dbo].[patients] ([id], [name]) VALUES (10, N'John Smith                      ')
/****** Object:  Table [dbo].[records]    Script Date: 11/25/2009 12:50:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[records]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[records](
	[id] [int] NOT NULL,
	[patient] [int] NOT NULL,
	[value] [nchar](20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[is_clarification] [bit] NOT NULL,
	[trial] [int] NOT NULL
)
END
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[records]') AND name = N'IX_records')
CREATE CLUSTERED INDEX [IX_records] ON [dbo].[records] 
(
	[id] ASC
)WITH ( STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
INSERT [dbo].[records] ([id], [patient], [value], [is_clarification], [trial]) VALUES (0, 10, N'blah                ', 1, 1)
INSERT [dbo].[records] ([id], [patient], [value], [is_clarification], [trial]) VALUES (1, 10, N'blah not clar       ', 0, 1)
/****** Object:  Table [dbo].[trials]    Script Date: 11/25/2009 12:50:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[trials]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[trials](
	[id] [int] NOT NULL,
	[cro] [nchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[name] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
END
GO
CREATE CLUSTERED INDEX [IX_trails] ON [dbo].[trials] 
(
	[id] ASC
)WITH ( STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
INSERT [dbo].[trials] ([id], [cro], [name]) VALUES (1, N'1                             ', N'Common Flu Trial')
/****** Object:  Table [dbo].[principals]    Script Date: 11/25/2009 12:50:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[principals]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[principals](
	[id] [int] NOT NULL,
	[name] [nchar](40) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
END
GO
CREATE CLUSTERED INDEX [IX_principals] ON [dbo].[principals] 
(
	[id] ASC
)WITH ( STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
INSERT [dbo].[principals] ([id], [name]) VALUES (1, N'_cro                                    ')
INSERT [dbo].[principals] ([id], [name]) VALUES (2, N'_site                                   ')
INSERT [dbo].[principals] ([id], [name]) VALUES (3, N'_cgc                                    ')
INSERT [dbo].[principals] ([id], [name]) VALUES (4, N'_physician                              ')
