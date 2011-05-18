USE [ClinicalTrials]
GO
/****** Object:  ForeignKey [FK_physAssignments_trials]    Script Date: 05/16/2011 17:40:47 ******/
ALTER TABLE [dbo].[physAssignments] DROP CONSTRAINT [FK_physAssignments_trials]
GO
/****** Object:  ForeignKey [FK_records_trials]    Script Date: 05/16/2011 17:40:48 ******/
ALTER TABLE [dbo].[records] DROP CONSTRAINT [FK_records_trials]
GO
/****** Object:  ForeignKey [FK_siteAssignments_trials]    Script Date: 05/16/2011 17:40:48 ******/
ALTER TABLE [dbo].[siteAssignments] DROP CONSTRAINT [FK_siteAssignments_trials]
GO
/****** Object:  Table [dbo].[records]    Script Date: 05/16/2011 17:40:48 ******/
ALTER TABLE [dbo].[records] DROP CONSTRAINT [FK_records_trials]
GO
DROP TABLE [dbo].[records]
GO
/****** Object:  Table [dbo].[siteAssignments]    Script Date: 05/16/2011 17:40:48 ******/
ALTER TABLE [dbo].[siteAssignments] DROP CONSTRAINT [FK_siteAssignments_trials]
GO
DROP TABLE [dbo].[siteAssignments]
GO
/****** Object:  Table [dbo].[physAssignments]    Script Date: 05/16/2011 17:40:47 ******/
ALTER TABLE [dbo].[physAssignments] DROP CONSTRAINT [FK_physAssignments_trials]
GO
DROP TABLE [dbo].[physAssignments]
GO
/****** Object:  Table [dbo].[physPatients]    Script Date: 05/16/2011 17:40:47 ******/
DROP TABLE [dbo].[physPatients]
GO
/****** Object:  Table [dbo].[trials]    Script Date: 05/16/2011 17:40:48 ******/
DROP TABLE [dbo].[trials]
GO
/****** Object:  User [dkal]    Script Date: 05/16/2011 17:40:48 ******/
DROP USER [dkal]
GO
USE [master]
GO
/****** Object:  Login [##MS_PolicyEventProcessingLogin##]    Script Date: 05/16/2011 17:40:48 ******/
DROP LOGIN [##MS_PolicyEventProcessingLogin##]
GO
/****** Object:  Login [##MS_PolicyTsqlExecutionLogin##]    Script Date: 05/16/2011 17:40:48 ******/
DROP LOGIN [##MS_PolicyTsqlExecutionLogin##]
GO
/****** Object:  Login [BUILTIN\Users]    Script Date: 05/16/2011 17:40:48 ******/
DROP LOGIN [BUILTIN\Users]
GO
/****** Object:  Login [dkal]    Script Date: 05/16/2011 17:40:48 ******/
DROP LOGIN [dkal]
GO
/****** Object:  Login [dkal]    Script Date: 05/16/2011 17:40:48 ******/
/* For security reasons the login is created disabled and with a random password. */
CREATE LOGIN [dkal] WITH PASSWORD='dkal', DEFAULT_DATABASE=[master], DEFAULT_LANGUAGE=[us_english], CHECK_EXPIRATION=OFF, CHECK_POLICY=OFF
GO

USE [ClinicalTrials]
GO
/****** Object:  User [dkal]    Script Date: 05/16/2011 17:40:48 ******/
CREATE USER [dkal] FOR LOGIN [dkal] WITH DEFAULT_SCHEMA=[dbo]
GO
/****** Object:  Table [dbo].[trials]    Script Date: 05/16/2011 17:40:48 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[trials](
	[id] [int] NOT NULL,
	[organizer] [varchar](50) NOT NULL,
	[name] [text] NOT NULL,
 CONSTRAINT [PK_trials] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
INSERT [dbo].[trials] ([id], [organizer], [name]) VALUES (42, N'org1', N'highly classified trial')
/****** Object:  Table [dbo].[physPatients]    Script Date: 05/16/2011 17:40:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[physPatients](
	[id] [int] NOT NULL,
	[needInfo] [bit] NOT NULL,
 CONSTRAINT [PK_physPatients] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
INSERT [dbo].[physPatients] ([id], [needInfo]) VALUES (1015, 1)
INSERT [dbo].[physPatients] ([id], [needInfo]) VALUES (1100, 1)
/****** Object:  Table [dbo].[physAssignments]    Script Date: 05/16/2011 17:40:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[physAssignments](
	[phys] [varchar](50) NOT NULL,
	[trial] [int] NOT NULL,
	[n1] [int] NOT NULL,
	[n2] [int] NOT NULL,
	[unnotified] [bit] NOT NULL,
 CONSTRAINT [PK_physAssignments] PRIMARY KEY CLUSTERED 
(
	[phys] ASC,
	[trial] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
INSERT [dbo].[physAssignments] ([phys], [trial], [n1], [n2], [unnotified]) VALUES (N'phys1', 42, 1010, 1050, 1)
/****** Object:  Table [dbo].[siteAssignments]    Script Date: 05/16/2011 17:40:48 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[siteAssignments](
	[site] [varchar](50) NOT NULL,
	[trial] [int] NOT NULL,
	[n1] [int] NOT NULL,
	[n2] [int] NOT NULL,
	[unnotified] [bit] NOT NULL,
 CONSTRAINT [PK_siteAssignments] PRIMARY KEY CLUSTERED 
(
	[site] ASC,
	[trial] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
INSERT [dbo].[siteAssignments] ([site], [trial], [n1], [n2], [unnotified]) VALUES (N'site1', 42, 1000, 1250, 0)
/****** Object:  Table [dbo].[records]    Script Date: 05/16/2011 17:40:48 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[records](
	[id] [int] NOT NULL,
	[patient] [int] NOT NULL,
	[trial] [int] NOT NULL,
	[data] [text] NOT NULL,
 CONSTRAINT [PK_Records] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
INSERT [dbo].[records] ([id], [patient], [trial], [data]) VALUES (101, 1005, 42, N'ala ma kota')
INSERT [dbo].[records] ([id], [patient], [trial], [data]) VALUES (102, 1015, 42, N'kot ma ale')
INSERT [dbo].[records] ([id], [patient], [trial], [data]) VALUES (103, 1016, 42, N'abecadlo')
INSERT [dbo].[records] ([id], [patient], [trial], [data]) VALUES (104, 1100, 42, N'z pieca spadlo')
/****** Object:  ForeignKey [FK_physAssignments_trials]    Script Date: 05/16/2011 17:40:47 ******/
ALTER TABLE [dbo].[physAssignments]  WITH CHECK ADD  CONSTRAINT [FK_physAssignments_trials] FOREIGN KEY([trial])
REFERENCES [dbo].[trials] ([id])
GO
ALTER TABLE [dbo].[physAssignments] CHECK CONSTRAINT [FK_physAssignments_trials]
GO
/****** Object:  ForeignKey [FK_records_trials]    Script Date: 05/16/2011 17:40:48 ******/
ALTER TABLE [dbo].[records]  WITH CHECK ADD  CONSTRAINT [FK_records_trials] FOREIGN KEY([trial])
REFERENCES [dbo].[trials] ([id])
GO
ALTER TABLE [dbo].[records] CHECK CONSTRAINT [FK_records_trials]
GO
/****** Object:  ForeignKey [FK_siteAssignments_trials]    Script Date: 05/16/2011 17:40:48 ******/
ALTER TABLE [dbo].[siteAssignments]  WITH CHECK ADD  CONSTRAINT [FK_siteAssignments_trials] FOREIGN KEY([trial])
REFERENCES [dbo].[trials] ([id])
GO
ALTER TABLE [dbo].[siteAssignments] CHECK CONSTRAINT [FK_siteAssignments_trials]
GO
