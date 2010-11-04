The "uni_connection_string.txt" SQL connection string for this example must point to a readable SQL DB that contains:

- A 'allowances' table with columns:

CREATE TABLE [dbo].[allowances](
	[destination] [nvarchar](max) NOT NULL,
	[position] [nvarchar](max) NOT NULL,
	[max] [smallint] NOT NULL,
 CONSTRAINT [IX_allowances] UNIQUE NONCLUSTERED 
(
	[max] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

- Intuitively, each row in this table gives the 'max' allowed amount to approve travel grants for 'destination' and for a particular job 'position'. If a particular ('destination','position') pair is not present, then it is equivalent to a 0 maximum allowance.

