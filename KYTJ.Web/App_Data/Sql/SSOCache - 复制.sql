USE [SSOCache]
GO
/****** Object:  Table [dbo].[LoginLog]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[LoginLog](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[UserId] [nvarchar](50) NULL CONSTRAINT [DF_LoginLog_UserId]  DEFAULT (''),
	[UserName] [nvarchar](50) NULL CONSTRAINT [DF_LoginLog_UserName]  DEFAULT (''),
	[LoginState] [int] NULL CONSTRAINT [DF_LoginLog_LoginState]  DEFAULT ((1)),
	[LoginTime] [datetime] NULL CONSTRAINT [DF_LoginLog_LoginTime]  DEFAULT (getdate()),
 CONSTRAINT [PK_LoginLog] PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[Token]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[Token](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[TokenKey] [varchar](50) NOT NULL,
	[UserID] [nvarchar](50) NULL,
	[UserName] [nvarchar](50) NULL,
	[LastActiveTime] [datetime] NULL,
 CONSTRAINT [PK_Token] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  StoredProcedure [dbo].[usp_AddLoginLog]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[usp_AddLoginLog]
	@UserId varchar(50),@UserName varchar(50),@LoginState int,@LoginTime datetime
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	
     insert into [dbo].[LoginLog]([UserId],[UserName],[LoginState],[LoginTime])
	 values (@UserId,@UserName,@LoginState,@LoginTime)
END


GO
/****** Object:  StoredProcedure [dbo].[usp_AddToken]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[usp_AddToken]
	@TokenKey varchar(50),@UserID varchar(50),@UserName nvarchar(50)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	
     insert into [dbo].[Token]([TokenKey],[UserID],[UserName],[LastActiveTime])
	 values (@TokenKey,@UserID,@UserName,GETDATE())
END

GO
/****** Object:  StoredProcedure [dbo].[usp_ClearAllToken]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[usp_ClearAllToken]
as
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	 SET NOCOUNT ON;	
     delete from [dbo].[Token] 
	 where datediff(hh,lastactivetime,getdate())>=24*30 
END




GO
/****** Object:  StoredProcedure [dbo].[usp_DeleteToken]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[usp_DeleteToken]
	@TokenKey varchar(50)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	 SET NOCOUNT ON;	
     delete from [dbo].[Token] where [TokenKey]=@TokenKey
END




GO
/****** Object:  StoredProcedure [dbo].[usp_GetLogin]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[usp_GetLogin]
	-- Add the parameters for the stored procedure here
	@BeginTime datetime,
	@EndTime datetime
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
	SELECT UserId as 用户编号 ,count(UserId) as 登录次数
	from LoginLog
	where LoginTime >= @BeginTime and LoginTime<=@EndTime
	group by UserId
	order by count(UserId) desc
END


GO
/****** Object:  StoredProcedure [dbo].[usp_GetLoginLog]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[usp_GetLoginLog]
	@UserId varchar(50)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	
     select top(5) * from [dbo].[LoginLog] 
	 where UserId=@UserId and DateDiff(dd,LoginTime,GETDATE())=0 
	 order by LoginTime desc
END

GO
/****** Object:  StoredProcedure [dbo].[usp_GetLogins]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[usp_GetLogins] 
	@UserId varchar(50),
	@UserName varchar(50),
	@LoginState int=0,
	@BeginTime datetime,
	@EndTime datetime
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
    -- Insert statements for procedure here
    select UserId as 用户编号,UserName as 用户名称,LoginTime as 登陆时间,
      (case LoginState
        when 0 then '登陆失败'
        when 1 then '正常登陆'
        else ''
        end
       )登陆状态
    from LoginLog where UserId=@UserId and UserName=@UserName and LoginState=@LoginState and LoginTime between @BeginTime and @EndTime
END

GO
/****** Object:  StoredProcedure [dbo].[usp_GetToken]    Script Date: 2020/6/28 11:16:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[usp_GetToken]
	@TokenKey varchar(50)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;
	
     select * from [dbo].[Token] where [TokenKey]=@TokenKey
END

GO
