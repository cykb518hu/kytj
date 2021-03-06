begin tran tran_copyInfo --开始事务
declare @tran_error int;
Declare @NewRdResultId int;
set @tran_error=0;
begin try
    --DECLARE  @RdResultId int ;
	declare @DataSetId int;
	Declare @DataName nvarchar(100);
	declare @TableName nvarchar(100);
	declare @NewTableName nvarchar(100);
	declare @WdName nvarchar(100);
	set @NewTableName =CONVERT(NVARCHAR(50),newid());
	set @NewTableName=LOWER(REPLACE(@NewTableName,'-','')); 
	select @DataSetId=DataSet_Id,@DataName=Name, @TableName=TableName ,@WdName=WdName from RD_ResultData where id=@RdResultId
	
	declare @total int;
	select @total= count(*)  from RD_ResultData where Dataset_Id=@DataSetId
	if @total>0
	begin
		set @total =@total-1;
	end
	set @DataName=@DataName+'_copy.' +CONVERT(varchar(5), @total);
	--select @DataName,@TableName,@NewTableName;
	
	declare @sql nvarchar(max);
	
	set @sql ='select * into ResearchData..['+@NewTableName+'] from ResearchData..['+@TableName+']'
	exec sp_executesql @sql
	
	set @sql='insert into RD_ResultData select '''+ @DataName  +''' ,null,DataCount,getdate(),getdate(),Dataset_Id,WdName,'''+@NewTableName+''',IsManual 
	from RD_ResultData where id='+CAST(@RdResultId AS NVARCHAR(10))
	exec sp_executesql @sql   
	
	set @NewRdResultId = @@IDENTITY
	
	set @sql='insert into DF_TableConfig select ''ResearchData'', '''+ @NewTableName +''' , '''+ @DataName +''', '''+ @DataName +''','''+ @WdName +''','+ CAST(@DataSetId AS NVARCHAR(10)) 
	
	exec sp_executesql @sql  
	
	set @sql='insert into RD_DataColumn select name,Rem,CleansingMethod,IsContinuous,StatisticsInfo,KindCount,' +CAST(@NewRdResultId AS NVARCHAR(10)) +',IsGrouping,nullPercent from RD_DataColumn
	where ResultData_Id='+CAST(@RdResultId AS NVARCHAR(10))
	
	exec sp_executesql @sql  
	

end try
begin catch
 set @tran_error=@tran_error+1; --加分号或不加都能正常执行
end catch
if(@tran_error>0)
begin
 rollback tran tran_copyInfo; --执行出错，回滚事务(指定事务名称)
 print @tran_error;
end
else
begin
 commit tran tran_copyInfo; --没有异常，提交事务(指定事务名称)
 print @tran_error;
end

select ident_current('dbo.RD_ResultData') as resultDataId
