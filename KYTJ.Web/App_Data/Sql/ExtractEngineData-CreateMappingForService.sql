begin tran tran_copyInfo --开始事务
declare @tran_error int;
set @tran_error=0;
begin try

	declare @diseaseId int;
	DECLARE @dataSetId int;
	
	

	MERGE dbo.SM_Diseases AS target
	USING(select @projectTypeStr as DiseaseTypeStr,@projectName as diseasename, @userName as username ) AS source
	ON(target.DiseaseTypeStr=source.DiseaseTypeStr and target.DiseaseName=source.diseasename and  target.username=source.username)
	when matched then UPDATE SET updatetime = getdate() WHEN NOT MATCHED THEN 
	INSERT(DeptID, DiseaseName, DiseaseTypeStr,UserName,createtime, IsDeleted,DiseaseType)
	VALUES(1, source.diseasename, source.DiseaseTypeStr,source.username,getdate(), 0,2); 
	
	select @diseaseId =id from SM_Diseases where DiseaseName=@projectName and username=@userName and DiseaseTypeStr=@projectTypeStr

	insert into dbo.DF_Dataset values('ResearchData',@tableName,@datasetName,1,getdate(),null,@diseaseId,null,2,null,@userName);
	
    select @dataSetId=ident_current('dbo.DF_Dataset');
	insert into DF_TableConfig values('ResearchData',@tableName,@datasetName,@datasetName,'',@dataSetId );

	insert into RD_ResultData values(@datasetName,null,@dataCount,getdate(),getdate(),@dataSetId,'',@tableName,null)

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

select ident_current('dbo.DF_TableConfig') as tableId , ident_current('dbo.DF_Dataset') as datasetId,ident_current('dbo.RD_ResultData') as resultDataId

