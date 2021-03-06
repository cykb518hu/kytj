MERGE dbo.T_EngineMapping AS target
USING(select {0} disease_id,'{1}' data_name ) AS source ON(target.disease_id=source.disease_id and target.data_name=source.data_name)
when matched then UPDATE SET lastupdatetime = getdate() WHEN NOT MATCHED THEN 
INSERT(export_id, disease_id, data_name,createUserCode, createdate,lastupdatetime)
VALUES({2}, source.disease_id, source.data_name,'{3}',getdate(),getdate());

declare @total int;
select @total= count(*)  from DF_Dataset where DisplayName like '%{1}%'
if @total>0
begin
	set @total =@total +1;
end
DECLARE @dataName nvarchar(200);
set @dataName='{1}'+'_' +CONVERT(varchar(5), @total);


insert into dbo.DF_Dataset values('ResearchData','{4}',@dataName,1,getdate(),
null,{5},null,1,null,'{3}');
DECLARE @dataSetId int;
select @dataSetId=ident_current('dbo.DF_Dataset');

insert into DF_TableConfig values('ResearchData','{4}',@dataName,@dataName,'{6}',@dataSetId );

insert into RD_ResultData values(@dataName,null,{7},getdate(),getdate(),@dataSetId,'{6}','{4}',null)

select ident_current('dbo.DF_TableConfig') as tableId , ident_current('dbo.DF_Dataset') as datasetId,ident_current('dbo.RD_ResultData') as resultDataId

