delete from DF_Field where Dataset_Id =@dataSetId;
delete from DF_TableConfig where DataSetId=@dataSetId;
 
delete from RD_DataResultFilter where DataColumn_Id in (select id from RD_DataColumn 
where ResultData_Id in (select id from RD_ResultData where Dataset_Id=@dataSetId) );

delete from RD_GroupingTag where DataColumn_Id in (select id from RD_DataColumn
where ResultData_Id in (select id from RD_ResultData where Dataset_Id=@dataSetId) );

delete  from RD_DataColumn
where ResultData_Id in (select id from RD_ResultData where Dataset_Id=@dataSetId);

delete  from  RD_ResultData where Dataset_Id=@dataSetId;
delete  from  DF_Dataset where id=@dataSetId;