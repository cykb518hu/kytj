delete from DF_Dataset where id=@dataSetId; 
delete from DF_TableConfig where id=@tableId
delete from RD_ResultData where id=@resultDataId