 --kystaticManagement
 
alter table RD_DataColumn add  nullPercent nvarchar(10);
ALTER TABLE DF_TableConfig ALTER COLUMN TableNameCN nvarchar(200);
ALTER TABLE DF_TableConfig ALTER COLUMN TableNameSource nvarchar(200);