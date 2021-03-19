using KYTJ.Data.Context;
using KYTJ.Data.Handler;
using KYTJ.Model;
using Microsoft.Extensions.Logging;
using SqlSugar;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace KYTJ.Data.Repository
{
    public interface IDataSetRepository
    {
        public List<SearchEngineDataModel> SearchEngineDataList( string userName, string dataName, int pageIndex, int pageSize, ref int totalCount);

        public void ExtractEngineData(int exportDataId, int projectId, string userName);

        public List<DataSetModel> SearchDataSetList(string dataSetName, string userName, int pageIndex,int pageSize,ref int totalCount);

        bool DeleteDataSet(int dataSetId);
        bool EditDataSet(DataSetModel data);
        public void AddRdDataColumns();

    }

    public class DataSetRepository : KytjDbContext, IDataSetRepository
    {
        private readonly ILogger<DataSetRepository> _logger;
        public DataSetRepository(ILogger<DataSetRepository> logger)
        {
            _logger = logger;
            
        }

        public List<SearchEngineDataModel> SearchEngineDataList(string userName,string dataName, int pageIndex, int pageSize, ref int totalCount)
        {
            var result = new List<SearchEngineDataModel>();
            try
            {
                var query = _dbMySql.Queryable<SearchEngineDataModel>();
                result = query.Where(x => x.UserName == userName && x.IsDeleted == 0 && x.Status == 2 && x.DataName.Contains(dataName)).OrderBy("createTime desc").ToPageList(pageIndex, pageSize, ref totalCount);
                var idList = result.Select(x => x.Id).ToList();
                var mappingQuery = _dbKyStatic.Queryable<EngineMappingModel>().Where(x => idList.Contains(x.Id)).ToList();
                foreach (var r in result)
                {
                    var data = mappingQuery.FirstOrDefault(x => x.Id == r.Id && x.DataName == r.DataName);
                    if (data != null)
                    {
                        r.ExtractTime = data.LastUpdateTime.ToString("yyyy-MM-dd HH:mm:mm");
                    }
                }
            }
            catch (Exception ex)
            {
                _logger.LogError("查询搜索引擎数据失败：" + ex.ToString());
            }
            return result;
        }

        public void ExtractEngineData(int exportDataId, int projectId, string userName)
        {
            var extractDataSetMappingModel = new ExtractDataSetMappingModel();
            try
            {
                _logger.LogInformation($"开始抽取export_id:{exportDataId},projectId:{projectId},userName:{userName}");
                string tableName = Guid.NewGuid().ToString("N");

                var engineData = _dbMySql.Queryable<SearchEngineDataModel>().Where(x => x.Id == exportDataId).First();
                var extractDataListSql = string.Format(GetSqlText("ExtractEngineData-GetExportDataList.sql"), engineData.DataType, exportDataId);

                //获取原始数据
                DataTable dtSource = _dbMySql.Ado.GetDataTable(extractDataListSql);

                _logger.LogInformation("开始数据表格式处理");
                ExtractEngineDataHandler extractEngineDataHandler = new ExtractEngineDataHandler(_logger);
                //数据转成表格
                DataTable dtFormat = extractEngineDataHandler.ConvertToStandardTable(dtSource, engineData.DataType);
                dtFormat.TableName = tableName;

                _logger.LogInformation("开始在sql server创建mapping关系");
                //ms sql server 创建dataset table_config的mapping关系
                var createMappingSql = string.Format(GetSqlText("ExtractEngineData-CreateMapping.sql"), engineData.DiseaseId, engineData.DataName, engineData.Id, userName, tableName, projectId, engineData.DataType, dtFormat.Rows.Count);
                extractDataSetMappingModel = _dbKyStatic.Ado.SqlQuerySingle<ExtractDataSetMappingModel>(createMappingSql);
                _logger.LogInformation("开始导入数据");

                //创建数据表 将所有列插入到data_field 表
                extractEngineDataHandler.CreateSchemaAndExportData(extractDataSetMappingModel, dtFormat);
                _logger.LogInformation($"抽取结束datasetId:{extractDataSetMappingModel.DataSetId},tableId:{extractDataSetMappingModel.TableId}");


            }
            catch (Exception ex)
            {
                _logger.LogError("搜索引擎数据导入到sql server失败：" + ex.ToString());
                DeleteDataSet(extractDataSetMappingModel.DataSetId);
            }
        } 


        public List<DataSetModel> SearchDataSetList(string dataSetName, string userName,int pageIndex , int pageSize, ref int totalCount)
        {
            var result = new List<DataSetModel>();
            try
            {
                var sql = GetSqlText("DataSet-SearchDataSetList.sql");
                result = _dbKyStatic.Ado.SqlQuery<DataSetModel>(sql, new List<SugarParameter>(){
                   new SugarParameter("@userName",userName),
                   new SugarParameter("@dataSetName",dataSetName)
                }).ToList();
                totalCount = result.Count;
                result=result.Skip((pageIndex - 1) * pageSize).Take(pageSize).ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询数据集失败：" + ex.ToString());
            }
            return result;
        }

        public bool DeleteDataSet(int dataSetId)
        {
            var result = true;
            try
            {
                //有问题，需要用DF_TableConfig 数据来删除，
                var dbNameList = _dbKyStatic.Ado.SqlQuery<string>("select dbName from DF_TableConfig where DataSetId=" + dataSetId);
                foreach (var dbName in dbNameList)
                {
                    var dropTable = $"drop table dbo.[{dbName}]";
                    _dbResearch.Ado.ExecuteCommand(dropTable);
                }
                var sql = GetSqlText("DataSet-DeleteDataSet.sql");
                _dbKyStatic.Ado.ExecuteCommand(sql, new List<SugarParameter>(){
                   new SugarParameter("@dataSetId",dataSetId)
                });
            }
            catch (Exception ex)
            {
                result = false;
                _logger.LogError("删除数据集失败：" + ex.ToString());
            }
            return result;
        }

        public bool EditDataSet(DataSetModel data)
        {
            var result = false;
            try
            {
                result = _dbKyStatic.Updateable<DataSetModel>()
                    .SetColumns(x => new DataSetModel() { DataSetName = data.DataSetName, DataSetDesc = data.DataSetDesc, ProjectId=data.ProjectId, UpdateDateTime =DateTime.Now})
                    .Where(x => x.DataSetId == data.DataSetId).ExecuteCommandHasChange();
            }
            catch (Exception ex)
            {
                _logger.LogError("编辑数据集失败：" + ex.ToString());
            }
            return result;
        }


        public List<ResultData> GetRdDataColumns()
        {
            var list = _dbKyStatic.Queryable<ResultData>().Mapper(it => it.DataColumns, it => it.DataColumns.First().ResultDataId)
                .Mapper((s, cache) =>
                {
                    foreach(var t in s.DataColumns)
                    {
                        t.GroupingTags = _dbKyStatic.Queryable<RdGroupingTag>().Where(o => o.DataColumnId == t.Id).ToList();
                    }
                }
                )
                .Where(x => x.Id == 1178).ToList();
            return list;
        }

        public void AddRdDataColumns()
        {
                   var list = new List<RdDataColumn>();
            var cols = new RdDataColumn();
            cols.Name = "test";
            cols.Rem = "testrem";
            cols.StatisticsInfo = "[]";
            cols.KindCount = 1;
            cols.ResultDataId = 1178;
            cols.IsGrouping = true;

            cols.GroupingTags = new List<RdGroupingTag>();
            cols.GroupingTags.Add(new RdGroupingTag { Name = "1", Index = "1" });
            cols.GroupingTags.Add(new RdGroupingTag { Name = "2", Index = "2" });

     
            list.Add(cols);

            var cols2 = new RdDataColumn();
            cols2.Name = "test2";
            cols2.Rem = "testrem2";
            cols2.StatisticsInfo = "[]";
            cols2.KindCount = 1;
            cols2.ResultDataId = 1178;
            cols2.IsGrouping = true;
            cols2.GroupingTags = null;
            list.Add(cols2);
            //_dbKyStatic.Insertable(list)


            _dbKyStatic.Insertable(list).AddSubList(x => x.GroupingTags.First().DataColumnId)
                .ExecuteReturnPrimaryKey();

        }
    }
}
