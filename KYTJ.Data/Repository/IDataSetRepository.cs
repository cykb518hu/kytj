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
        List<SearchEngineDataModel> SearchEngineDataList( string userName, string dataName, int pageIndex, int pageSize, ref int totalCount);

        void ExtractEngineData(int exportDataId, int projectId, string userName);

        List<DataSetModel> SearchDataSetList(string dataSetName, string userName, int pageIndex,int pageSize,ref int totalCount);

        bool DeleteDataSet(int dataSetId);
        bool UpdateDataSet(DataSetModel data);
 

        List<DataSetModel> GetDataSetAndSub(int projectId);

        DataSetModel GetDataSetById(int id);
        int ExtractEngineDataForService(string projectTypeStr, string projectName, string userName, DataTable dtFormat);

    }

    public class DataSetRepository : KytjDbContext, IDataSetRepository
    {
        private readonly ILogger<DataSetRepository> _logger;
        private readonly ILogger<LegacyCodeHandler> _extractEngineDataHandlerlogger;
        public DataSetRepository(ILogger<DataSetRepository> logger, ILogger<LegacyCodeHandler> extractEngineDataHandlerlogger)
        {
            _logger = logger;
            _extractEngineDataHandlerlogger = extractEngineDataHandlerlogger;
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
                LegacyCodeHandler extractEngineDataHandler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);
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

        public int ExtractEngineDataForService(string projectTypeStr, string projectName, string userName, DataTable dtFormat)
        {
            var extractDataSetMappingModel = new ExtractDataSetMappingModel();
            try
            {
                _logger.LogInformation($"projectTypeStr:{projectTypeStr},projectName:{projectName},userName:{userName}");
                string tableName = Guid.NewGuid().ToString("N");
                string datasetName = projectName + "_" + DateTime.Now.ToString("yyyyMMddhhmmssfff");

                _logger.LogInformation("开始在sql server创建mapping关系");
                //ms sql server 创建dataset table_config的mapping关系
                var createMappingSql = GetSqlText("ExtractEngineData-CreateMappingForService.sql");
                extractDataSetMappingModel = _dbKyStatic.Ado.SqlQuerySingle<ExtractDataSetMappingModel>(createMappingSql, new List<SugarParameter>(){
                   new SugarParameter("@projectTypeStr",projectTypeStr),
                   new SugarParameter("@projectName",projectName),
                   new SugarParameter("@userName",userName),
                   new SugarParameter("@tableName",tableName),
                   new SugarParameter("@datasetName",datasetName),
                   new SugarParameter("@dataCount",dtFormat.Rows.Count)
                });
                _logger.LogInformation("开始导入数据");

                LegacyCodeHandler extractEngineDataHandler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);
                //创建数据表 将所有列插入到data_field 表
                dtFormat.TableName = tableName;
                extractEngineDataHandler.CreateSchemaAndExportData(extractDataSetMappingModel, dtFormat);
                _logger.LogInformation($"抽取结束datasetId:{extractDataSetMappingModel.DataSetId},tableId:{extractDataSetMappingModel.TableId}");


            }
            catch (Exception ex)
            {
                _logger.LogError("接口导入到sql server失败：" + ex.ToString());
                DeleteDataSet(extractDataSetMappingModel.DataSetId);
                extractDataSetMappingModel.DataSetId = 0;
            }
            return extractDataSetMappingModel.DataSetId;
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
                var dbNameList = _dbKyStatic.Ado.SqlQuery<string>("select TableName from RD_ResultData where DataSet_Id=" + dataSetId);

                var sql = GetSqlText("DataSet-DeleteDataSet.sql");
                _dbKyStatic.Ado.ExecuteCommand(sql, new List<SugarParameter>(){
                   new SugarParameter("@dataSetId",dataSetId)
                });

                foreach (var dbName in dbNameList)
                {
                    var dropTable = $"drop table dbo.[{dbName}]";
                    _dbResearch.Ado.ExecuteCommand(dropTable);
                }
             
            }
            catch (Exception ex)
            {
                result = false;
                _logger.LogError("删除数据集失败：" + ex.ToString());
            }
            return result;
        }

        public bool UpdateDataSet(DataSetModel data)
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
        public List<DataSetModel> GetDataSetAndSub(int projectId)
        {
            var resultsTask = new List<DataSetModel>();
            try
            {
                var query = _dbKyStatic.Queryable<DataSetModel>();
                resultsTask = query.Where(x => x.ProjectId == projectId)
                    .Mapper(it => it.ResultDataList, it => it.ResultDataList.First().DataSetId)
                    .ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询GetDataSetAndSub失败：" + ex.ToString());
            }
            return resultsTask;
        }

        public DataSetModel GetDataSetById(int id)
        {
            var resultsTask = new DataSetModel();
            try
            {
                var query = _dbKyStatic.Queryable<DataSetModel>();
                resultsTask = query.Where(x => x.DataSetId == id)
                    .ToList().FirstOrDefault();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询GetDataSet失败：" + ex.ToString());
            }
            return resultsTask;
        }

    }
}
