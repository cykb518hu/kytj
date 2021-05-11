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
    public interface IDataManageRepository
    {

        
        public List<RdDataColumn> SearchRdColumnList(int resultDataId, int pageIndex, int pageSize, ref int totalCount);
        public bool UpdateRdColumnName(RdDataColumn rdData);
        public bool CopyRd(int resultDataId);
        public bool UpdateRdName(int resultDataId, string Name);
        List<ResultData> GetRdAndSub(int resultDataId);

        DataTable GetOriginalDataFromMySql(string tableName);
        List<StatisticsMethodModel> GetStatisticsMethod();

        List<DictionaryDTO> GetDictionaryDTOs(string type);
        bool FillRdField(FillFieldRdColumnModel param);
    }

    public class DataManageRepository : KytjDbContext, IDataManageRepository
    {
        private readonly ILogger<DataManageRepository> _logger;
        private readonly ILogger<LegacyCodeHandler> _legacyCodeHandler;
        public DataManageRepository(ILogger<DataManageRepository> logger, ILogger<LegacyCodeHandler> legacyCodeHandler)
        {
            _logger = logger;
            _legacyCodeHandler = legacyCodeHandler;
        }
    
  
        public List<RdDataColumn> SearchRdColumnList(int resultDataId, int pageIndex, int pageSize, ref int totalCount)
        {
            var resultsTask = new List<RdDataColumn>();
            try
            {
                resultsTask = _dbKyStatic.Queryable<RdDataColumn>().Where(x => x.ResultDataId == resultDataId).ToPageList(pageIndex, pageSize, ref totalCount);
            }
            catch (Exception ex)
            {
                _logger.LogError($"查询数据所有列出错resultDataId: {resultDataId}：" + ex.ToString());
            }
            return resultsTask;
        }

        public bool UpdateRdColumnName(RdDataColumn rdData)
        {
            var result = false;
            try
            {
                result = _dbKyStatic.Updateable<RdDataColumn>()
                    .SetColumns(x => new RdDataColumn() { Rem = rdData.Rem})
                    .Where(x => x.Id == rdData.Id).ExecuteCommandHasChange();
            }
            catch (Exception ex)
            {
                _logger.LogError("编辑数据rdColumn失败：" + ex.ToString());
            }
            return result;
        }



        public bool CopyRd(int resultDataId)
        {
            var result = true;
            try
            {
                var newId = _dbKyStatic.Ado.GetInt(GetSqlText("DataManage-CopyData.sql"), new List<SugarParameter>(){
                   new SugarParameter("@RdResultId",resultDataId)
                });
                var oldRdColumnAndTags = _dbKyStatic.Queryable<RdDataColumn>()
                    .Mapper(it => it.GroupingTags, it => it.GroupingTags.First().DataColumnId)
                 .Where(x => x.ResultDataId == resultDataId).ToList();

                var newRdColumn= _dbKyStatic.Queryable<RdDataColumn>()
                 .Where(x => x.ResultDataId == newId).ToList();

                var newTags = new List<RdGroupingTag>();
                foreach (var r in oldRdColumnAndTags)
                {
                    var newRdCol = newRdColumn.FirstOrDefault(x => x.Name == r.Name);
                    if(newRdCol!=null)
                    {
                        var newRdColumnId = newRdCol.Id;
                        foreach(var d in r.GroupingTags)
                        {
                            d.DataColumnId = newRdColumnId;
                            d.Id = 0;
                            newTags.Add(d);
                        }
                    }
                   
                }
                _dbKyStatic.Insertable<RdGroupingTag>(newTags).ExecuteCommand();
            }
            catch (Exception ex)
            {
                result = false;
                _logger.LogError("复制数据CopyResultData失败：" + ex.ToString());
            }
            return result;
        }

        public bool UpdateRdName(int resultDataId,string name)
        {
            var result = false;
            try
            {
                result = _dbKyStatic.Updateable<ResultData>()
                    .SetColumns(x => new ResultData() { Name = name, UpdateDateTime =DateTime.Now})
                    .Where(x => x.Id == resultDataId).ExecuteCommandHasChange();
            }
            catch (Exception ex)
            {
                _logger.LogError("UpdateRdName失败：" + ex.ToString());
            }
            return result;
        }

        public List<ResultData> GetRdAndSub(int resultDataId)
        {
            var list = _dbKyStatic.Queryable<ResultData>().Mapper(it => it.DataColumns, it => it.DataColumns.First().ResultDataId)
                .Mapper((s, cache) =>
                {
                    foreach (var t in s.DataColumns)
                    {
                        t.GroupingTags = _dbKyStatic.Queryable<RdGroupingTag>().Where(o => o.DataColumnId == t.Id).ToList();
                    }
                }
                )
                .Where(x => x.Id == resultDataId).ToList();
            return list;
        }

        public DataTable GetOriginalDataFromMySql(string tableName)
        {
            var dt = new DataTable();
            try
            {
                 dt = _dbResearch.Ado.GetDataTable($"select * from dbo.[{tableName}]");

            }
            catch (Exception ex)
            {
                _logger.LogError("GetOriginalTable：" + ex.ToString());
            }
            return dt;
        }

        public List<StatisticsMethodModel> GetStatisticsMethod()
        {
            var result = new List<StatisticsMethodModel>();
            try
            {
                 result = _dbKyStatic.Queryable<StatisticsMethodModel>()
                    .Where(x=>!x.IsDeleted)
                    .OrderBy(x=>x.Code)
                    .Mapper(it => it.Kind, it => it.KindId)
                    .ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("GetStatisticsMethod" + ex.ToString());
            }
            return result;
        }

        public List<DictionaryDTO> GetDictionaryDTOs(string type)
        {
            var result = new List<DictionaryDTO>();
            try
            {
                 result = _dbKyStatic.Ado.SqlQuery<DictionaryDTO>(GetSqlText("DataFlow-GetDictionary.sql"), new List<SugarParameter>(){
                   new SugarParameter("@type",type)
                });
            }
            catch (Exception ex)
            {
                _logger.LogError("GetDictionaryDTOs失败：" + ex.ToString());
            }
            return result;
        }

        public bool FillRdField(FillFieldRdColumnModel param)
        {
            var result = true;
            try
            {
                LegacyCodeHandler legacyCodeHandler = new LegacyCodeHandler(_legacyCodeHandler);
                var rd = _dbKyStatic.Queryable<ResultData>().Where(x => x.Id == param.ResultDataId).ToList().FirstOrDefault();
                var tableName = rd.TableName;

                StringBuilder filter = new StringBuilder();
                filter.Append($" update dbo.[{tableName}] set [{param.ColumnName}] =@value where ");
                switch (param.Condition)
                {
                    case "null":
                        filter.Append($"[{param.ColumnName}] is null and ");
                        break;
                    case "blank":
                        filter.Append($"[{param.ColumnName}] ='' and ");
                        break;
                    case "blankOrNull":
                        filter.Append($"[{param.ColumnName}] is null or [{param.ColumnName}] =''  and ");
                        break;
                }
                filter.Append(" 1=1");

                var count = _dbResearch.Ado.ExecuteCommand(filter.ToString(), new List<SugarParameter>(){
                   new SugarParameter("@value",param.FieldValue) });
                if (count > 0)
                {
                    var dt = GetOriginalDataFromMySql(tableName);
                    legacyCodeHandler.GenerateColumnDataForSingleColumn(dt, param.ColumnName,param.RdDataColumnId);
                }
            }
            catch(Exception ex)
            {
                _logger.LogError("FillRdField：" + ex.ToString());
                result = false;
            }
            return result;
        }

    }
}
