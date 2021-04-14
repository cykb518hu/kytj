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
        public bool CopyResultData(int resultDataId);
        public bool UpdateResultDataName(int resultDataId, string Name);
        List<ResultData> GetRdDataAndSub(int resultDataId);

        DataTable GetOriginalTable(string tableName);
        List<StatisticsMethodModel> GetStatisticsMethod();
    }

    public class DataManageRepository : KytjDbContext, IDataManageRepository
    {
        private readonly ILogger<DataManageRepository> _logger;
        public DataManageRepository(ILogger<DataManageRepository> logger)
        {
            _logger = logger;
            
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



        public bool CopyResultData(int resultDataId)
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

        public bool UpdateResultDataName(int resultDataId,string name)
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
                _logger.LogError("UpdateResultDataName失败：" + ex.ToString());
            }
            return result;
        }

        public List<ResultData> GetRdDataAndSub(int resultDataId)
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

        public DataTable GetOriginalTable(string tableName)
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

    }
}
