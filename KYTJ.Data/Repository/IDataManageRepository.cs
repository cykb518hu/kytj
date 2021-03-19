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

        public List<ProjectModel> GetProjectAndSub(string userName);
        public List<RdDataColumn> SearchRdColumnList(int resultDataId, int pageIndex, int pageSize, ref int totalCount);
    }

    public class DataManageRepository : KytjDbContext, IDataManageRepository
    {
        private readonly ILogger<DataManageRepository> _logger;
        public DataManageRepository(ILogger<DataManageRepository> logger)
        {
            _logger = logger;
            
        }
        public List<ProjectModel> GetProjectAndSub(string userName)
        {
            var resultsTask = new List<ProjectModel>();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask = query.Mapper(it => it.DataSetList, it => it.DataSetList.First().ProjectId)
                 .Where(x => x.UserName == userName && x.IsDeleted == 0)
                .Mapper((s, cache) =>
                {
                    foreach (var t in s.DataSetList)
                    {
                        t.ResultDataList = _dbKyStatic.Queryable<ResultData>().Where(o => o.DataSetId == t.DataSetId).ToList();
                    }
                }
                ).ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目及其数据失败：" + ex.ToString());
            }
            return resultsTask;
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


    }
}
