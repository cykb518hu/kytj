using KYTJ.Data.Context;
using KYTJ.Model;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace KYTJ.Data.Repository
{
    public interface ILogRepository
    {
        void Add(string action, string projectName="", string sourceName="", string userName = "", string message = "");
        List<LogModel> Search(DateTime startDate, DateTime endDate, string role, string userName, int pageIndex, int pageSize, ref int totalCount);
    }


    public class LogRepository : KytjDbContext, ILogRepository
    {
        private readonly ILogger<LogRepository> _logger;
        private readonly IHttpContextAccessor _httpContextAccessor;
        public LogRepository(ILogger<LogRepository> logger, IHttpContextAccessor httpContextAccessor)
        {
            _logger = logger;
            _httpContextAccessor = httpContextAccessor;
        }

        public void Add(string action, string projectName = "", string sourceName = "", string userName = "", string message = "")
        {
            try
            {
                LogModel actionLog = new LogModel();
                actionLog.DtDate = DateTime.Now;
                actionLog.Level = "INFO";
                actionLog.Account = userName;
                actionLog.Message = message;
                actionLog.ClientIp = _httpContextAccessor.HttpContext.Connection.RemoteIpAddress.ToString();
                actionLog.ProjectName = projectName;
                actionLog.SourceName = sourceName;
                actionLog.Action = action;
                actionLog.Thread = Thread.CurrentThread.ManagedThreadId.ToString("00");
                int count = _dbKyStatic.Insertable(actionLog).ExecuteCommand();
            }
            catch (Exception ex)
            {
                _logger.LogError("记录操作日志失败：" + ex.ToString());
            }
        }

        public List<LogModel> Search(DateTime startDate, DateTime endDate, string role, string userName, int pageIndex, int pageSize, ref int totalCount)
        {
            var resultsTask = new List<LogModel>();
            try
            {
                var query = _dbKyStatic.Queryable<LogModel>();
                if (role == "admin")
                {
                    resultsTask = query.Where(x => x.DtDate >= startDate && x.DtDate < endDate).OrderBy("DtDate desc").ToPageList(pageIndex, pageSize, ref totalCount);
                }
                else
                {
                    resultsTask = query.Where(x => x.DtDate >= startDate && x.DtDate < endDate && x.Account == userName).OrderBy("DtDate desc").ToPageList(pageIndex, pageSize, ref totalCount);
                }
            }
            catch (Exception ex)
            {
                _logger.LogError("查询日志失败：" + ex.ToString());
            }
            return resultsTask;
        }
    }

}
