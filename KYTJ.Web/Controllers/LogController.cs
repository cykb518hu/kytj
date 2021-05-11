using KYTJ.Data.Repository;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;
using SSO.Client;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using System.Threading.Tasks;

namespace KYTJ.Web.Controllers
{
    public class LogController : KYTJControllerBase
    {
        private readonly ILogRepository _logRepository;

        private readonly ILogger<LogController> _logger;
        private readonly ISSOUser _ssoUser;
        public LogController (ILogRepository logRepository, ILogger<LogController> logger, ISSOUser sSOUser)
        {
            _logRepository = logRepository;
            _logger = logger;
            _ssoUser = sSOUser;
        }
        public IActionResult Index()
        {
            _logRepository.Add("查看日志");
            return View();
        }

        public JsonResult SearchLogList(List<string> logDateRange, int pageIndex = 1, int pageSize = 10)
        {
            try
            {
                var total = 0;
                var endDate = DateTime.Now;
                var startDate = endDate.AddDays(-30);
                if (logDateRange != null && logDateRange.Count > 1)
                {
                    DateTime.TryParse(logDateRange[0].Substring(0, 24), out startDate);
                    DateTime.TryParse(logDateRange[1].Substring(0, 24), out endDate);
                }
                var role = "none"; 
                var userName = _ssoUser.GetUserIdentity();

                if(userName.ToLower()=="sys")
                {
                    role = "admin";// to do, this haven't done yet
                }
                if (startDate == endDate)
                {
                    endDate = endDate.AddDays(1);
                }
                var data = _logRepository.Search(startDate, endDate, role, userName, pageIndex, pageSize, ref total);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                 _logger.LogError("查询操作日志错误：" + ex.ToString());
                return Json(new { success = false, msg = ex.ToString() });
            }
        }


    }
}
