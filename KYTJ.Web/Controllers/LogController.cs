using KYTJ.Data.Repository;
using Microsoft.AspNetCore.Mvc;
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
        public LogController (ILogRepository logRepository)
        {
            _logRepository = logRepository;
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
                var role = "11admin";// HttpContext.User.Claims.FirstOrDefault(c => c.Type == ClaimTypes.Role).Value;
                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                if (startDate == endDate)
                {
                    endDate = endDate.AddDays(1);
                }
                var data = _logRepository.Search(startDate, endDate, role, userName, pageIndex, pageSize, ref total);
                //  var data = JsonConvert.SerializeObject(list, _jsonSetting);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                // _logger.LogError("查询操作日志错误：" + ex.ToString());
                return Json(new { success = false, msg = ex.ToString() });
            }
        }


    }

    public class OperationLogOutPut
    {

        /// <summary>
        /// 描述
        /// </summary>
        [JsonProperty("action")]
        public string Action { get; set; }


        /// <summary>
        /// 创建人
        /// </summary>
        /// 
        [JsonProperty("createdBy")]
        public string CreatedBy { get; set; }

        /// <summary>
        /// 时间
        /// </summary>
        [JsonProperty("createdOn")]
        public string CreatedOn { get; set; }
    }
}
