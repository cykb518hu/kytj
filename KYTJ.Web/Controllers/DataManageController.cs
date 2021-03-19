using KYTJ.Data.Repository;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using SSO.Client;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace KYTJ.Web.Controllers
{
    public class DataManageController : KYTJControllerBase
    {
        private readonly ILogger<DataManageController> _logger;

        private readonly IDataManageRepository _dataManageRepository;
        public DataManageController(ILogger<DataManageController> logger, IDataManageRepository dataManageRepository)
        {
            _logger = logger;
            _dataManageRepository = dataManageRepository;
        }
        public IActionResult Index()
        {
            return View();
        }

        public IActionResult DataCheck()
        {
            return View();
        }

        public JsonResult SearchProjectAndSub()
        {
            try
            {
                var total = 0;

                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                var data = _dataManageRepository.GetProjectAndSub(userName);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult SearchRdColumnList(int resultDataId, int pageIndex = 1, int pageSize = 10)
        {
            try
            {

                var total = 0;
                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                var data = _dataManageRepository.SearchRdColumnList(resultDataId, pageIndex, pageSize, ref total);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
    }
}
