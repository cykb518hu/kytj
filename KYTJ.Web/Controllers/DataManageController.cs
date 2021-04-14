using KYTJ.Data.Repository;
using KYTJ.Model;
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

        public JsonResult UpdateRdColumnName(RdDataColumn rdData)
        {
            try
            {
                var result = false;
                result = _dataManageRepository.UpdateRdColumnName(rdData);
                var msg = "操作成功";
                if (!result)
                {
                    msg = "操作失败";
                }
                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult CopyResultData(int resultDataId)
        {
            try
            {
                var result = false;
                result = _dataManageRepository.CopyResultData(resultDataId);
                var msg = "操作成功";
                if (!result)
                {
                    msg = "操作失败";
                }
                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult UpdateResultDataName(int resultDataId, string Name)
        {
            try
            {
                var result = false;
                result = _dataManageRepository.UpdateResultDataName(resultDataId,Name);
                var msg = "操作成功";
                if (!result)
                {
                    msg = "操作失败";
                }
                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
        public JsonResult Test(int resultDataId,string name)
        {
            try
            {
                var result = false;
               // result = _dataManageRepository.UpdateResultDataName(resultDataId,name);
                var msg = "操作成功";
                if (!result)
                {
                    msg = "操作失败";
                }
                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
    }
}
