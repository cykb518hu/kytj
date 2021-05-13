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
        private readonly ILogRepository _logRepository;
        private readonly IDataManageRepository _dataManageRepository;
        private readonly ISSOUser _ssoUser;


        //rd  就是result data 
        public DataManageController(ILogger<DataManageController> logger, IDataManageRepository dataManageRepository, ISSOUser  sSOUser, ILogRepository logRepository)
        {
            _logger = logger;
            _dataManageRepository = dataManageRepository;
            _ssoUser = sSOUser;
            _logRepository = logRepository;
        }
        public IActionResult Index()
        {
            return View();
        }

        public IActionResult DataCheck()
        {
            _logRepository.Add("查看数据集检查页面");
            return View();
        }
        public JsonResult SearchRdColumnList(int resultDataId, int pageIndex = 1, int pageSize = 10)
        {
            try
            {

                var total = 0;
                var userName = _ssoUser.GetUserIdentity();
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
                _logRepository.Add("数据检查-修改字段名", $"column Id:{rdData.Id},rem:{rdData.Rem}");
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

        public JsonResult CopyRd(int resultDataId)
        {
            try
            {
                _logRepository.Add("画布-复制数据", $"rdResultId:{resultDataId}");
                var result = false;
                result = _dataManageRepository.CopyRd(resultDataId);
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

        public JsonResult UpdateRdName(int resultDataId, string Name)
        {
            try
            {
                _logRepository.Add("画布-数据集重命名", $"rdResultId:{resultDataId},name:{Name}");
                var result = false;
                result = _dataManageRepository.UpdateRdName(resultDataId,Name);
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

        public JsonResult FillRdField(FillFieldRdColumnModel param)
        {
            try
            {
                _logRepository.Add("数据检查-填充", $"rdResultId:{param.ResultDataId},column Id:{param.RdDataColumnId}");
                var result = false;
                result = _dataManageRepository.FillRdField(param);
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
