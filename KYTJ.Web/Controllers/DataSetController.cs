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
    public class DataSetController : KYTJControllerBase
    {
        private readonly ILogger<DataSetController> _logger;
        private readonly IDataSetRepository _dataSetRepository;
        public DataSetController(ILogger<DataSetController> logger, IDataSetRepository dataSetRepository)
        {
            _logger = logger;
            _dataSetRepository = dataSetRepository;
        }

        public IActionResult Index()
        {
            return View();
        }
        public IActionResult SearchEngine()
        {
            return View();
        }
        public ActionResult DatasetList()
        {

            return View();
        }

        public JsonResult SearchEngineDataList(string dataName,int pageIndex = 1, int pageSize =10)
        {
            try
            {
                if(string.IsNullOrEmpty(dataName))
                {
                    dataName = "";
                }
                var total = 0;
                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                var data = _dataSetRepository.SearchEngineDataList( userName, dataName, pageIndex, pageSize, ref total);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult ExtractSearchEngineData(int exportDataId , int projectId)
        {
            try
            {
                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                _dataSetRepository.ExtractEngineData(exportDataId, projectId,userName);
                var data = true;
                var msg = "正在执行,请到数据集管理查看状态";
                if (!data)
                {
                    msg = "删除失败";
                }
                return Json(new { success = data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }


        public ActionResult SearchDataSetList(string dataSetName, int pageIndex = 1, int pageSize = 10)
        {
            try
            {
                if (string.IsNullOrEmpty(dataSetName))
                {
                    dataSetName = "";
                }
                var total = 0;
                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                var data = _dataSetRepository.SearchDataSetList(dataSetName, userName, pageIndex, pageSize,ref total );
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public ActionResult DeleteDataSet(int dataSetId )
        {
            try
            {

                var data = _dataSetRepository.DeleteDataSet(dataSetId);
                var msg = "删除成功";
                if (!data)
                {
                    msg = "删除失败";
                }
                return Json(new { success = data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult MaintainDataSet(DataSetModel data)
        {
            try
            {
                var result = false;
                var userName = SSOUser.GetUser();

                result = _dataSetRepository.EditDataSet(data);

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

        public JsonResult GetDataSetAndSub(int projectId)
        {
            try
            {
                var data = _dataSetRepository.GetDataSetAndSub(projectId);
                return Json(new { success=true, data = data});
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult Test(int projectId)
        {
            try
            {
                var msg = "ss";
                //var result = true;
                //Int32 nullCount = 12;
                //int dataCount = 17;
                //var NullPercent = ((double)nullCount / dataCount).ToString("P");
                ////_dataSetRepository.AddRdDataColumns();
                ///

                var data = _dataSetRepository.GetDataSetAndSub(projectId);
                return Json(new { success = true, data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
    }
}
