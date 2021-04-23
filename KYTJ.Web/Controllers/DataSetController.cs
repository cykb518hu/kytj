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
        private readonly ILogRepository _logRepository;
        public DataSetController(ILogger<DataSetController> logger, IDataSetRepository dataSetRepository, ILogRepository logRepository)
        {
            _logger = logger;
            _dataSetRepository = dataSetRepository;
            _logRepository = logRepository;
        }

        public IActionResult Index()
        {
            return View();
        }
        public IActionResult SearchEngine()
        {
            _logRepository.Add("查看数据集抽取页面");
            return View();
        }
        public ActionResult DatasetList()
        {
            _logRepository.Add("查看数据集管理页面");
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
                var userName = SSOUser.GetUserName();// HttpContext.User.Identity.Name;
                var data = _dataSetRepository.SearchEngineDataList(userName, dataName, pageIndex, pageSize, ref total);
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
                _logRepository.Add("抽取数据", "", "", "", $"抽取数据源ID:{exportDataId},项目id:{projectId}");
                Task t = Task.Run(() =>
                {
                    var userName = SSOUser.GetUserName();// HttpContext.User.Identity.Name;
                    _dataSetRepository.ExtractEngineData(exportDataId, projectId, userName);
                });
               
                var data = true;
                var msg = "任务已提交,请稍后到数据集管理查看状态";
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
                var userName = SSOUser.GetUserName();// HttpContext.User.Identity.Name;
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
                _logRepository.Add("删除数据集", "", "", "", $"数据集DataSetId:{dataSetId}");
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
                _logRepository.Add("修改数据集", "", "", "", $"数据集DataSetId:{data.DataSetId}");
                var result = false;
                var userName = SSOUser.GetUserName();

                result = _dataSetRepository.UpdateDataSet(data);

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

    }
}
