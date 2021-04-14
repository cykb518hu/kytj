
using KYTJ.Business.Repository;
using KYTJ.Data.Repository;
using KYTJ.Model;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace KYTJ.Web.Controllers
{
    public class DataFlowController : Controller
    {

        private readonly IDataFlowRepository _dataFlowRepository;
        private readonly IDataManageRepository _dataManageRepository;
        public DataFlowController(IDataFlowRepository dataFlowRepository, IDataManageRepository dataManageRepository)
        {
            _dataFlowRepository = dataFlowRepository;
            _dataManageRepository = dataManageRepository;
        }

        public JsonResult SetDataFlowCache(int resultDataId, string node)
        {
            try
            {
                var msg = "设置成功";
                _dataFlowRepository.SetDataFlowCache(resultDataId, node);
                return Json(new { success = true, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
        public JsonResult GetDataFlowCache(string node, string prevNode)
        {
            try
            {
                var result = true;
                var msg = "";
                var data = _dataFlowRepository.GetDataFlowCache(node, prevNode);
                if (data == null)
                {
                    result = false;
                    msg = "请先设置数据源";
                }
                return Json(new { success = result, data = data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult GetFilterCols(string node, string filterType, int filterPercent)
        {
            try
            {
                var data = new List<DFDataColumn>();
                var result = false;
                var msg = "";
                if (string.IsNullOrEmpty(filterType))
                {
                    msg = "筛选条件为空";

                }
                else if (filterPercent==0)
                {
                    msg = "筛选预期不正确";
                }
                else
                {
                    data = _dataFlowRepository.GetFilterCols(node, filterType, filterPercent);
                    if (data.Any())
                    {
                        result = true;
                    }
                    else
                    {
                        msg = "没找到匹配的数据";
                    }
                }
                return Json(new { success = result, data = data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult RemoveFilterCols(string node, List<int> ids)
        {
            try
            {
                var result = false;
                result = _dataFlowRepository.RemoveFilterCols(node, ids);
                var msg = "";
                if (result)
                {
                    msg = "删除成功";
                }
                else
                {
                    msg = "删除失败";
                }

                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult GetStatisticsMethod()
        {
            try
            {
                var data = _dataManageRepository.GetStatisticsMethod();
                data.ForEach(x =>
                {
                    x.Name = x.Kind.Name + "-" + x.Name;
                });
                var msg = "";
                return Json(new { success = true, data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult Calculate(StatisticsMethodVM parameters)
        {
            try
            {
                var data = _dataFlowRepository.Calculate(parameters);
                if(data!=null&& !string.IsNullOrEmpty(data.OutputHTML))
                {
                    return Json(new { success = true, data });
                }
                else
                {
                    return Json(new { success = false, msg = "方法运行失败，请联系管理员！"});
                }
                
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
        public JsonResult TestStr(StatisticsMethodVM parameters)
        {
            try
            {
                var data = _dataFlowRepository.Calculate(parameters);
                //_dataFlowRepository.Calculate( node,  methodCode,  methodPath);
                //var list = new List<TestModel>();
                //var str = System.IO.File.ReadAllText(@"D:\Document\exceltest\test.txt");
                //var firstLevel = str.Split("],");
                //for(var i=0;i< firstLevel.Length;i++)
                //{
                //    var subLevel = firstLevel[i].Split("\":[");
                //    var name= subLevel[0].Replace("{", "").Replace("\"", "").Replace("}", "").Replace("]", "");

                //    var thirdLevel = subLevel[1].Replace("{", "").Replace("\"", "").Replace("}", "").Replace("]", "").Split(",");
                //    for (var j = 0; j < thirdLevel.Length; j++)
                //    {
                //        var data = new TestModel();
                //        data.Name = name;
                //        data.SubName = thirdLevel[j].Split(":")[0];
                //        data.SubValue = thirdLevel[j].Split(":")[1];
                //        list.Add(data);
                //    }

                //}

                return Json(new { success = true, data });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        

    }

    public class TestModel
    {
        public string Name { get; set; }
        public string SubName { get; set; }

        public string SubValue { get; set; }
    }
}
