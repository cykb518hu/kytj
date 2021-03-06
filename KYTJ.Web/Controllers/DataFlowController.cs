
using KYTJ.Business.Repository;
using KYTJ.Data.Repository;
using KYTJ.Infrastructure.Handler;
using KYTJ.Model;
using Microsoft.AspNetCore.Mvc;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

namespace KYTJ.Web.Controllers
{
    public class DataFlowController : Controller
    {
        private readonly ILogRepository _logRepository;
        private readonly IDataFlowRepository _dataFlowRepository;
        private readonly IDataManageRepository _dataManageRepository;
        public DataFlowController(IDataFlowRepository dataFlowRepository, IDataManageRepository dataManageRepository, ILogRepository logRepository)
        {
            _dataFlowRepository = dataFlowRepository;
            _dataManageRepository = dataManageRepository;
            _logRepository = logRepository;
        }

        public JsonResult SetDataFlowCache(int resultDataId, string node)
        {
            try
            {
                _logRepository.Add("画布-设置数据源", $"rdResultId:{resultDataId}");
                var msg = "设置成功";
                _dataFlowRepository.SetDataFlowCache(resultDataId, node);
                return Json(new { success = true, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
        public JsonResult GetDataFlowCache(string node, string prevNode, bool caculateInfo)
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
                else
                {
                    if (caculateInfo)
                    {
                        foreach(var r in data.DataColumns)
                        {
                            List<object> colStatis = new List<object>();
                            if (r.IsContinuous)
                            {
                                var list = (from dt in data.DataTable.AsEnumerable() select dt.Field<double>(r.Name)).ToList();
                                var min = list.Min();
                                var max = list.Max();
                                var area = (max - min) / 5;
                                for(int i = 0; i < 5; i++)
                                {
                                    var start = min + (i * area);
                                    var end = i == 4 ? max + 0.1 : min + ((i + 1) * area);
                                    var count = list.Count(x => x >= start && x < end);
                                    var value = Convert.ToInt32(start).ToString() + " - " + Convert.ToInt32(end).ToString();
                                    colStatis.Add(new { value = value, count = count});
                                }
                                r.NullPercent = JsonConvert.SerializeObject(colStatis);
                            }
                        }

                    }
                }
                var cloneData = data.Clone();
                cloneData.DataTable = null;
                return Json(new { success = result, data = cloneData, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DataFilterGetColumns(string node, string filterType, int filterPercent)
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
                    data = _dataFlowRepository.DataFilterGetColumns(node, filterType, filterPercent);
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

        public JsonResult DataFilterDeleteColumns(string node, List<int> ids)
        {
            try
            {
                _logRepository.Add("画布-过滤");
                var result = false;
                result = _dataFlowRepository.DataFilterDeleteColumns(node, ids);
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

        public JsonResult DataCalculateGetMethod()
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

        public JsonResult DataCalculateDo(StatisticsMethodVM parameters)
        {
            try
            {
                _logRepository.Add("画布-统计分析");
                var data = _dataFlowRepository.DataCalculate(parameters);
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

        public JsonResult GetDictionaryDTOs(string type)
        {
            try
            {
                var data = _dataManageRepository.GetDictionaryDTOs(type);
                return Json(new { success = true, data });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DataRowFilterByColumns(string node, List<DataRowFilter> filterColumns)
        {
            try
            {
                _logRepository.Add("画布-行处理");
                var result = 0;
                result = _dataFlowRepository.DataRowFilterByColumns(node, filterColumns);
                return Json(new { success = true, data = result });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DataSampleExtractSimple(string node, DataSampleModel simObj)
        {
            try
            {
                _logRepository.Add("画布-样本管理-简单抽样");
                var result = true;
                var msg = "提取成功";
                result = _dataFlowRepository.DataSampleExtractSimple(node, simObj);
                if (!result)
                {
                    msg = "提取失败";
                }
                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DataCombineGetSource(List<string> prevNodeIds)
        {
            try
            {
                var result = true;
                var data = new List<DataCombineSourceModel>();
                var msg = "";
                var mainData = _dataFlowRepository.GetDataFlowCache(prevNodeIds[0], "");
                var secondData = _dataFlowRepository.GetDataFlowCache(prevNodeIds[1], "");
                if (mainData == null|| secondData == null)
                {
                    result = false;
                    msg = "某前置节点没有数据";
                }
                else
                {
                    var mainColumns = mainData.DataColumns.Select(x => x.Name).ToList();
                    var secondColumns = secondData.DataColumns.Select(x => x.Name).ToList();
                    var diffColumns = secondColumns.Except(mainColumns).ToList();
                    if (diffColumns.Count > 5)
                    {
                        //超过五个不一样的列，认为不是两个结构相同的数据表
                        result = false;
                        msg = "选择的两个数据集可能不是一个表";
                    }
                    else
                    {
                        foreach (var r in mainColumns)
                        {
                            var secondColumn = "";
                            if (secondData.DataColumns.Any(x => x.Name == r))
                            {
                                secondColumn = r;
                            }
                            data.Add(new DataCombineSourceModel { OutColumn = r, MainColumn = r, SecondColumn = secondColumn });
                        }
                        if (diffColumns.Count > 0)
                        {
                            foreach (var r in diffColumns)
                            {
                                data.Add(new DataCombineSourceModel { OutColumn = r, MainColumn = "", SecondColumn = r });
                            }
                        }
                    }
                   
                }
             
                return Json(new { success = result, data = data, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DataCombineAppend(string node,List<string> prevNodeIds,string fieldSource)
        {
            try
            {
                _logRepository.Add("画布-数据整合-合并");
                var result = true;
                var msg = "合并成功";
                result = _dataFlowRepository.DataCombineAppend(node, prevNodeIds, fieldSource);
                if (!result)
                {
                    msg = "合并失败";
                }

                return Json(new { success = result,  msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DataColumnFillField(string node, string fieldName, string condition,string filedValue)
        {
            try
            {
                _logRepository.Add("画布-裂处理-填充");
                var result = true;
                var msg = "填充成功";
                result = _dataFlowRepository.DataColumnFillField(node, fieldName, condition, filedValue);
                if (!result)
                {
                    msg = "填充失败";
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
