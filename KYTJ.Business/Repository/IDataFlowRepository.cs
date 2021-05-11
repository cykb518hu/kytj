using KYTJ.Data.Handler;
using KYTJ.Data.Repository;
using KYTJ.Infrastructure.Handler;
using KYTJ.Infrastructure.Model;
using KYTJ.Model;
using KYTJ.RMethod;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;

namespace KYTJ.Business.Repository
{
    public interface IDataFlowRepository
    {
        void SetDataFlowCache(int resultDataId, string node);
        DataFlowCacheModel GetDataFlowCache(string node, string prevNode = "");

        List<DFDataColumn> GetDataFilterColumns(string node, string filterType, int filterPercent);

        bool DeleteDataFilterColumns(string node, List<int> ids);
        StatisticsMethodResult Calculate(StatisticsMethodVM parameters);
        int DataRowFilterByColumns(string node, List<DataRowFilter> filterColumns);
        bool DataSampleExtractSimple(string node, DataSampleModel simObj);
        bool DataCombineAppend(string node, List<string> prevNodeIds, string fieldSource);

        bool DataColumnFillField(string node, string fieldName, string condition, string filedValue);
    }

    public class DataFlowRepository: IDataFlowRepository
    {
        private readonly IDataManageRepository _dataManageRepository;
        private readonly IDataSetRepository _dataSetRepository;
        private readonly IProjectRepository _projectRepository;
        private readonly ILogger<DataFlowRepository> _logger;
        private readonly ICacheHandler _cacheHandler;
        private readonly IHostingEnvironment _env;
        private readonly ILogger<StatisticsMethod> _staticlogger;
        private readonly ILogger<LegacyCodeHandler> _extractEngineDataHandlerlogger;

        public DataFlowRepository(IDataManageRepository dataManageRepository, IDataSetRepository dataSetRepository, IProjectRepository projectRepository,ILogger<DataFlowRepository> logger, ICacheHandler cacheHandler, IHostingEnvironment env, ILogger<StatisticsMethod> staticlogger, ILogger<LegacyCodeHandler> extractEngineDataHandlerlogger)
        {
            _dataManageRepository = dataManageRepository;
            _dataSetRepository = dataSetRepository;
            _projectRepository = projectRepository;
            _logger = logger;
            _cacheHandler = cacheHandler;
            _env = env;
            _staticlogger = staticlogger;
            _extractEngineDataHandlerlogger = extractEngineDataHandlerlogger;
        }

        public DataFlowCacheModel GetDataFlowCache(string node, string prevNode="")
        {
            var data = _cacheHandler.Get<DataFlowCacheModel>(node);
            if (data == null && !string.IsNullOrEmpty(prevNode))
            {
                data = _cacheHandler.Get<DataFlowCacheModel>(prevNode);
                var obj = data.Clone();
                SetDataFlowCache(obj, node);
            }
            return data;
        }
        public void SetDataFlowCache(int resultDataId, string node)
        {

            var data = GetDataFlowCacheFromDb(resultDataId);
            SetDataFlowCache(data, node);
        }

        public void SetDataFlowCache(DataFlowCacheModel data,string node)
        {
            try
            {
                _cacheHandler.Set(node, data);
            }
            catch (Exception ex)
            {
                _logger.LogError("SetDataFlowCache失败：" + ex.ToString());
                throw ex;
            }
           
        }
        public DataFlowCacheModel GetDataFlowCacheFromDb(int resultDataId)
        {
            
            DataFlowCacheModel data = new DataFlowCacheModel();
            try
            {
                var dataSetId = 0;
                var projectId = 0;
                var rd = _dataManageRepository.GetRdAndSub(resultDataId);

                if (rd.Any())
                {
                    dataSetId = rd.FirstOrDefault().DataSetId;
                    data.Id = rd.FirstOrDefault().Id;
                    data.Name = rd.FirstOrDefault().Name;
                    data.DataCount = rd.FirstOrDefault().DataCount;
                    data.TableName = rd.FirstOrDefault().TableName;
                    data.WdName = rd.FirstOrDefault().WdName;
                    data.DataColumns = new List<DFDataColumn>();

                    rd.FirstOrDefault().DataColumns.ForEach(col =>
                    {
                        DFDataColumn colItem = new DFDataColumn
                        {
                            Id = col.Id,
                            Name = col.Name,
                            Rem=col.Rem,
                            IsContinuous = col.IsContinuous,
                            StatisticsInfo = col.StatisticsInfo,
                            KindCount = col.KindCount,
                            NullPercent = col.NullPercent,
                            GroupingTags = new List<DFGroupingTag>()
                        };
                        col.GroupingTags.ForEach(tag =>
                        {
                            DFGroupingTag tagItem = new DFGroupingTag
                            {
                                Id = tag.Id,
                                Name = tag.Name,
                                Index = tag.Index
                            };
                            colItem.GroupingTags.Add(tagItem);
                        });
                        data.DataColumns.Add(colItem);

                    });

                    var dataSet = _dataSetRepository.GetDataSetById(dataSetId);
                    projectId = dataSet.ProjectId;
                    data.DataSetInfo = new DFDataSet
                    {
                        Id = dataSet.DataSetId,
                        Name = dataSet.DataSetName
                    };
                    var project = _projectRepository.GetProjectById(projectId);
                    data.ProjectInfo = new DFProject
                    {
                        Id = project.Id,
                        Name = project.ProjectName
                    };
                    data.DataTable = _dataManageRepository.GetOriginalDataFromMySql(data.TableName);
                }
            }
            catch(Exception ex)
            {
                _logger.LogError("获取缓存初始化数据失败：" + ex.ToString());
            }
            return data;
        }

        public List<DFDataColumn> GetDataFilterColumns(string node,string filterType,int filterPercent)
        {
            var cols = new List<DFDataColumn>();
            try
            {
                var cacheData = GetDataFlowCache(node);
                var count = cacheData.DataCount;
                var dt = cacheData.DataTable;
                var handler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);
                switch (filterType)
                {
                    case "unique":
                        foreach (System.Data.DataColumn item in dt.Columns)
                        {
                            bool s = handler.ConvertTypeToSqlDbType(item.DataType, count, dt, item.ColumnName, filterPercent);
                            if (s)
                                cols.Add(cacheData.DataColumns.FirstOrDefault(x => x.Name == item.ColumnName));
                        }
                        break;
                    case "single":
                        DataView dataView = dt.DefaultView;
                        foreach (System.Data.DataColumn item in dt.Columns)
                        {
                            int toal = dataView.ToTable(true, $"{item.ColumnName}").Rows.Count;
                            if (toal == 0)
                                continue;
                            bool s = (float)toal / count >= (float)filterPercent / 100;
                            if (s)
                                cols.Add(cacheData.DataColumns.FirstOrDefault(x => x.Name == item.ColumnName));
                        }
                        //DataTable dataTableDistinct = dataView.ToTable(true, "FieldName1", "FieldName2", "...");
                        break;
                    default:
                        foreach (System.Data.DataColumn item in dt.Columns)
                        {
                            int toal = dt.Select($"[{item.ColumnName}] is null ").Count();
                            if (toal == 0)
                                continue;
                            bool s = (float)toal / count >= (float)filterPercent / 100;
                            if (s)
                                cols.Add(cacheData.DataColumns.FirstOrDefault(x => x.Name == item.ColumnName));
                        }
                        break;
                }
            }
            catch(Exception ex)
            {
                _logger.LogError("DataFilter失败：" + ex.ToString());
            }
            return cols;
        }

        public bool DeleteDataFilterColumns(string node, List<int> ids)
        {
            var result = true;
            try
            {
                var cacheData = GetDataFlowCache(node);

                foreach (var id in ids)
                {
                    var column = cacheData.DataColumns.FirstOrDefault(x => x.Id == id);
                    var columnName = column.Name;
                    if (column != null)
                    {
                        cacheData.DataTable.Columns.Remove(columnName);
                        cacheData.DataColumns.Remove(column);
                    }
                    cacheData.DataCount--;
                }
            }
            catch (Exception ex)
            {
                _logger.LogError("RemoveFilterCols失败："  + ex.ToString());
                result = false;
            }
            return result;
        }

        public StatisticsMethodResult Calculate(StatisticsMethodVM  parameters)
        {
            var res = new StatisticsMethodResult();
            try
            {
                _logger.LogInformation($"开始计算");
                var userId = GlobalSetting.RScriptAcount;
                var outputPath =$@"\wwwroot\Output\{userId}\{parameters.MethodCode}\{DateTime.Now.Ticks.ToString()}";
                //@"\wwwroot\Output\1\1_1\637539039214698493";// 
                _logger.LogInformation($"计算目录");
                var target = _env.ContentRootPath + outputPath;
                //R 语言脚本文件夹
                var source = _env.ContentRootPath + Path.GetDirectoryName(parameters.MethodPath);
                Directory.CreateDirectory(target);
                var srcFiles = new DirectoryInfo(source).GetFiles();
                foreach (FileInfo item in srcFiles)
                {
                    item.CopyTo(Path.Combine(target, item.Name), true);
                }
                var cache = GetDataFlowCache(parameters.Node);
                _logger.LogInformation($"计算参数,node:{parameters.Node},projectId:{cache.ProjectInfo.Id},dataSetId:{cache.DataSetInfo.Id},dataResultId:{cache.Id}");
                var rMethod = CaculateMethodFactory.Build(cache, parameters.MethodCode, JsonConvert.SerializeObject(parameters));
                rMethod.RScriptWorkingDirectory = target;
                rMethod.RScriptFileName = Path.GetFileName(parameters.MethodPath);
                rMethod.StaticLogger = _staticlogger;
                var methodData = new MethodData()
                {
                    dt = cache.DataTable,
                    DataColumns = cache.DataColumns,
                };
                var isOk = rMethod.Execute(methodData);
               
                if (isOk)
                {
                    var tick1 = DateTime.Now.Ticks;
                    var done = false;
                    do
                    {
                        long tickDiff = DateTime.Now.Ticks;
                        double n = (tickDiff - tick1) / 10000000.0; 
                        if (n > 30)
                        {
                            done = true;
                            _logger.LogError("统计操作超时！");
                        }
                        if (System.IO.Directory.Exists(target))
                        {
                            res.OutputHTML = CalculateResult(target);
                            if (!string.IsNullOrEmpty(res.OutputHTML))
                            {
                                var fileList = System.IO.Directory.EnumerateFiles(target).OrderBy(f => Path.GetExtension(f));
                                if (fileList != null)
                                {
                                    foreach (var item in fileList)
                                    {
                                        var fileName = System.IO.Path.GetFileName(item);
                                        var ext = System.IO.Path.GetExtension(item);
                                        if (ext.ToLower() == ".png")
                                        {
                                            res.Pictures.Add(fileName);
                                        }
                                    }
                                    done = true;
                                }
                            }
                        }
                        else
                        {
                            System.Threading.Thread.Sleep(100);
                        }
                        done = true;
                    } while (!done);

                    if (res != null && !string.IsNullOrEmpty(res.OutputHTML))
                    {
                        res.OutputHTML = Path.Combine(outputPath.Replace("~", "").Replace(@"\wwwroot\",""), res.OutputHTML);
                        if (res.Pictures.Any())
                        {
                            for (int i = 0; i < res.Pictures.Count; i++)
                            {
                                res.Pictures[i] = Path.Combine(outputPath.Replace("~", "").Replace(@"\wwwroot\", ""), res.Pictures[i]);
                            }
                        }
                    }  
                }
               
            }
            catch(Exception ex)
            {
                _logger.LogError("Calculate 失败" + ex.ToString());
            }
            return res;
        }

        private string CalculateResult(string outPath)
        {
            var fileList = System.IO.Directory.EnumerateFiles(outPath).OrderBy(f => Path.GetExtension(f));
            if (fileList != null)
            {
                foreach (var item in fileList)
                {
                    var fileName = System.IO.Path.GetFileName(item);
                    var ext = System.IO.Path.GetExtension(item);
                    if (ext.ToLower() == ".htm" || ext.ToLower() == ".txt")
                    {
                        var result = fileName;
                        if (Path.GetExtension(result) == ".htm")
                        {
                            var content = File.ReadAllText(Path.Combine(outPath, result), System.Text.Encoding.Default);
                            if (!content.ToLower().Contains("</html>"))
                                result = string.Empty;
                        }
                        return result;
                    }
                }
            }
            return string.Empty;
        }


        public int DataRowFilterByColumns(string node, List<DataRowFilter> filterColumns)
        {
            var result = 0;
            try
            {
                var cacheData = GetDataFlowCache(node);
                StringBuilder filter = new StringBuilder();
                foreach(var r in filterColumns)
                {
                    if(r.Operation.Equals("like"))
                    {
                        filter.Append($"{r.Column} {r.Operation} '%{r.Value}%' and ");
                    }
                    else if(r.Operation.Equals("nl"))
                    {
                        filter.Append($"{r.Column} not like '%{r.Value}%' and ");
                    }
                    else
                    {
                        filter.Append($"{r.Column} {r.Operation} '{r.Value}' and ");
                    }
                    
                }
                filter.Append(" 1=1");

                var filterRows = cacheData.DataTable.Select(filter.ToString());
                if (filterRows.Length > 0)
                {
                    result = filterRows.Length;
                    cacheData.DataTable = filterRows.CopyToDataTable();
                    cacheData.DataCount = filterRows.Length;
                    var handler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);
                    cacheData.DataColumns = handler.GenerateColumnDataForCache(cacheData.DataTable,cacheData.DataColumns);

                }
            }
            catch (Exception ex)
            {
                _logger.LogError("DataRowFilterByColumns失败：" + ex.ToString());
            }
            return result;

        }

        public bool DataSampleExtractSimple(string node, DataSampleModel simObj)
        {
            var result = true;
            try
            {
                var cacheData = GetDataFlowCache(node);
                DataTable dtRel = new DataTable();
                var dt = cacheData.DataTable;
                int dataCount = dt.Rows.Count;
                switch (simObj.Method)
                {
                    case "extract":

                        for (int i = 1; i <= dt.Rows.Count; i++)
                        {
                            if (i % simObj.SimVal == 1)
                                dtRel.ImportRow(dt.Rows[i]);
                        }
                        break;
                    case "pecent":
                        bool valBoo = System.Text.RegularExpressions.Regex.IsMatch(simObj.SimVal.ToString(), @"^[1-9]\d*$");
                        if (!valBoo)
                            simObj.SimVal = simObj.SimVal * 100;
                        if (simObj.SimMax > 0)
                        {
                            double dc = 0;
                            if (valBoo)
                                dc = dataCount * ((double)(simObj.SimVal) / 100);
                            else
                                dc = dataCount * ((double)(simObj.SimVal));
                            if (simObj.SimMax > dc)
                                dtRel = dt.AsEnumerable().Take(simObj.SimMax).CopyToDataTable();
                            else
                            {
                                dtRel = dt.AsEnumerable().Take(Convert.ToInt32(Math.Round(simObj.SimMax / (double)(simObj.SimVal / 100), 0))).CopyToDataTable();
                                dtRel = dtRel.AsEnumerable().Take(Convert.ToInt32(Math.Round(dataCount * (simObj.SimVal / 100), 0))).CopyToDataTable();
                            }
                        }
                        else
                        {
                            dtRel = dt.AsEnumerable().Take(Convert.ToInt32(Math.Round(dataCount * (simObj.SimVal / 100), 0))).CopyToDataTable();
                        }
                        break;
                    default:
                        dtRel = dt.AsEnumerable().Take(Convert.ToInt32(Math.Round(simObj.SimVal, 0))).CopyToDataTable();
                        break;
                }

                cacheData.DataTable = dtRel;
                cacheData.DataCount = dtRel.Rows.Count;
                var handler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);
                cacheData.DataColumns = handler.GenerateColumnDataForCache(cacheData.DataTable, cacheData.DataColumns);
            }
            catch (Exception ex)
            {
                _logger.LogError("ExtractSimple失败：" + ex.ToString());
                result = false;
            }
            return result;
        }

        public bool DataSampleExtractComplex(string node, DataSampleModel complexObj)
        {
            var result = true;
            try
            {
               //to do
            }
            catch (Exception ex)
            {
                _logger.LogError("ExtractSimple失败：" + ex.ToString());
                result = false;
            }
            return result;
        }

        public bool DataCombineAppend(string node, List<string> prevNodeIds, string fieldSource)
        {
            var result = true;
            try
            {
                var mainData = GetDataFlowCache(prevNodeIds[0], "");
                var secondData = GetDataFlowCache(prevNodeIds[1], "");

                var table1 = mainData.DataTable;
                var table2 = secondData.DataTable;
                var modifyTable = table1.Copy();

                if (fieldSource == "mainData")
                {
                    modifyTable.Merge(table2, true, MissingSchemaAction.Ignore);
                }
                else
                {
                    modifyTable.Merge(table2,true,MissingSchemaAction.Add);
                }

                var mainColumns = mainData.DataColumns.Select(x => x.Name).ToList();
                var secondColumns = secondData.DataColumns.Select(x => x.Name).ToList();
                //DataTable dt = new DataTable();
                //if (fieldSource == "mainData")
                //{
                //    string[] colCommon = mainColumns.Intersect(secondColumns).ToArray();
                //    dt = mainData.DataTable.Copy();
                //    DataTable dt2 = secondData.DataTable.DefaultView.ToTable(false, colCommon);
                //    foreach (DataRow itemR in dt2.Rows)
                //    {
                //        DataRow dr = dt.NewRow();
                //        foreach (System.Data.DataColumn itemC in dt2.Columns)
                //        {
                //            dr[itemC] = itemR[itemC];
                //        }
                //        dt.Rows.Add(dr);
                //    }
                //}
                //else
                //{
                //    string[] colCommon = mainColumns.Union(secondColumns).ToArray();
                //    //获取两个数据源的并集
                //    IEnumerable<DataRow> query2 = secondData.DataTable.AsEnumerable().Union(secondData.DataTable.AsEnumerable(), DataRowComparer.Default);
                //    //两个数据源的并集集合
                //    dt = query2.CopyToDataTable();
                //}
                var diffColumns = secondColumns.Except(mainColumns).ToList();
                var data = new DataFlowCacheModel();
                var handler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);

                var columns = mainData.DataColumns;
                if (diffColumns.Count > 0)
                {
                    foreach(var r in diffColumns)
                    {
                        columns.Add(secondData.DataColumns.FirstOrDefault(x => x.Name == r));
                    }
                }
                data.DataTable = modifyTable;
                data.DataColumns = handler.GenerateColumnDataForCache(modifyTable, columns);
                data.DataCount = modifyTable.Rows.Count;
                data.DataSetInfo = mainData.DataSetInfo;
                data.Id = mainData.Id;
                data.Name = mainData.Name;
                data.ProjectInfo = mainData.ProjectInfo;
                data.TableName = mainData.TableName;
                data.WdName = mainData.WdName;

                SetDataFlowCache(data, node);

            }
            catch (Exception ex)
            {
                _logger.LogError("ExtractSimple失败：" + ex.ToString());
                result = false;
            }
            return result;
        }

        public bool DataColumnFillField(string node, string fieldName, string condition, string filedValue)
        {
            var result = true;
            try
            {
                var cacheData = GetDataFlowCache(node);
                var dt = cacheData.DataTable;
                StringBuilder filter = new StringBuilder();
                switch (condition)
                {
                    case "null":
                        filter.Append($"{fieldName} is null and ");
                        break;
                    case "blank":
                        filter.Append($"{fieldName} ='' and ");
                        break;
                    case "blankOrNull":
                        filter.Append($"{fieldName} is null or {fieldName} =''  and ");
                        break;
                }
                filter.Append(" 1=1");
                var rows = dt.Select(filter.ToString());
                if (rows != null && rows.Length > 0)
                {
                    for (int i = 0; i < rows.Length; i++)
                    {
                        rows[i][fieldName] = filedValue;
                    }
                    var handler = new LegacyCodeHandler(_extractEngineDataHandlerlogger);
                    cacheData.DataColumns = handler.GenerateColumnDataForCache(cacheData.DataTable, cacheData.DataColumns);
                }
            }
            catch (Exception ex)
            {
                _logger.LogError("DataColumnFillField失败：" + ex.ToString());
                result = false;
            }
            return result;
        }
    }

}
