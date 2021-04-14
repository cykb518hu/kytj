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
        DataFlowCacheModel GetDataFlowCache(string node, string prevNode, bool removeDt = true);

        public List<DFDataColumn> GetFilterCols(string node, string filterType, int filterPercent);

        bool RemoveFilterCols(string node, List<int> ids);
        StatisticsMethodResult Calculate(StatisticsMethodVM parameters);
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

        public DataFlowRepository(IDataManageRepository dataManageRepository, IDataSetRepository dataSetRepository, IProjectRepository projectRepository,ILogger<DataFlowRepository> logger, ICacheHandler cacheHandler, IHostingEnvironment env, ILogger<StatisticsMethod> staticlogger)
        {
            _dataManageRepository = dataManageRepository;
            _dataSetRepository = dataSetRepository;
            _projectRepository = projectRepository;
            _logger = logger;
            _cacheHandler = cacheHandler;
            _env = env;
            _staticlogger = staticlogger;
        }

        public DataFlowCacheModel GetDataFlowCache(string node, string prevNode, bool removeDt = true)
        {
            var data = _cacheHandler.Get<DataFlowCacheModel>(node);
            if (data == null && !string.IsNullOrEmpty(prevNode))
            {
                data = _cacheHandler.Get<DataFlowCacheModel>(prevNode);
                SetDataFlowCache(data, node);
            }
            if (data != null && removeDt)
            {
                var result = new DataFlowCacheModel();
                result.DataColumns = data.DataColumns;
                result.DataCount = data.DataCount;
                result.DataSetInfo = data.DataSetInfo;
                result.Id = data.Id;
                result.Name = data.Name;
                result.ProjectInfo = data.ProjectInfo;
                result.TableName = data.TableName;
                result.WdName = data.WdName;
                return result;
            }
            return data;
        }
        public void SetDataFlowCache(int resultDataId,string node)
        {
            var data = GetDataFlowCacheFromDb(resultDataId);
            _cacheHandler.Set(node, data);
        }

        public void SetDataFlowCache(DataFlowCacheModel data,string node)
        {
            _cacheHandler.Set(node, data);
        }
        public DataFlowCacheModel GetDataFlowCacheFromDb(int resultDataId)
        {
            
            DataFlowCacheModel data = new DataFlowCacheModel();
            try
            {
                var dataSetId = 0;
                var projectId = 0;
                var rd = _dataManageRepository.GetRdDataAndSub(resultDataId);

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

                    var dataSet = _dataSetRepository.GetDataSet(dataSetId);
                    projectId = dataSet.ProjectId;
                    data.DataSetInfo = new DFDataSet
                    {
                        Id = dataSet.DataSetId,
                        Name = dataSet.DataSetName
                    };
                    var project = _projectRepository.GetProject(projectId);
                    data.ProjectInfo = new DFProject
                    {
                        Id = project.Id,
                        Name = project.ProjectName
                    };
                    data.DataTable = _dataManageRepository.GetOriginalTable(data.TableName);
                }
            }
            catch(Exception ex)
            {
                _logger.LogError("获取缓存初始化数据失败：" + ex.ToString());
            }
            return data;
        }

        public List<DFDataColumn> GetFilterCols(string node,string filterType,int filterPercent)
        {
            var cols = new List<DFDataColumn>();
            try
            {
                var cacheData = GetDataFlowCache(node, "", false);
                var count = cacheData.DataCount;
                var dt = cacheData.DataTable;
                switch (filterType)
                {
                    case "unique":
                        foreach (System.Data.DataColumn item in dt.Columns)
                        {
                            bool s = ConvertTypeToSqlDbType(item.DataType, count, dt, item.ColumnName, filterPercent);
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

        private bool ConvertTypeToSqlDbType(Type type, int count, DataTable dt, string columnName, int value)
        {
            bool columnExist = false;
            int dCo = 0;
            switch (type.Name)
            {
                case "String":
                    List<string> list = (from d in dt.AsEnumerable() select d.Field<string>($"{columnName}")).ToList();
                    dCo = list.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;
                    break;
                case "Int32":
                    List<int?> listIn = (from d in dt.AsEnumerable() select d.Field<int?>($"{columnName}")).ToList();
                    dCo = listIn.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;
                    break;
                case "Int16":
                    List<Int16?> listSm = (from d in dt.AsEnumerable() select d.Field<Int16?>($"{columnName}")).ToList();
                    dCo = listSm.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;
                    break;
                case "Int64":
                    List<Int64?> listBig = (from d in dt.AsEnumerable() select d.Field<Int64?>($"{columnName}")).ToList();
                    dCo = listBig.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;
                    break;
                case "DateTime":
                    List<DateTime?> listdt = (from d in dt.AsEnumerable() select d.Field<DateTime?>($"{columnName}")).ToList();
                    dCo = listdt.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;

                    break;
                case "Decimal":
                    List<decimal?> listde = (from d in dt.AsEnumerable() select d.Field<decimal?>($"{columnName}")).ToList();
                    dCo = listde.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;
                    break;
                case "Double":
                    List<double?> listflo = (from d in dt.AsEnumerable() select d.Field<double?>($"{columnName}")).ToList();
                    dCo = listflo.GroupBy(c => c).Count();
                    columnExist = (float)dCo / count >= (float)value / 100;
                    break;
                default:
                    break;
            }
            return columnExist;
        }

        public bool RemoveFilterCols(string node, List<int> ids)
        {
            var result = true;
            try
            {
                var cacheData = GetDataFlowCache(node, "", false);

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
                var cache = GetDataFlowCache(parameters.Node, "", false);
                _logger.LogInformation($"计算参数,node:{parameters.Node},projectId:{cache.ProjectInfo.Id},dataSetId:{cache.DataSetInfo.Id},dataResultId:{cache.Id}");
                var rMethod = MethodFactory.Build(cache, parameters.MethodCode, JsonConvert.SerializeObject(parameters));
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
                            res.OutputHTML = ResultGenerated(target);
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

        private  string ResultGenerated(string outPath)
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

    }

}
