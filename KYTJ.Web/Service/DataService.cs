using KYTJ.Data.Repository;
using KYTJ.Infrastructure.Model;
using Microsoft.Extensions.Logging;
using NPOI.HSSF.UserModel;
using NPOI.SS.UserModel;
using NPOI.XSSF.UserModel;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Net;
using System.ServiceModel;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace KYTJ.Web.Service
{
    [ServiceContract]
    public interface IDataService
    {
        [OperationContract]
        string Test(string str);

        [OperationContract]
        string GetAllDiseasesList();

        [OperationContract]
        string GetDataByBZ(string projectid, string projectname, string filePath, string userId);

    }
    public class DataService: IDataService
    {
        private readonly ILogger<DataService> _logger;
        private readonly IProjectRepository _projectRepository;
        private readonly ILogRepository _logRepository;
        private readonly IDataSetRepository _dataSetRepository;
        public DataService(ILogger<DataService> logger, IProjectRepository projectRepository, ILogRepository logRepository, IDataSetRepository dataSetRepository)
        {
            _logger = logger;
            _projectRepository = projectRepository;
            _logRepository = logRepository;
            _dataSetRepository = dataSetRepository;
        }
        public string Test(string str)
        {
            return str;
        }
        public string GetAllDiseasesList()
        {
            string s = "[]";
            try
            {
               var data= _projectRepository.GetAllProjects();
                
                var dt =new List<object>();
                foreach(var r in data)
                {
                    dynamic project = new { id = r.Id, DiseaseName = r.ProjectName };
                    dt.Add(project);
                }
                s = Newtonsoft.Json.JsonConvert.SerializeObject(dt);
            }
            catch (Exception ex)
            {
                _logger.LogError("service-GetAllDiseasesList:" + ex.Message + ex.StackTrace);
            }
            return s;
        }

        public string GetDataByBZ(string projectid, string projectname, string filePath, string userId)
        {
            _logger.LogWarning($"输入参数, projectid:{projectid},projectname:{projectname},filepath:{filePath},userid:{userId}");

            string url = "";
            decimal datasetId = 0;
            try
            {
                DataTable dt = ExcelImport(filePath);
                if (dt != null & dt.Rows.Count > 0)
                    datasetId = _dataSetRepository.ExtractEngineDataForService(projectid, projectname, userId, dt);
            }
            catch (Exception ex)
            {
                return "导入数据失败";
                _logger.LogError("GetDataByBZ ex.Message:" + ex.Message + Environment.NewLine + "ex.StackTrace" + ex.StackTrace);
            }
            if (datasetId > 0)
            {
                string sitePath = GlobalSetting.SiteUrl;
                url = $"{sitePath}/" + "?datasetId=" + datasetId;
            }

            return url;
        }

        #region 单病种数据导入
        public DataTable ExcelImport(string strFileName)
        {
            DataTable dt = new DataTable();
            try
            {
                ISheet sheet = null;

                using (System.IO.MemoryStream memStream = new System.IO.MemoryStream())
                {
                    memStream.Position = 0;
                    WebClient webClient = new WebClient();
                    var obj = webClient.DownloadData(strFileName);//fileName 是远程url地址，可以url直接下载
                    Stream stream = new MemoryStream(obj);
                    if (strFileName.IndexOf("/raw") > 0)
                    {
                        XSSFWorkbook xssfworkbook = new XSSFWorkbook(stream);
                        sheet = xssfworkbook.GetSheetAt(0);
                    }
                    else if (strFileName.IndexOf(".xlsx") == -1) //2003
                    {
                        HSSFWorkbook hssfworkbook = new HSSFWorkbook(stream);
                        sheet = hssfworkbook.GetSheetAt(0);
                    }
                    else//2007
                    {
                        XSSFWorkbook xssfworkbook = new XSSFWorkbook(stream);
                        sheet = xssfworkbook.GetSheetAt(0);
                    }
                }
                System.Collections.IEnumerator rows = sheet.GetRowEnumerator();
                IRow headerRow = sheet.GetRow(0);
                int cellCount = headerRow.LastCellNum;

                for (int j = 0; j < cellCount; j++) //需求：列名的重复,则删除重复的列
                {
                    ICell cell = headerRow.GetCell(j);
                    var name = cell.ToString();
                    name = Regex.Replace(name, "[ \\[ \\] \\^ \\*×(^)$%~!@#$…&%￥√+=<>《》!！??？:：•`·、。，；,.;\"‘’“”-]", " ");
                    dt.Columns.Add(name); //添加这列名
                }
                for (int i = (sheet.FirstRowNum + 1); i <= sheet.LastRowNum; i++)
                {
                    IRow row = sheet.GetRow(i);
                    DataRow dataRow = dt.NewRow();

                    for (int j = row.FirstCellNum; j < cellCount; j++)
                    {
                        if (row.GetCell(j) != null)
                            switch (row.GetCell(j).CellType)
                            {
                                case CellType.Blank:
                                    dataRow[j] = "";
                                    break;
                                case CellType.Boolean:
                                    dataRow[j] = row.GetCell(j);
                                    break;
                                case CellType.Error:
                                    break;
                                case CellType.Formula:
                                    dataRow[j] = row.GetCell(j).CellFormula;
                                    break;
                                case CellType.Numeric:
                                    if (DateUtil.IsCellDateFormatted(row.GetCell(j)))
                                        dataRow[j] = row.GetCell(j).DateCellValue.ToString();
                                    else
                                        dataRow[j] = row.GetCell(j);
                                    break;
                                case CellType.String:
                                    dataRow[j] = row.GetCell(j).StringCellValue;
                                    break;
                                case CellType.Unknown:
                                    break;
                                default:
                                    dataRow[j] = row.GetCell(j);
                                    break;
                            }
                    }

                    dt.Rows.Add(dataRow);
                }
                return dt;
            }
            catch (Exception ex)
            {
                _logger.LogError("ExcelImport ex.Message:" + ex.Message + Environment.NewLine + "ex.StackTrace" + ex.StackTrace);
                return dt;
            }
        }

        #endregion

    }
}
