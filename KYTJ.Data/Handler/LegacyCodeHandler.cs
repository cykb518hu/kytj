using KYTJ.Data.Context;
using KYTJ.Data.Repository;
using KYTJ.Model;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace KYTJ.Data.Handler
{
    public class LegacyCodeHandler: KytjDbContext
    {
        private readonly ILogger<LegacyCodeHandler> _logger;
        public LegacyCodeHandler(ILogger<LegacyCodeHandler> logger)
        {
            _logger = logger;
        }

        public DataTable ConvertToStandardTable(DataTable _outDataSource, string dataType)
        {
            DataTable distinct_key = _outDataSource.DefaultView.ToTable(true, "Exportkey");
            System.Data.DataColumn[] primarykey_ = new System.Data.DataColumn[] { _outDataSource.Columns["Exportkey"], _outDataSource.Columns[dataType] };
            _outDataSource.PrimaryKey = primarykey_;
            DataTable new_DataTable = new DataTable();
            System.Data.DataColumn new_d_col = new System.Data.DataColumn();
            new_d_col.ColumnName = dataType;
            new_d_col.DataType = typeof(string);
            new_DataTable.Columns.Add(new_d_col);
            foreach (DataRow dr in distinct_key.Rows)
            {
                new_d_col = new System.Data.DataColumn();
                new_d_col.ColumnName = dr["Exportkey"].ToString();
                new_d_col.Caption = dr["Exportkey"].ToString();
                new_DataTable.Columns.Add(new_d_col);
            }
            DataTable distinct_object = _outDataSource.DefaultView.ToTable(true, dataType);

            DataRow new_dr;
            foreach (DataRow dr in distinct_object.Rows)
            {
                new_dr = new_DataTable.NewRow();
                new_dr[dataType] = dr[dataType].ToString();
                foreach (DataRow _dr in distinct_key.Rows)
                {
                    //object[] obj = new object[] { dr[dataType].ToString(), _dr["Exportkey"].ToString() };
                    DataRow drs = _outDataSource.Rows.Find(new object[] { _dr["Exportkey"].ToString(), dr[dataType].ToString() });
                    if (drs != null) new_dr[_dr["Exportkey"].ToString()] = drs["ExportValue"];
                }
                new_DataTable.Rows.Add(new_dr);
            }
            return new_DataTable;
        }

        public void CreateSchemaAndExportData(ExtractDataSetMappingModel mapping, DataTable dataTable)
        {
            //创建行转列的表
            _logger.LogInformation("创建行转列的表");
            CreateSchema(mapping.DataSetId,  dataTable);
            //插入DF_Field表
            _logger.LogInformation("插入DF_Field表");
            InsertDF(mapping.DataSetId, mapping.TableId, dataTable);

            //插入行转列的数据
            _logger.LogInformation("插入行转列的数据");
            BulkInsertEngineData(dataTable);

            //更新分析数据
            _logger.LogInformation("更新分析数据");
            //这个地方必须要从数据库里面取，之前的datatable column 没有具体类型，全是string
            var dt = _dbResearch.Ado.GetDataTable($"select * from dbo.[{dataTable.TableName}]");
            GenerateColumnData(dt, mapping.ResultDataId);
        }
        public void CreateSchema(int datasetId,  DataTable dataTable)
        {

            //DataTable dt = dataTable.Clone();

            var sql = $"CREATE TABLE [" + dataTable.TableName + "] (%1)";
            var cols = new List<string>();
            List<string> dfField = new List<string>();
            for (int i = 0; i < dataTable.Columns.Count; i++)
            {
                dataTable.Columns[i].ColumnName = dataTable.Columns[i].ColumnName.Replace("[", "").Replace("]", "").Replace("&", "_").Replace("-", ".");
                dfField.Add(dataTable.Columns[i].ColumnName);
                var dbtype = GetDBType(dataTable.Columns[i].DataType);
                int ee = -1;
                bool Isdouble = true;
                bool isDate = true;
                try
                {
                    switch (dbtype)
                    {
                        case SqlDbType.NVarChar:
                            List<string> listCol = GetColumnValues<string>(dataTable, dataTable.Columns[i].ColumnName);
                            foreach (var item in listCol)
                            {
                                if (!string.IsNullOrEmpty(item))
                                {
                                    DateTime date;
                                    if (!Regex.IsMatch(item, @"^[+-]?\d*[.]?\d*$"))
                                    {
                                        Isdouble = false;
                                        if (!DateTime.TryParse(item, out date))
                                        {
                                            isDate = false;
                                            break;
                                        }
                                        else continue;
                                    }
                                    else if (!DateTime.TryParse(item, out date))
                                    {
                                        isDate = false;
                                        if (!Regex.IsMatch(item, @"^[+-]?\d*[.]?\d*$"))
                                        {
                                            Isdouble = false;
                                            break;
                                        }
                                        else continue;
                                    }
                                    else continue;
                                }
                            }
                            if (!Isdouble && !isDate)
                                ee = listCol.Max(c => c.Length);
                            else if (Isdouble)
                            {
                                dataTable.Columns[i].DataType = System.Type.GetType("System.Double");
                                dataTable.Columns[i].AllowDBNull = true;
                            }
                            else if (isDate)
                            {
                                dataTable.Columns[i].DataType = System.Type.GetType("System.DateTime");
                                dataTable.Columns[i].AllowDBNull = true;
                            }
                            break;
                        default:
                            break;
                    }
                }
                catch (Exception ex)
                {
                    ee = -1;
                }
                if (ee > 0 && ee < 20)
                    ee = 20;
                else if (ee > 20 && ee < 200)
                    ee = 200;
                else if (ee > 200 && ee < 500)
                    ee = 500;
                else if (ee > 500 && ee < 2000)
                    ee = 2000;
                else if (ee > 2000)
                    ee = 4000;

                string str = string.Empty;
                if (Isdouble)
                    str = "float";
                else if (isDate)
                    str = "Datetime";
                else
                    str = dbtype.ToString() + (dbtype == SqlDbType.NVarChar ? (ee > 0 ? "(" + ee + ")" : "(MAX)") : "");
                cols.Add($" [{dataTable.Columns[i].ColumnName}] {str} NULL");
            }
            sql = sql.Replace("%1", string.Join(",\r\n", cols));
            _dbResearch.Ado.ExecuteCommand(sql);
        }
        private void InsertDF(int dataSetId, int tableId, DataTable dataTable)
        {
            StringBuilder sdField = new StringBuilder($"insert into dbo.DF_Field ");
            for (int i = 0; i < dataTable.Columns.Count; i++)
            {
                var name = dataTable.Columns[i].ColumnName;
                if (i == 0)
                    sdField.Append($" select '{name}','',null,1,'{name}',null,{tableId},{dataSetId} ");
                else
                    sdField.Append($" union all select '{name}','',null,1,'{name}',null,{tableId},{dataSetId} ");
            }
            _dbKyStatic.Ado.ExecuteCommand(sdField.ToString());
        }

        private SqlDbType GetDBType(System.Type theType)
        {
            System.Data.SqlClient.SqlParameter p1;
            System.ComponentModel.TypeConverter tc;
            p1 = new System.Data.SqlClient.SqlParameter();
            tc = System.ComponentModel.TypeDescriptor.GetConverter(p1.DbType);
            if (tc.CanConvertFrom(theType))
            {
                p1.DbType = (System.Data.DbType)tc.ConvertFrom(theType.Name);
            }
            else
            {
                //Try brute force
                try
                {
                    p1.DbType = (System.Data.DbType)tc.ConvertFrom(theType.Name);
                }
                catch (Exception)
                {

                    //Do Nothing; will return NVarChar as default
                }
            }
            return p1.SqlDbType;
        }
        public List<T> GetColumnValues<T>(DataTable dtSource, string filedName)
        {
            return (from r in dtSource.AsEnumerable() select r.Field<T>(filedName)).ToList<T>();
        }


        public  void BulkInsertEngineData(DataTable table)
        {
            if (string.IsNullOrEmpty(table.TableName)) throw new Exception("DataTable.TableName属性不能为空");
            var tableName = $"[{table.TableName}]";
            using (SqlBulkCopy bulk = new SqlBulkCopy(ResearchData))
            {
                bulk.BatchSize = 5000;
                bulk.BulkCopyTimeout = 60;
                bulk.DestinationTableName = tableName;

                foreach (System.Data.DataRow row in table.Rows)
                {
                    for (var i = 0; i < table.Columns.Count; i++)
                    {
                        if (string.IsNullOrEmpty(row[i] as string))
                        { 
                            row[i] = null;
                        }

                    }
                 
                }

                foreach (DataColumn col in table.Columns)
                {
                    bulk.ColumnMappings.Add(col.ColumnName, col.ColumnName);
                }
                bulk.WriteToServer(table);
                bulk.Close();
            }
        }

        public void GenerateColumnData(DataTable dt,int resultDataId)
        {
            try
            {

                var rdDataColumns = new List<RdDataColumn>();
                var dataCount = dt.Rows.Count;
                List<RdGroupingTag> removedTags = new List<RdGroupingTag>();
                for (int i = 0; i < dt.Columns.Count; i++)
                {
                    var name = dt.Columns[i].ColumnName;
                    var col = new RdDataColumn();
                    col.ResultDataId = resultDataId;
                    col.GroupingTags = new List<RdGroupingTag>();
                    col.Name = name;
                    col.Rem = name;
                    var kindCount = 0;
                    var nullCount = 0;
                    List<RdGroupingTag> tags = new List<RdGroupingTag>();
                    var colStat = GenerateColumnStatistics(dt, col, dataCount, out kindCount, out tags,out nullCount);
                    removedTags.AddRange(tags);
                    col.StatisticsInfo = JsonConvert.SerializeObject(colStat);
                    col.IsContinuous = (kindCount == 0);
                    col.KindCount = kindCount;
                    col.NullPercent = ((double)nullCount / dataCount).ToString("P");
                    if(!col.GroupingTags.Any())
                    {
                        col.GroupingTags = null;
                    }
                    rdDataColumns.Add(col);
                }
                if (removedTags != null && removedTags.Count > 0)
                {
                    _dbKyStatic.Deleteable<RdGroupingTag>().In(removedTags.Select(x => x.Id).ToList());
                }

                _dbKyStatic.Insertable(rdDataColumns).AddSubList(x => x.GroupingTags.First().DataColumnId)
                  .ExecuteReturnPrimaryKey();
            }
            catch (Exception ex)
            {
                _logger.LogInformation("更新分析数据失败:" + ex.ToString());
                throw ex;
            }
        }
        public List<DFDataColumn> GenerateColumnDataForCache(DataTable dt, List<DFDataColumn> columns)
        {
            var result = new List<DFDataColumn>();
            try
            {
                var dataCount = dt.Rows.Count;
                for (int i = 0; i < dt.Columns.Count; i++)
                {
                    var name = dt.Columns[i].ColumnName;
                    var col = new RdDataColumn();
                    col.GroupingTags = new List<RdGroupingTag>();
                    col.Name = name;
                    col.Rem = name;
                    var kindCount = 0;
                    var nullCount = 0;
                    List<RdGroupingTag> tags = new List<RdGroupingTag>();
                    var colStat = GenerateColumnStatistics(dt, col, dataCount, out kindCount, out tags, out nullCount);

                    col.StatisticsInfo = JsonConvert.SerializeObject(colStat);
                    col.IsContinuous = (kindCount == 0);
                    col.KindCount = kindCount;
                    col.NullPercent = ((double)nullCount / dataCount).ToString("P");

                    var dataColumnTags = new List<DFGroupingTag>();
                    foreach (var r in col.GroupingTags)
                    {
                        dataColumnTags.Add(new DFGroupingTag
                        {
                            Id = r.Id,
                            Name = r.Name,
                            Index = r.Index
                        });
                    }
                    var column = columns.FirstOrDefault(x => x.Name == name);
                    result.Add(new DFDataColumn {
                        Id=column.Id,
                        Name=column.Name,
                        Rem=column.Rem,
                        IsContinuous=col.IsContinuous,
                        KindCount=col.KindCount,
                        NullPercent=col.NullPercent,
                        StatisticsInfo=col.StatisticsInfo,
                        GroupingTags=dataColumnTags                   
                    });;
                }
            }
            catch (Exception ex)
            {
                _logger.LogInformation("更新分析数据失败:" + ex.ToString());
                throw ex;
            }
            return result;
        }
        public List<object> GenerateColumnStatistics(System.Data.DataTable dt, Model.RdDataColumn column, int dataCount, out int kindCount, out List<RdGroupingTag> tagsRemoved, out int nullCount)
        {
            int continuesBaseCount = 9; //多少数值算连续变量
            int count = dataCount;
            decimal min = 0m;
            decimal max = 0m;
            double mean = 0d;
            double std = 0d;
            Dictionary<string, int> valCountDict = new Dictionary<string, int>();   //同值计数
            bool isNum = true;
            //string nullKey = $"<null>{column.Name}";
            string nullKey = "";
            tagsRemoved = new List<RdGroupingTag>();
            //var data = new List<Dictionary<string, object>>();
            string type = dt.Columns[$"{column.Name}"].DataType.Name;
            List<object> colStatis = new List<object>();
            //List<struct> s = new List<struct>();
            switch (type)
            {
                case "Int32":

                    List<Int32?> ids = GetColumnValues<Int32?>(dt, column.Name);
                    nullCount = ids.Count(x => x == null);
                    if (ids.Count(c => c != null) > 0)
                    {
                        min = (decimal)ids.Min();
                        max = (decimal)ids.Max();
                        mean = (double)ids.Average();
                    }
                    var gro = ids.GroupBy(c => c);
                    foreach (var item in gro)
                    {

                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key.ToString(), item.Count());


                    }
                    break;
                case "Int16":
                    List<Int16?> colSmall = GetColumnValues<Int16?>(dt, column.Name);
                    nullCount = colSmall.Count(x => x == null);
                    if (colSmall.Count(c => c != null) > 0)
                    {
                        min = (decimal)colSmall.Min();
                        max = (decimal)colSmall.Max();
                        mean = (double)colSmall.Sum(c => c) / colSmall.Count();

                    }
                    var groSmall = colSmall.GroupBy(c => c);
                    foreach (var item in groSmall)
                    {
                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key.ToString(), item.Count());
                    }

                    break;
                case "Int64":
                    List<Int64?> colBig = GetColumnValues<Int64?>(dt, column.Name);
                    nullCount = colBig.Count(x => x == null);
                    if (colBig.Count(c => c != null) > 0)
                    {
                        min = (decimal)colBig.Min();
                        max = (decimal)colBig.Max();
                        mean = (double)colBig.Average();

                    }
                    var grobig = colBig.GroupBy(c => c);
                    foreach (var item in grobig)
                    {
                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key.ToString(), item.Count());
                    }
                    break;
                case "DateTime":
                    List<DateTime?> colDate = GetColumnValues<DateTime?>(dt, column.Name);
                    nullCount = colDate.Count(x => x == null);
                    var groDate = colDate.GroupBy(c => c);
                    foreach (var item in groDate)
                    {
                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key?.ToString("yyyy-MM-dd"), item.Count());
                    }
                    isNum = false;
                    break;
                case "Decimal":
                    List<decimal?> coldec = GetColumnValues<decimal?>(dt, column.Name);
                    nullCount = coldec.Count(x => x == null);
                    if (coldec.Count(c => c != null) > 0)
                    {
                        min = (decimal)coldec.Min();
                        max = (decimal)coldec.Max();
                        mean = (double)Convert.ToDouble(coldec.Average());

                    }
                    var grodec = coldec.GroupBy(c => c);
                    foreach (var item in grodec)
                    {
                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key.ToString(), item.Count());
                    }

                    break;
                case "Double":
                    List<double?> coldo = GetColumnValues<double?>(dt, column.Name);
                    nullCount = coldo.Count(x => x == null);
                    if (coldo.Count(c => c != null) > 0)
                    {
                        min = Convert.ToDecimal(coldo.Min());
                        max = Convert.ToDecimal(coldo.Max());
                        mean = (double)coldo.Average();
                    }
                    var grodo = coldo.GroupBy(c => c);
                    foreach (var item in grodo)
                    {
                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key.ToString(), item.Count());
                    }
                    break;
                case "Single":
                    List<float?> colflo = GetColumnValues<float?>(dt, column.Name);
                    nullCount = colflo.Count(x => x == null);
                    if (colflo.Count(c => c != null) > 0)
                    {
                        min = Convert.ToDecimal(colflo.Min());
                        max = Convert.ToDecimal(colflo.Max());
                        mean = (double)colflo.Average();
                    }
                    var groflo = colflo.GroupBy(c => c);
                    foreach (var item in groflo)
                    {
                        if (item.Key == null) valCountDict.Add(nullKey, item.Count());
                        else valCountDict.Add(item.Key.ToString(), item.Count());
                    }
                    break;
                default://字符串
                    List<string> colStr = GetColumnValues<string>(dt, column.Name);
                    nullCount = colStr.Count(x => x == null);
                    var groStr = colStr.GroupBy(c => c);
                    foreach (var item in groStr)
                    {
                        if (string.IsNullOrEmpty(item.Key))
                        {
                            if (valCountDict.ContainsKey(nullKey))
                                //valCountDict.Add(nullKey, valCountDict[nullKey] + item.Count());
                                valCountDict[nullKey] = valCountDict[nullKey] + item.Count();
                            else
                                valCountDict.Add(nullKey, item.Count());
                        }
                        else
                            valCountDict.Add(item.Key, item.Count());

                    }
                    isNum = false;
                    break;
            }
            continuesBaseCount = valCountDict.Keys.Count < 100 ? 5 : 9;
            if (valCountDict.Keys.Count > continuesBaseCount && isNum)
            {
                var d = 0d;

                foreach (var item in valCountDict.Keys)
                {
                    if (item == nullKey) continue;
                    d += Math.Pow((Convert.ToDouble(item) - Convert.ToDouble(mean)), 2) * Convert.ToDouble(valCountDict[item]);
                }
                std = Math.Pow((d / (count - 1)), 0.5);

                colStatis.Add(new { min = min, max = max, mean = mean, std = std, count = count, });
            }
            else
            {
                if (column.IsGrouping == false)
                {
                    var tmpList = new List<RdGroupingTag>();
                    column.GroupingTags.ToList().ForEach(t => { tmpList.Add(t); });
                    foreach (var val in valCountDict.Keys)
                    {
                        colStatis.Add(new { value = val ?? "", count = valCountDict[val], percent = valCountDict[val] * 1.0 / count * 100 });
                        if (val != null)
                        {
                            var tag = tmpList.Where(t => t.Name == val.ToString()).FirstOrDefault(); //new GroupingTag() { Index = val.Key, Name = val.Key, };
                            if (tag == null) column.GroupingTags.Add(new RdGroupingTag() { Index = val.ToString(), Name = val.ToString(), });
                            else tmpList.Remove(tag);
                        }
                    }
                    tagsRemoved = tmpList;
                }
            }
            kindCount = isNum && valCountDict.Keys.Count > continuesBaseCount ? 0 : valCountDict.Keys.Count();
            return colStatis;
        }

        public bool ConvertTypeToSqlDbType(Type type, int count, DataTable dt, string columnName, int value)
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


    }
}
