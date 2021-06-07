using Newtonsoft.Json;
using SqlSugar;
using System;
using System.Collections.Generic;
using System.Text;

namespace KYTJ.Model
{
    [SugarTable("DF_Dataset")]
    public class DataSetModel
    {
        [SugarColumn(ColumnName = "id", IsPrimaryKey = true, IsIdentity = true)]
        [JsonProperty("dataSetId")]
        public int DataSetId { get; set; }

        [SugarColumn(ColumnName = "DisplayName")]
        public string DataSetName { get; set; }

        [SugarColumn(ColumnName = "Description")]
        public string DataSetDesc { get; set; }

        public int IsGenerated { get; set; }

        [SugarColumn(ColumnName = "CreateDateTime")]
        public DateTime CreateTime { get; set; }

        public DateTime UpdateDateTime { get; set; }

        [SugarColumn(ColumnName = "SearchResult_id")]

        public int ProjectId { get; set; }
        [SugarColumn(IsIgnore = true)]
        public string ProjectName { get; set; }

        [SugarColumn(ColumnName = "SourceType")]
        public string DataSource { get; set; }
        public string UserName { get; set; }

        public string DbName { get; set; }
        [SugarColumn(IsIgnore = true)]
        public List<ResultData> ResultDataList { get; set; }
    }

    [SugarTable("RD_ResultData")]
    public class ResultData
    {
        [SugarColumn(IsPrimaryKey = true, IsIdentity = true)]
        public int Id { get; set; }
        public string Name { get; set; }

        public int DataCount { get; set; }
        public DateTime CreateDateTime { get; set; }

        public DateTime UpdateDateTime { get; set; }
        public string WdName { get; set; }
        public string TableName { get; set; }

        [SugarColumn(ColumnName = "dataSet_id")]
        public int DataSetId { get; set; }

        [SugarColumn(IsIgnore = true)]
        public List<RdDataColumn> DataColumns { get; set; }
    }

    [SugarTable("RD_DataColumn")]
    public class RdDataColumn
    {
        [SugarColumn(IsPrimaryKey = true, IsIdentity = true)]
        public int Id { get; set; }
        public string Name { get; set; }
        public string Rem { get; set; }
        public string CleansingMethod { get; set; }
        public bool IsContinuous { get; set; }
        public string StatisticsInfo { get; set; }
        public int KindCount { get; set; }

        [SugarColumn(ColumnName = "ResultData_Id")]
        public int ResultDataId { get; set; }
        public bool IsGrouping { get; set; }

        [SugarColumn(IsIgnore = true)]
        public string NullPercent { get; set; }

        [SugarColumn(IsIgnore = true)]
        public List<RdGroupingTag> GroupingTags { get; set; }
    }

    [SugarTable("RD_GroupingTag")]
    public class RdGroupingTag
    {
        [SugarColumn(IsPrimaryKey = true, IsIdentity = true)]
        public int Id { get; set; }
        public string Name { get; set; }
        public string Index { get; set; }

        [SugarColumn(ColumnName = "DataColumn_Id")]
        public int DataColumnId { get; set; }
    }

    public class FillFieldRdColumnModel
    {
        public string Condition { get; set; }
        public string FieldValue { get; set; }
        public string ColumnName { get; set; }
        public int ResultDataId { get; set; }
        public int RdDataColumnId { get; set; }
    }
}
