using SqlSugar;
using System;
using System.Collections.Generic;
using System.Data;
using System.Text;

namespace KYTJ.Model
{
    [Serializable]
    public class DFProject
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }
    [Serializable]
    public class DFDataSet
    {
        public int Id { get; set; }

        public string Name { get; set; }
    }

    [Serializable]
    public class DataFlowCacheModel
    {
        public int Id { get; set; }
        public string Name { get; set; }

        public int DataCount { get; set; }
        public string TableName { get; set; }
        public string WdName { get; set; }

        public DFDataSet DataSetInfo { get; set; }

        public DFProject ProjectInfo { get; set; }

        public List<DFDataColumn> DataColumns { get; set; }

        public DataTable DataTable { get; set; }
    }

    [Serializable]
    public class DFDataColumn
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string Rem { get; set; }
        public bool IsContinuous { get; set; }
        public string StatisticsInfo { get; set; }
        public int KindCount { get; set; }
        public string NullPercent { get; set; }

        public List<DFGroupingTag> GroupingTags { get; set; }
    }

    public class RelativeDFDataColumn : DFDataColumn
    {
        public string DistroType { get; set; }
        public string LinkFunction { get; set; }
    }

    [Serializable]
    public class DFGroupingTag
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string Index { get; set; }
    }


    [SugarTable("ST_StatisticsMethod")]
    public class StatisticsMethodModel
    {
        [SugarColumn(IsPrimaryKey = true, IsIdentity = true)]
        public int Id { get; set; }
        public string Code { get; set; }
        public string Name { get; set; }
        public string Path { get; set; }
        public string MethodName { get; set; }

        [SugarColumn(ColumnName = "Kind_Id")]
        public string KindId { get; set; }
        public bool IsDeleted { get; set; }
        [SugarColumn(IsIgnore = true)]
        public StatisticsMethodKind Kind { get; set; }
    }
    [SugarTable("ST_StatisticsMethodKind")]
    public class StatisticsMethodKind
    {
        [SugarColumn(IsPrimaryKey = true, IsIdentity = true)]
        public int Id { get; set; }
        public string Name { get; set; }
    }

}
