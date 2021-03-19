using Newtonsoft.Json;
using SqlSugar;
using System;
using System.Collections.Generic;
using System.Text;

namespace KYTJ.Model
{
    [SugarTable("disease_export")]
    public class SearchEngineDataModel
    {
        public int Id { get; set; }

        [SugarColumn(ColumnName = "disease_id")]
        public string DiseaseId { get; set; }

        [SugarColumn(ColumnName = "data_name")]
        public string DataName { get; set; }

        public DateTime CreateTime { get; set; }

        [SugarColumn(IsIgnore =true)]
        public string ExtractTime { get; set; }

        public int Status { get; set; }

        [JsonIgnore]
        public int IsDeleted { get; set; }

        [SugarColumn(ColumnName = "createUserCode")]
        public string UserName { get; set; }

        [JsonIgnore]
        public string DataType { get; set; }
    }


    [SugarTable("T_EngineMapping")]
    public class EngineMappingModel
    {

        [SugarColumn(ColumnName = "export_id")]
        public int Id { get; set; }

        public DateTime LastUpdateTime { get; set; }

        [SugarColumn(ColumnName = "data_name")]
        public string DataName { get; set; }
    }

    public class ExtractDataSetMappingModel
    {
        public int TableId { get; set; }
        public int DataSetId { get; set; }

        public int ResultDataId { get; set; }
    }

    public class ExtractDataDetailModel
    {
        public string DimensionValue { get; set; }

        public string Exportkey { get; set; }
        public string ExportDataTableCN { get; set; }
        public string ExportValue { get; set; }
    }

  
}
