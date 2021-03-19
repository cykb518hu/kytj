using Newtonsoft.Json;
using SqlSugar;
using System;
using System.Collections.Generic;
using System.Text;

namespace KYTJ.Model
{
    [SugarTable("SM_Diseases")]
    public class ProjectModel
    {
        [SugarColumn(IsPrimaryKey = true, IsIdentity = true)]
        public int Id { get; set; }


        [SugarColumn(ColumnName = "DiseaseName")]
        public string ProjectName { get; set; }

        [SugarColumn(ColumnName = "DiseaseDescribe")]
        public string ProjectDesc { get; set; }
        public DateTime CreateTime { get; set; }
        public DateTime UpdateTime { get; set; }

        [JsonIgnore]
        public int IsDeleted { get; set; }
        public string UserName { get; set; }

        [JsonIgnore]
        public int DiseaseType { get; set; }

        [JsonIgnore]
        public int DeptID { get; set; }

        [SugarColumn(IsIgnore = true)]
        public List<DataSetModel> DataSetList { get; set; }
    }
}
