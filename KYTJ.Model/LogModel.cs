using Newtonsoft.Json;
using SqlSugar;
using System;
using System.Collections.Generic;
using System.Text;

namespace KYTJ.Model
{
    [SugarTable("ActionLog")]
    public class LogModel
    {
        public string ProjectName { get; set; }

        [JsonIgnore]
        [SugarColumn(ColumnName = "sLevel")]
        public string Level { get; set; }

        [JsonIgnore]
        [SugarColumn(ColumnName = "sMessage")]
        public string Message { get; set; }
        public string SourceName { get; set; }
        public DateTime DtDate { get; set; }
        public string Account { get; set; }

        [SugarColumn(ColumnName = "OptionNode")]
        public string Action { get; set; }
        public string ClientIp { get; set; }

        [JsonIgnore]
        [SugarColumn(ColumnName = "sThread")]
        public string Thread { get; set; }
    }
}
