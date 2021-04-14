using System.Linq;
using System.Collections.Generic;
using KYTJ.Model;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 两样本比较的t检验
    /// </summary>
    public class StatisticsMethod10_1 : StatisticsMethod
    {
        public StatisticsMethod10_1()
        {
            SelectedColumns = new List<DFDataColumn>();
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 分组参数
        /// </summary>
        public DFDataColumn GroupingColumn { get; set; }
        public DFDataColumn GroupingRowColumn { get; set; }
        public int scField { get; set; }
        public int jqField { get; set; }
        public int rowGroupSign { get; set; }

        /// <summary>
        /// 分组列标签
        /// </summary>
        public List<DFGroupingTag> GroupingTags { get; set; }

        private bool Executing()
        {
            var res = base.WriteDataTo(this.MethodData);
            if (res)
            {
                var parameter = GetParameterReady();
                res = WriteParamNew(parameter);
            }
            if (res) res = base.ExecuteMethod();
            return res;
        }
        private List<string> GetParameterReady()
        {
            List<string> p = new List<string>();
            p.Add(string.Join(",", SelectedColumns.Select(c => c.Name.ToUpper()).ToArray()));
            p.Add(string.Join(",", SelectedColumns.Select(c => c.Rem).ToArray()));
            p.Add(string.Join(",", SelectedColumns.Select(c =>
            {
                if (c.IsContinuous) return "0"; else return c.KindCount.ToString();
            }).ToArray()));

          
            p.Add(GroupingColumn.Name.ToUpper());
            p.Add(GroupingColumn.Rem);
            p.Add(scField.ToString());
            p.Add(jqField.ToString());
            p.Add(rowGroupSign.ToString());
            if(rowGroupSign==1)
            {
                p.Add(GroupingRowColumn.Name.ToUpper());
                p.Add(GroupingRowColumn.Rem);
            }else
            {
                p.Add("NA");
                p.Add("NA");
            }
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (GroupingColumn != null && GroupingColumn.IsContinuous == false && GroupingColumn.KindCount == 2);
            return res;
        }
    }
}