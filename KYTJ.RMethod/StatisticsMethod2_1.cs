using System.Linq;
using System.Collections.Generic;
using KYTJ.Model;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 样本均数与总体均数比较的检验
    /// </summary>
    public class StatisticsMethod2_1 : StatisticsMethod
    {
        public StatisticsMethod2_1()
        {
            SelectedColumns = new List<DFDataColumn>();
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        public List<DFDataColumn> SelectedColumns { get; set; }
        public decimal? Average { get; set; }

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
            p.Add(Average == null ? string.Empty : Average.ToString());
            return p;
        }
        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = (SelectedColumns != null && SelectedColumns.Count > 0);
            return res;
        }
    }
}