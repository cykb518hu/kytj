using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 条件Logistic回归
    /// </summary>
    public class StatisticsMethod5_5 : StatisticsMethod
    {
        public StatisticsMethod5_5()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 
        /// </summary>
        public DFDataColumn ResultColumn { get; set; }
        /// <summary>
        /// 
        /// </summary>
        public DFDataColumn SNColumn { get; set; }
        public int jyField { get; set; }

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
            p.Add(ResultColumn.Name.ToUpper());
            p.Add(ResultColumn.Rem);
            p.Add(string.Join(",", SelectedColumns.Select(c => c.Name.ToUpper()).ToArray()));
            p.Add(string.Join(",", SelectedColumns.Select(c => c.Rem).ToArray()));
            p.Add(string.Join(",", SelectedColumns.Select(c => c.KindCount).ToArray()));

            p.Add(jyField.ToString());
            p.Add(SNColumn.Name.ToUpper());
            p.Add(SNColumn.Rem);
           
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (ResultColumn != null);
            res = res && (SNColumn != null);
            return res;
        }
    }
}