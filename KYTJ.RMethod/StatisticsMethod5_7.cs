using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 多分类Logistic回归分析
    /// </summary>
    public class StatisticsMethod5_7 : StatisticsMethod
    {
        public StatisticsMethod5_7()
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
        /// 分层变量
        /// </summary>
        public DFDataColumn FCColumn { get; set; }
        public int fcSign { get; set; }

        private bool Executing()
        {
            var res = base.WriteDataTo(this.MethodData);
            if (res)
            {
                var parameter = GetParameterReady();
                res = WriteParamNew(parameter);
                //var parameter = GetOneLineParameter();
                //res = WriteParametersTo(new List<string>() { string.Join(",", parameter.ToArray()) }, parameter.Count, output);
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
           
            p.Add(fcSign.ToString());
            p.Add(FCColumn.Name.ToUpper());
            p.Add(FCColumn.Rem);
            return p;
        }
        private List<string> GetOneLineParameter()
        {
            List<string> p = new List<string>();
            p.Add(SelectedColumns.Count.ToString());
            if (ResultColumn != null)
            {
                p.Add(ResultColumn.Name);
                p.Add(ResultColumn.Rem);
                p.Add(ResultColumn.KindCount.ToString());
            }
            foreach (DFDataColumn item in SelectedColumns)
            {
                p.Add(item.Name);
                p.Add(item.Rem);
                p.Add(item.KindCount.ToString());
            }
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (ResultColumn != null);
            return res;
        }
    }
}