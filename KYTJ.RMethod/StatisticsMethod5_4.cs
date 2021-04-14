using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 广义相加混合模型
    /// </summary>
    public class StatisticsMethod5_4 : StatisticsMethod
    {
        public StatisticsMethod5_4()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 与变量
        /// </summary>
        public List<RelativeDFDataColumn> RelativeColumns { get; set; }
        /// <summary>
        /// 曲线拟合自变量
        /// </summary>
        public List<DFDataColumn> CurveColumns { get; set; }
        /// <summary>
        /// 组编号
        /// </summary>
        public DFDataColumn SNColumn { get; set; }

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
            p.Add(string.Join(",", SelectedColumns.Select(c => c.KindCount).ToArray()));
            foreach (var item in RelativeColumns)
            {
                p.Add(item.DistroType);
                p.Add(item.LinkFunction);
            }
            p.Add(string.Join(",", CurveColumns.Select(c => c.Name.ToUpper()).ToArray()));
            p.Add(string.Join(",", CurveColumns.Select(c => c.Rem).ToArray()));
            p.Add(string.Join(",", CurveColumns.Select(c => c.KindCount).ToArray()));

            p.Add(string.Join(",", RelativeColumns.Select(c => c.Name.ToUpper()).ToArray()));
            p.Add(string.Join(",", RelativeColumns.Select(c => c.Rem).ToArray()));
            p.Add(string.Join(",", RelativeColumns.Select(c => c.KindCount).ToArray()));
          
            p.Add(SNColumn.Name.ToUpper());
            p.Add(SNColumn.Rem);
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (RelativeColumns != null && RelativeColumns.Count > 0);
            res = res && (CurveColumns != null && CurveColumns.Count > 0);
            res = res && (SNColumn != null);
            return res;
        }
    }
}