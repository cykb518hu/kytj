using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 多位点单倍体型分数检验
    /// </summary>
    public class StatisticsMethod9_3 : StatisticsMethod
    {
        public StatisticsMethod9_3()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 表形变量
        /// </summary>
        public List<RelativeDFDataColumn> RelativeColumns { get; set; }
        /// <summary>
        /// 自变量类型
        /// </summary>
        public string ColumnType { get; set; }
        /// <summary>
        /// 调整变量
        /// </summary>
        public List<DFDataColumn> AdjustColumns { get; set; }
        /// <summary>
        /// 分层变量
        /// </summary>
        public DFDataColumn LayeringColumn { get; set; }
        /// <summary>
        /// 是否模拟P值
        /// </summary>
        public bool? SimulationPValue { get; set; }



        private bool Executing( )
        {
            var res = base.WriteDataTo(this.MethodData);
            if (res)
            {
                var parameter = GetOneLineParameter();
                res = WriteParametersTo(new List<string>() { string.Join(",", parameter.ToArray()) }, parameter.Count);
            }
            if (res) res = base.ExecuteMethod();
            return res;
        }

        private List<string> GetOneLineParameter()
        {
            List<string> p = new List<string>();
            p.Add(SelectedColumns.Count.ToString());
            if (!string.IsNullOrEmpty(ColumnType)) p.Add(ColumnType); else p.Add("NA");
            if (RelativeColumns != null && RelativeColumns.Count > 0)
            {
                foreach (var item in RelativeColumns)
                {
                    p.Add(item.Name);
                    p.Add(item.Rem);
                    p.Add(item.KindCount.ToString());
                    p.Add(item.DistroType);
                    p.Add(item.LinkFunction);
                }
            }
            if (AdjustColumns != null && AdjustColumns.Count > 0)
            {
                foreach (var item in AdjustColumns)
                {
                    p.Add(item.Name);
                    p.Add(item.Rem);
                    p.Add(item.KindCount.ToString());
                }
            }
            if (LayeringColumn != null)
            {
                p.Add(LayeringColumn.Name);
                p.Add(LayeringColumn.Rem);
                p.Add(LayeringColumn.KindCount.ToString());
            }
            if (SimulationPValue != null && SimulationPValue.Value) p.Add("true");
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
            res = res && (RelativeColumns != null && RelativeColumns.Count > 0);
            res = res && (!string.IsNullOrEmpty(ColumnType));
            return res;
        }
    }
}