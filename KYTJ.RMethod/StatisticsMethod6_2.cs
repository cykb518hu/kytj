using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// Cox回归模型
    /// </summary>
    public class StatisticsMethod6_2 : StatisticsMethod
    {
        public StatisticsMethod6_2()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 处理重复事件方法
        /// </summary>
        public string RepeatHandler { get; set; }
        /// <summary>
        /// 结果变量
        /// </summary>
        public DFDataColumn ResultColumn { get; set; }
        /// <summary>
        /// 时间变量
        /// </summary>
        public DFDataColumn TimeColumn { get; set; }
        /// <summary>
        /// 模型中的分层变量
        /// </summary>
        public DFDataColumn StrataColumn { get; set; }
        /// <summary>
        /// 分层变量
        /// </summary>
        public DFDataColumn LayeringColumn { get; set; }
        /// <summary>
        /// 或 开始时间
        /// </summary>
        public DFDataColumn BeginTimeColumn { get; set; }


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
            p.Add(RepeatHandler);
            if (ResultColumn != null)
            {
                p.Add(ResultColumn.Name);
                p.Add(ResultColumn.Rem);
                p.Add(ResultColumn.IsContinuous ? "0" : ResultColumn.KindCount.ToString());
            }
            if (TimeColumn != null)
            {
                p.Add(TimeColumn.Name);
                p.Add(TimeColumn.Rem);
                p.Add(TimeColumn.IsContinuous ? "0" : TimeColumn.KindCount.ToString());
            }
            if (BeginTimeColumn != null)
            {
                p.Add(BeginTimeColumn.Name);
                p.Add(BeginTimeColumn.Rem);
                p.Add(BeginTimeColumn.IsContinuous ? "0" : BeginTimeColumn.KindCount.ToString());
            }
            if (StrataColumn != null)
            {
                p.Add(StrataColumn.Name);
                p.Add(StrataColumn.Rem);
                p.Add(StrataColumn.IsContinuous ? "0" : StrataColumn.KindCount.ToString());
            }
            if (LayeringColumn != null)
            {
                p.Add(LayeringColumn.Name);
                p.Add(LayeringColumn.Rem);
                p.Add(LayeringColumn.IsContinuous ? "0" : LayeringColumn.KindCount.ToString());
            }
            if (SelectedColumns != null && SelectedColumns.Count>0)
            {
                foreach (var item in SelectedColumns)
                {
                    p.Add(item.Name);
                    p.Add(item.Rem);
                    p.Add(item.IsContinuous ? "0" : item.KindCount.ToString());
                }
            }
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (ResultColumn != null);
            res = res && (TimeColumn != null);
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (!string.IsNullOrEmpty(RepeatHandler));
            return res;
        }


    }
}