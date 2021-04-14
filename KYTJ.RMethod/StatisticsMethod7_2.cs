using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 非参数方差分析
    /// </summary>
    public class StatisticsMethod7_2 : StatisticsMethod
    {
        public StatisticsMethod7_2()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 分组变量
        /// </summary>
        public DFDataColumn GroupingColumn { get; set; }
        /// <summary>
        /// 分层变量
        /// </summary>
        public DFDataColumn LayeringColumn { get; set; }


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
            if (GroupingColumn != null)
            {
                p.Add(GroupingColumn.Name);
                p.Add(GroupingColumn.Rem);
                p.Add(GroupingColumn.IsContinuous ? "0" : GroupingColumn.KindCount.ToString());
            }
            if (LayeringColumn != null)
            {
                p.Add(LayeringColumn.Name);
                p.Add(LayeringColumn.Rem);
                p.Add(LayeringColumn.IsContinuous ? "0" : LayeringColumn.KindCount.ToString());
            }
            if (SelectedColumns != null && SelectedColumns.Count > 0)
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
            res = res && (GroupingColumn != null);
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            return res;
        }

    }
}