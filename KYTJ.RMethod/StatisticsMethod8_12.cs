using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 多重对应分析(MCA)
    /// </summary>
    public class StatisticsMethod8_12 : StatisticsMethod
    {
        public StatisticsMethod8_12()
        {
            SelectedColumns = new List<DFDataColumn>();
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }
        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 自变量分组列标签
        /// </summary>
        public List<List<DFGroupingTag>> SelectedGroupingTags { get; set; }
        /// <summary>
        /// 编号
        /// </summary>
        public DFDataColumn SNColumn { get; set; }


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
            if (SNColumn != null)
            {
                p.Add(SNColumn.Name);
                p.Add(SNColumn.Rem);
                p.Add(SNColumn.IsContinuous ? "0" : SNColumn.KindCount.ToString());
            }
            var i = 0;
            foreach (var item in SelectedColumns)
            {
                p.Add(item.Name);
                p.Add(item.Rem);
                p.Add(item.IsContinuous ? "0" : item.KindCount.ToString());

                var sbName = new List<string>();
                sbName.Add(item.Name);
                foreach (var t in SelectedGroupingTags[i])
                {
                    sbName.Add(item.Name + "." + t.Index);
                }
                var sbLabel = new List<string>();
                sbLabel.Add(item.Rem);
                foreach (var t in SelectedGroupingTags[i])
                {
                    sbLabel.Add(" " + t.Name);
                }
                p.Add(string.Join("|", sbName));
                p.Add(string.Join("|", sbLabel));
            }
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (SNColumn != null);
            return res;
        }

 
    }
}