
using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 多元Hotelling T平方检验
    /// </summary>
    public class StatisticsMethod8_3 : StatisticsMethod
    {
        public StatisticsMethod8_3()
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
        /// <summary>
        /// 分组列标签
        /// </summary>
        public List<DFGroupingTag> GroupingTags { get; set; }
        /// <summary>
        /// 分层参数
        /// </summary>
        public DFDataColumn LayeringColumn { get; set; }
        /// <summary>
        /// 分层列标签
        /// </summary>
        public List<DFGroupingTag> LayeringTags { get; set; }


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
            var sbName = new List<string>();
            sbName.Add(GroupingColumn.Name);
            foreach (var item in GroupingTags)
            {
                sbName.Add(GroupingColumn.Name + "." + item.Index);
            }
            var sbLabel = new List<string>();
            sbLabel.Add(GroupingColumn.Rem);
            foreach (var item in GroupingTags)
            {
                sbLabel.Add(" " + item.Name);
            }
            p.Add(string.Join("|", sbName));
            p.Add(string.Join("|", sbLabel));

            if (LayeringColumn != null)
            {
                p.Add(LayeringColumn.Name);
                p.Add(LayeringColumn.Rem);
                p.Add(LayeringColumn.IsContinuous ? "0" : LayeringColumn.KindCount.ToString());
            }
            foreach (var item in SelectedColumns)
            {
                p.Add(item.Name);
                p.Add(item.Rem);
                p.Add(item.IsContinuous ? "0" : item.KindCount.ToString());
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