
using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 多样本方差齐性检验
    /// </summary>
    public class StatisticsMethod2_4 : StatisticsMethod
    {
        public StatisticsMethod2_4()
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
        ///// <summary>
        ///// 分层参数
        ///// </summary>
        //public Tuple<string, string, int> LayeringColumn { get; set; }
        /// <summary>
        /// 分组列标签
        /// </summary>
        public List<DFGroupingTag> GroupingTags { get; set; }
        public int IsSelectGroup { get; set; }

        public int jyfield { get; set; }
        public int jzfield { get; set; }


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
            p.Add(jyfield.ToString());
            p.Add(jzfield.ToString());
            if (IsSelectGroup > 0)
            {
                p.Add("1");
                p.Add(GroupingColumn.Name.ToUpper() + "|" +
                    string.Join("|", GroupingColumn.GroupingTags.Select(c => GroupingColumn.Name.ToUpper() + "." + c.Index).ToArray()));
                //p.Add(GroupingColumn.Name.ToUpper());
                p.Add(GroupingColumn.Rem + "|" +
                    string.Join("|", GroupingColumn.GroupingTags.Select(c => " " + c.Index).ToArray()));
                //p.Add(GroupingColumn.Rem.ToUpper());
            }
            else p.Add("0");

            //var sbName = new List<string>();
            //sbName.Add(GroupingColumn.Name.ToUpper());
            //foreach (var item in GroupingTags)
            //{
            //    sbName.Add(GroupingColumn.Name + "." + item.Index);
            //}
            //var sbLabel = new List<string>();
            //sbLabel.Add(GroupingColumn.Rem);
            //foreach (var item in GroupingTags)
            //{
            //    sbLabel.Add(" " + item.Name);
            //}
            //p.Add(string.Join("|", sbName));
            //p.Add(string.Join("|", sbLabel));
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            //res = res && (GroupingColumn != null && GroupingColumn.IsContinuous == false && GroupingColumn.KindCount > 2);
            return res;
        }

    }
}