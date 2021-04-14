using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 序变量(Polychoric)相关系数
    /// </summary>
    public class StatisticsMethod4_2 : StatisticsMethod
    {
        public StatisticsMethod4_2()
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
        public List<DFDataColumn> AndColumns { get; set; }

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
            //p.Add(string.Join(",", SelectedColumns.Select(c => c.Name.ToUpper()).ToArray()));
            List<string> nameList = new List<string>();
            foreach (var item in SelectedColumns)
            {
                nameList.Add(item.Name.ToUpper() + "|" +
                    string.Join("|", item.GroupingTags.Select(c => item.Name.ToUpper() + "." + c.Index).ToArray()));
            }
            p.Add(string.Join("|", nameList.ToArray()));
            //p.Add(string.Join(",", SelectedColumns.Select(c => c.Rem).ToArray()));
            List<string> remList = new List<string>();
            foreach (var item in SelectedColumns)
            {
                remList.Add(item.Rem + "|" +
                    string.Join("|", item.GroupingTags.Select(c => " " + c.Index).ToArray()));
            }
            p.Add(string.Join("|", remList.ToArray()));

            p.Add(string.Join(",", SelectedColumns.Select(c =>
            {
                if (c.IsContinuous) return "0"; else return c.KindCount.ToString();
            }).ToArray()));
            p.Add(string.Join(",", SelectedColumns.Select(c => c.Name.ToUpper()).ToArray()));
            return p;
        }


        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count == 2);
            return res;
        }
    }
}