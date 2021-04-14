using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 单向频数表
    /// </summary>
    public class StatisticsMethod3_1 : StatisticsMethod
    {
        public StatisticsMethod3_1()
        {
            SelectedColumns = new List<DFDataColumn>();
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        public List<DFDataColumn> SelectedColumns { get; set; }
        public string rytxt { get; set; }

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

            List<string> nameList = new List<string>();
            foreach (var item in SelectedColumns)
            {
                nameList.Add(item.Name.ToUpper() + "|" +
                    string.Join("|", item.GroupingTags.Select(c => item.Name.ToUpper() + "." + c.Index).ToArray()));
            }
            p.Add(string.Join("|", nameList.ToArray()));
            List<string> remList = new List<string>();
            foreach (var item in SelectedColumns)
            {
                remList.Add(item.Rem + "|" +
                    string.Join("|", item.GroupingTags.Select(c => " " + c.Index).ToArray()));
            }
            p.Add(string.Join("|", remList.ToArray()));

            //p.Add(string.Join(",", SelectedColumns.Select(c => c.Name.ToUpper()).ToArray()));
            //p.Add(string.Join(",", SelectedColumns.Select(c => c.Rem).ToArray()));
            p.Add(string.Join(",", SelectedColumns.Select(c =>
            {
                if (c.IsContinuous) return "0"; else return c.KindCount.ToString();
            }).ToArray()));
           
            p.Add(rytxt);
            return p;
        }



        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = (SelectedColumns != null && SelectedColumns.Count > 0);
            return res;
        }
    }
}