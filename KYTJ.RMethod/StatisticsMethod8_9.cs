using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 量大似然法因子分析(FA)
    /// </summary>
    public class StatisticsMethod8_9 : StatisticsMethod
    {
        public StatisticsMethod8_9()
        {
            SelectedColumns = new List<DFDataColumn>();
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        public List<DFDataColumn> SelectedColumns { get; set; }

        public DFDataColumn SNColumn { get; set; }

        public string Factor { get; set; }  //只能是数值

        public string Scores { get; set; }  //Scores

        public string Rotation { get; set; }    //rotation

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
            if (!string.IsNullOrEmpty(Factor)) p.Add(Factor); else p.Add("NA");
            if (!string.IsNullOrEmpty(Scores)) p.Add(Scores); else p.Add("NA");
            if (!string.IsNullOrEmpty(Rotation)) p.Add(Rotation); else p.Add("NA");
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
            res = res && (SNColumn != null);
            res = res && (!string.IsNullOrEmpty(Scores));
            return res;
        }


    }
}