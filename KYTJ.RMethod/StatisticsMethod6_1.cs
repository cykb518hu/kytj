using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// Kaplan Meier生存曲线
    /// </summary>
    public class StatisticsMethod6_1 : StatisticsMethod
    {
        public StatisticsMethod6_1()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }
        /// <summary>
        /// 结果变量
        /// </summary>
        public DFDataColumn ResultColumn { get; set; }
        /// <summary>
        /// 时间变量
        /// </summary>
        public DFDataColumn TimeColumn { get; set; }
        /// <summary>
        /// 分组变量
        /// </summary>
        public DFDataColumn GroupingColumn { get; set; }
        /// <summary>
        /// 或 开始时间
        /// </summary>
        public DFDataColumn BeginTimeColumn { get; set; }

        public int TimePoints { get; set; }

        public int LandmarkAnalysisTimeCuts { get; set; }


        private bool Executing()
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
            if (GroupingColumn != null)
            {
                p.Add(GroupingColumn.Name);
                p.Add(GroupingColumn.Rem);
                p.Add(GroupingColumn.IsContinuous ? "0" : GroupingColumn.KindCount.ToString());
            }
            if (BeginTimeColumn != null)
            {
                p.Add(BeginTimeColumn.Name);
                p.Add(BeginTimeColumn.Rem);
                p.Add(BeginTimeColumn.IsContinuous ? "0" : BeginTimeColumn.KindCount.ToString());
            }
            if (TimePoints > 0) p.Add(TimePoints.ToString());
            if (LandmarkAnalysisTimeCuts > 0) p.Add(LandmarkAnalysisTimeCuts.ToString());
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (ResultColumn != null);
            res = res && (TimeColumn != null);
            res = res && (GroupingColumn != null);
            return res;
        }

    }
}