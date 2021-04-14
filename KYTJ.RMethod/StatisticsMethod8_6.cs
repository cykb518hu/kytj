using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 广义估计方程多应变量回归
    /// </summary>
    public class StatisticsMethod8_6 : StatisticsMethod
    {
        public StatisticsMethod8_6()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        /// <summary>
        /// 应变量
        /// </summary>
        public List<RelativeDFDataColumn> RelativeColumns { get; set; }
        /// <summary>
        /// 应变量相关类型
        /// </summary>
        public string ColumnRelation { get; set; }
        /// <summary>
        /// 暴露变量
        /// </summary>
        public DFDataColumn ExposedColumn { get; set; }
        /// <summary>
        /// 暴露分组列标签
        /// </summary>
        public List<DFGroupingTag> ExposedGroupingTags { get; set; }
        /// <summary>
        /// 分层变量
        /// </summary>
        public DFDataColumn LayeringColumn { get; set; }


        private bool Executing( )
        {
            var res = base.WriteDataTo(this.MethodData);
            if (res)
            {
                var lines = new List<List<string>>();
                lines.Add(GetOneLineParameter(1));
                lines.Add(GetOneLineParameter(2));
                lines.Add(GetOneLineParameter(3));
                var max = lines.Select(p => p.Count).Max(c => c);
                foreach (var item in lines)
                {
                    for (int i = item.Count - 1; i < max; i++)
                    {
                        item.Add(string.Empty);
                    }
                }
                res = WriteParametersTo(lines.Select(l => string.Join(",", l)).ToList(), max);

             }
            if (res) res = base.ExecuteMethod();
            return res;
        }

        private List<string> GetOneLineParameter(int line)
        {
            List<string> p = new List<string>();
            p.Add(line.ToString());
            switch (line)
            {
                case 1:
                    p.Add(RelativeColumns.Count.ToString());
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
                    break;
                case 2:
                    p.Add(SelectedColumns.Count.ToString());
                    foreach (DFDataColumn item in SelectedColumns)
                    {
                        p.Add(item.Name);
                        p.Add(item.Rem);
                        p.Add(item.KindCount.ToString());
                    }
                    break;
                case 3:
                    if (!string.IsNullOrEmpty(ColumnRelation)) p.Add(ColumnRelation); else p.Add("NA");
                    if (ExposedColumn != null)
                    {
                        p.Add(ExposedColumn.Name);
                        p.Add(ExposedColumn.Rem);
                        p.Add(ExposedColumn.KindCount.ToString());

                        var sbName = new List<string>();
                        sbName.Add(ExposedColumn.Name);
                        foreach (var item in ExposedGroupingTags)
                        {
                            sbName.Add(ExposedColumn.Name + "." + item.Index);
                        }
                        var sbLabel = new List<string>();
                        sbLabel.Add(ExposedColumn.Rem);
                        foreach (var item in ExposedGroupingTags)
                        {
                            sbLabel.Add(" " + item.Name);
                        }
                        p.Add(string.Join("|", sbName));
                        p.Add(string.Join("|", sbLabel));
                    }
                    break;
                default:
                    break;
            }
            return p;
        }

        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (SelectedColumns != null && SelectedColumns.Count > 0);
            res = res && (RelativeColumns != null && RelativeColumns.Count > 0);
            res = res && (ExposedColumn != null);
            res = res && (!string.IsNullOrEmpty(ColumnRelation));
            return res;
        }
    }
}