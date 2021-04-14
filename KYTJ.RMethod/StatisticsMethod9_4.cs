using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// SNP与表型关联分析
    /// </summary>
    public class StatisticsMethod9_4 : StatisticsMethod
    {
        public StatisticsMethod9_4()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 自变量
        /// </summary>
        public List<DFDataColumn> SelectedColumns { get; set; }
        public List<List<DFGroupingTag>> GroupingTags { get; set; }
        /// <summary>
        /// 表形变量
        /// </summary>
        public List<RelativeDFDataColumn> RelativeColumns { get; set; }
        public bool HasSNP { get; set; }
        /// <summary>
        /// 家系编号
        /// </summary>
        public DFDataColumn FamilySnColumn { get; set; }
        public string GeeType { get; set; }
        public DFDataColumn TimeColumn { get; set; }
        public DFDataColumn BeginTime { get; set; }
        public DFDataColumn EndTime { get; set; }
        /// <summary>
        /// 调整变量
        /// </summary>
        public List<DFDataColumn> AdjustColumns { get; set; }
        /// <summary>
        /// 分层变量
        /// </summary>
        public DFDataColumn LayeringColumn { get; set; }
        /// <summary>
        /// 研究对象编号
        /// </summary>
        public DFDataColumn SnColumn { get; set; }


        private bool Executing( )
        {
            var res = base.WriteDataTo(this.MethodData);
            if (res)
            {
                var lines = new List<List<string>>();
                lines.Add(GetOneLineParameter(1));
                lines.Add(GetOneLineParameter(2));
                lines.Add(GetOneLineParameter(3));
                lines.Add(GetOneLineParameter(4));
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
                    p.Add(AdjustColumns.Count.ToString());
                    foreach (DFDataColumn item in AdjustColumns)
                    {
                        p.Add(item.Name);
                        p.Add(item.Rem);
                        p.Add(item.KindCount.ToString());
                    }
                    break;
                case 3:
                    p.Add(SelectedColumns.Count.ToString());
                    p.Add(HasSNP ? "1" : "0");
                    var i = 0;
                    foreach (DFDataColumn item in SelectedColumns)
                    {
                        p.Add(item.Name);
                        p.Add(item.Rem);
                        p.Add(item.KindCount.ToString());

                        var sbName = new List<string>();
                        sbName.Add(item.Name);
                        foreach (var t in GroupingTags[i])
                        {
                            sbName.Add(item.Name + "." + t.Index);
                        }
                        var sbLabel = new List<string>();
                        sbLabel.Add(item.Rem);
                        foreach (var t in GroupingTags[i])
                        {
                            sbLabel.Add(" " + t.Name);
                        }
                        p.Add(string.Join("|", sbName));
                        p.Add(string.Join("|", sbLabel));
                        i++;
                    }
                    break;
                case 4:
                    if (LayeringColumn != null)
                    {
                        p.Add(LayeringColumn.Name);
                        p.Add(LayeringColumn.Rem);
                        p.Add(LayeringColumn.KindCount.ToString());
                    }
                    if (SnColumn != null)
                    {
                        p.Add(SnColumn.Name);
                        p.Add(SnColumn.Rem);
                        p.Add(SnColumn.KindCount.ToString());
                    }
                    else
                    {
                        p.Add("NA"); p.Add("NA"); p.Add("NA");
                    }
                    if (FamilySnColumn != null)
                    {
                        p.Add(FamilySnColumn.Name);
                        p.Add(FamilySnColumn.Rem);
                        p.Add(FamilySnColumn.KindCount.ToString());
                    }
                    else
                    {
                        p.Add("NA"); p.Add("NA"); p.Add("NA");
                    }
                    if (!string.IsNullOrEmpty(GeeType)) p.Add(GeeType); else p.Add("NA");
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
            res = res && (AdjustColumns != null && AdjustColumns.Count > 0);
            return res;
        }
    }
}