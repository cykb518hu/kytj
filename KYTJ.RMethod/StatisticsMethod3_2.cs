using KYTJ.Model;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// 卡方检验
    /// </summary>
    public class StatisticsMethod3_2 : StatisticsMethod
    {
        public StatisticsMethod3_2()
        {
            base.BeforeExecute = this.IsValid;
            base.OnExecuting = this.Executing;
        }

        /// <summary>
        /// 行标签
        /// </summary>
        public DFDataColumn ColumnAsRow { get; set; }
        /// <summary>
        /// 列标签
        /// </summary>
        public DFDataColumn ColumnAsColumn { get; set; }
        /// <summary>
        /// 检验方法
        /// </summary>
        public int jyField { get; set; }
        /// <summary>
        /// 行分组标签
        /// </summary>
        public List<DFGroupingTag> RowGroupingTags { get; set; }
        /// <summary>
        /// 列分组标签
        /// </summary>
        public List<DFGroupingTag> ColumnGroupingTags { get; set; }

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

            //p.Add(GroupingColumn.Name.ToUpper());
            //p.Add(GroupingColumn.Rem);
            //p.Add(GroupingColumn.IsContinuous ? "0" : GroupingColumn.KindCount.ToString());
            string colNameStr = ColumnAsColumn.Name.ToUpper() + "|" +
                string.Join("|", ColumnGroupingTags.Select(c => { return ColumnAsColumn.Name.ToUpper() + "." + c.Index; }).ToArray());
            p.Add(colNameStr);
            string colRemStr = ColumnAsColumn.Rem + "|" +
                string.Join("|", ColumnGroupingTags.Select(c => { return " "+c.Index; }).ToArray());
            p.Add(colRemStr);
            p.Add(ColumnAsColumn.KindCount.ToString());
            string rowNameStr = ColumnAsRow.Name.ToUpper() + "|" +
                string.Join("|", RowGroupingTags.Select(c => { return ColumnAsRow.Name.ToUpper() + "." + c.Index; }).ToArray());
            p.Add(rowNameStr);
            string rowRemStr = ColumnAsRow.Rem + "|" +
               string.Join("|", RowGroupingTags.Select(c => { return " " + c.Index; }).ToArray());
            p.Add(rowRemStr);
            p.Add(ColumnAsRow.KindCount.ToString());
            p.Add(jyField.ToString());
            return p;
        }
        private bool IsValid()
        {
            var res = true;
            res = res && base.DataValid();
            res = res && (ColumnAsColumn != null && ColumnAsColumn.IsContinuous == false);
            res = res && (ColumnAsRow != null && ColumnAsRow.IsContinuous == false);
            return res;
        }
    }
}