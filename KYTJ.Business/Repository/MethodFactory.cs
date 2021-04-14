using KYTJ.Model;
using KYTJ.RMethod;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace KYTJ.Business.Repository
{
    public class MethodFactory
    {
        public static StatisticsMethod Build(DataFlowCacheModel vm, string code, string param)
        {
            StatisticsMethod obj = null;
            var columns = vm.DataColumns;
            switch (code)
            {
                case "1_1":
                    obj = Create11(columns, JsonConvert.DeserializeObject<M1_1VM>(param));
                    break;
                case "1_2":
                    obj = Create12(columns, JsonConvert.DeserializeObject<M1_2VM>(param));
                    break;
                case "2_1":
                    obj = Create21(columns, JsonConvert.DeserializeObject<M2_1VM>(param));
                    break;
                case "2_2":
                    obj = Create22(columns, JsonConvert.DeserializeObject<M2_2VM>(param));
                    break;
                case "2_3":
                    obj = Create23(columns, JsonConvert.DeserializeObject<M2_3VM>(param));
                    break;
                case "2_4":
                    obj = Create24(columns, JsonConvert.DeserializeObject<M2_4VM>(param));
                    break;
                case "3_1":
                    obj = Create31(columns, JsonConvert.DeserializeObject<M3_1VM>(param));
                    break;
                case "3_2":
                    obj = Create32(columns, JsonConvert.DeserializeObject<M3_2VM>(param));
                    break;
                case "4_1":
                    obj = Create41(columns, JsonConvert.DeserializeObject<M4_1VM>(param));
                    break;
                case "4_2":
                    obj = Create42(columns, JsonConvert.DeserializeObject<M4_2VM>(param));
                    break;
                case "4_3":
                    obj = Create43(columns, JsonConvert.DeserializeObject<M4_3VM>(param));
                    break;
                default:
                    break;
            }

            return obj;
        }

        #region 1_1 正态性检验
        private static StatisticsMethod1_1 Create11(List<DFDataColumn> columns, M1_1VM para)
        {
            
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            return new StatisticsMethod1_1()
            {
                SelectedColumns = selected
            };
        }
        #endregion

        #region 1_2
        private static StatisticsMethod10_1 Create12(List<DFDataColumn> columns, M1_2VM para)
        {
            StatisticsMethod10_1 methStat = new StatisticsMethod10_1();
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var grouping = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            if (para.LayeringField == 0)
                methStat.rowGroupSign = 0;
            else
            {
                methStat.rowGroupSign = 1;
                methStat.GroupingRowColumn = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
                {
                    Id = c.Id,
                    IsContinuous = c.IsContinuous,
                    KindCount = c.KindCount,
                    Name = c.Name,
                    Rem = c.Rem,
                }).FirstOrDefault();
            }

            var tag = columns.Where(c => c.Id == para.GroupingField).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            methStat.SelectedColumns = selected;
            methStat.GroupingColumn = grouping;
            methStat.GroupingTags = tag;
            methStat.scField = para.scField;
            methStat.jqField = para.jqField;
            return methStat;
        }
        #endregion

        #region 2_1 样本均数与总体均数比较的检验
        private static StatisticsMethod2_1 Create21(List<DFDataColumn> columns, M2_1VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            return new StatisticsMethod2_1()
            {
                SelectedColumns = selected,
                Average = para.Average,
            };
        }
        #endregion

        #region 2_2 两样本比较的t检验
        private static StatisticsMethod2_2 Create22(List<DFDataColumn> columns, M2_2VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var grouping = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var tag = columns.Where(c => c.Id == para.GroupingField).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            return new StatisticsMethod2_2()
            {
                SelectedColumns = selected,
                GroupingColumn = grouping,
                GroupingTags = tag,
                bzField = para.bzField,
                jyField = para.jyField
            };
        }
        #endregion

        #region 2_3 方差分析
        private static StatisticsMethod2_3 Create23(List<DFDataColumn> columns, M2_3VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var grouping = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var tag = columns.Where(c => c.Id == para.GroupingField).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            return new StatisticsMethod2_3()
            {
                SelectedColumns = selected,
                GroupingColumn = grouping,
                GroupingTags = tag,
            };
        }
        #endregion

        #region 2_4 多样本方差齐性检验
        private static StatisticsMethod2_4 Create24(List<DFDataColumn> columns, M2_4VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            StatisticsMethod2_4 methodV = new StatisticsMethod2_4();
            methodV.SelectedColumns = selected;
            methodV.jyfield = para.jyField;
            methodV.jzfield = para.jzfield;
            if (para.GroupingField > 0)
            {
                methodV.IsSelectGroup = 1;
                methodV.GroupingColumn = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
                {
                    Id = c.Id,
                    IsContinuous = c.IsContinuous,
                    KindCount = c.KindCount,
                    Name = c.Name,
                    Rem = c.Rem,
                    GroupingTags = c.GroupingTags.Select(d => new DFGroupingTag()
                    {
                        Id = d.Id,
                        Index = d.Index,
                        Name = d.Name
                    }).ToList()
                }).FirstOrDefault();
            }
            return methodV;
        }
        #endregion



        #region 3_1 单向频数表
        private static StatisticsMethod3_1 Create31(List<DFDataColumn> columns, M3_1VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
                GroupingTags = c.GroupingTags.Select(d => new DFGroupingTag()
                {
                    Id = d.Id,
                    Index = d.Index,
                    Name = d.Name
                }).ToList()
            }).ToList();
            return new StatisticsMethod3_1()
            {
                SelectedColumns = selected,
                rytxt = para.rytxt
            };
        }
        #endregion

        #region 3_2 卡方检验
        private static StatisticsMethod3_2 Create32(List<DFDataColumn> columns, M3_2VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var row = columns.Where(c => para.RowField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var col = columns.Where(c => para.ColumnField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var rowTag = columns.Where(c => c.Id == para.RowField).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            var colTag = columns.Where(c => c.Id == para.ColumnField).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            return new StatisticsMethod3_2()
            {
                ColumnAsRow = row,
                ColumnAsColumn = col,
                RowGroupingTags = rowTag,
                ColumnGroupingTags = colTag,
                jyField = para.jyField
            };
        }
        #endregion

        #region 4_1 一般线性相关系数
        private static StatisticsMethod4_1 Create41(List<DFDataColumn> columns, M4_1VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var and = columns.Where(c => para.AndFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            return new StatisticsMethod4_1()
            {
                SelectedColumns = selected,
                AndColumns = and,
            };
        }
        #endregion

        #region 4_2 序变量(Polychoric)相关系数
        private static StatisticsMethod4_2 Create42(List<DFDataColumn> columns, M4_2VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
                GroupingTags = c.GroupingTags.Select(d => new DFGroupingTag()
                {
                    Id = d.Id,
                    Index = d.Index,
                    Name = d.Name
                }).ToList()
            }).ToList();

            return new StatisticsMethod4_2()
            {
                SelectedColumns = selected,
                //AndColumns = and,
            };
        }
        #endregion

        #region 4_3 组内(Intraclass)相关系数
        private static StatisticsMethod4_3 Create43(List<DFDataColumn> columns, M4_3VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var grouping = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod4_3()
            {
                SelectedColumns = selected,
                GroupingColumn = grouping,
            };
        }
        #endregion
    }


}
