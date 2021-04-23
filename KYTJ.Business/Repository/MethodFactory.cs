using KYTJ.Model;
using KYTJ.RMethod;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace KYTJ.Business.Repository
{
    public class CaculateMethodFactory
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
                case "5_1":
                    obj = Create51(columns, JsonConvert.DeserializeObject<M5_1VM>(param));
                    break;
                case "5_2":
                    obj = Create52(columns, JsonConvert.DeserializeObject<M5_2VM>(param));
                    break;
                case "5_3":
                    obj = Create53(columns, JsonConvert.DeserializeObject<M5_3VM>(param));
                    break;
                case "5_4":
                    obj = Create54(columns, JsonConvert.DeserializeObject<M5_4VM>(param));
                    break;
                case "5_5":
                    obj = Create55(columns, JsonConvert.DeserializeObject<M5_5VM>(param));
                    break;
                case "5_6":
                    obj = Create56(columns, JsonConvert.DeserializeObject<M5_6VM>(param));
                    break;
                case "5_7":
                    obj = Create57(columns, JsonConvert.DeserializeObject<M5_7VM>(param));
                    break;
                case "5_8":
                    obj = Create58(columns, JsonConvert.DeserializeObject<M5_8VM>(param));
                    break;
                case "5_9":
                    obj = Create59(columns, JsonConvert.DeserializeObject<M5_9VM>(param));
                    break;
                case "5_10":
                    obj = Create510(columns, JsonConvert.DeserializeObject<M5_10VM>(param));
                    break;
                case "6_1":
                    obj = Create61(columns, JsonConvert.DeserializeObject<M6_1VM>(param));
                    break;
                case "6_2":
                    obj = Create62(columns, JsonConvert.DeserializeObject<M6_2VM>(param));
                    break;
                case "6_3":
                    obj = Create63(columns, JsonConvert.DeserializeObject<M6_3VM>(param));
                    break;
                case "7_1":
                    obj = Create71(columns, JsonConvert.DeserializeObject<M7_1VM>(param));
                    break;
                case "7_2":
                    obj = Create72(columns, JsonConvert.DeserializeObject<M7_2VM>(param));
                    break;
                case "7_3":
                    obj = Create73(columns, JsonConvert.DeserializeObject<M7_3VM>(param));
                    break;
                case "8_1":
                    obj = Create81(columns, JsonConvert.DeserializeObject<M8_1VM>(param));
                    break;
                case "8_2":
                    obj = Create82(columns, JsonConvert.DeserializeObject<M8_2VM>(param));
                    break;
                case "8_3":
                    obj = Create83(columns, JsonConvert.DeserializeObject<M8_3VM>(param));
                    break;
                case "8_4":
                    obj = Create84(columns, JsonConvert.DeserializeObject<M8_4VM>(param));
                    break;
                case "8_5":
                    obj = Create85(columns, JsonConvert.DeserializeObject<M8_5VM>(param));
                    break;
                case "8_6":
                    obj = Create86(columns, JsonConvert.DeserializeObject<M8_6VM>(param));
                    break;
                case "8_8":
                    obj = Create88(columns, JsonConvert.DeserializeObject<M8_8VM>(param));
                    break;
                case "8_9":
                    obj = Create89(columns, JsonConvert.DeserializeObject<M8_9VM>(param));
                    break;
                case "8_10":
                    obj = Create810(columns, JsonConvert.DeserializeObject<M8_10VM>(param));
                    break;
                case "8_11":
                    obj = Create811(columns, JsonConvert.DeserializeObject<M8_11VM>(param));
                    break;
                case "8_12":
                    obj = Create812(columns, JsonConvert.DeserializeObject<M8_12VM>(param));
                    break;
                case "9_1":
                    obj = Create91(columns, JsonConvert.DeserializeObject<M9_1VM>(param));
                    break;
                case "9_2":
                    obj = Create92(columns, JsonConvert.DeserializeObject<M9_2VM>(param));
                    break;
                case "9_3":
                    obj = Create93(columns, JsonConvert.DeserializeObject<M9_3VM>(param));
                    break;
                case "9_4":
                    obj = Create94(columns, JsonConvert.DeserializeObject<M9_4VM>(param));
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



        #region 5_1 广义线性模型
        private static StatisticsMethod5_1 Create51(List<DFDataColumn> columns, M5_1VM para)
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
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous,
                KindCount = c.KindCount,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            return new StatisticsMethod5_1()
            {
                SelectedColumns = selected,
                RelativeColumns = relative,
                jyField = para.jyField
            };
        }
        #endregion

        #region 5_2 广义相加模型
        private static StatisticsMethod5_2 Create52(List<DFDataColumn> columns, M5_2VM para)
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
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            var curve = columns.Where(c => para.CurveFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            return new StatisticsMethod5_2()
            {
                SelectedColumns = selected,
                RelativeColumns = relative,
                CurveColumns = curve,
            };
        }
        #endregion

        #region 5_3 广义估计方程(GEE)
        private static StatisticsMethod5_3 Create53(List<DFDataColumn> columns, M5_3VM para)
        {
            StatisticsMethod5_3 statsMe = new StatisticsMethod5_3();
            if ((columns == null && columns.Count <= 0) || para == null) return null;
            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            if (para.flField == 0)
            {
                statsMe.FLSign = 0;
                statsMe.FLColumn = new DFDataColumn { Name = "NA", Rem = "NA" };
            }
            else
            {
                statsMe.FLSign = 1;
                statsMe.FLColumn = columns.Where(c => para.flField == c.Id).Select(c => new DFDataColumn()
                {
                    Id = c.Id,
                    IsContinuous = c.IsContinuous ,
                    KindCount = c.KindCount ,
                    Name = c.Name,
                    Rem = c.Rem,
                }).FirstOrDefault();
            }

            var gee = columns.Where(c => para.GeeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            statsMe.SelectedColumns = selected;
            statsMe.RelativeColumns = relative;
            statsMe.GEEColumn = gee;
            return statsMe;

        }
        #endregion

        #region 5_4 广义相加混合模型
        private static StatisticsMethod5_4 Create54(List<DFDataColumn> columns, M5_4VM para)
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
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            var curve = columns.Where(c => para.CurveFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var sn = columns.Where(c => para.SNField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod5_4()
            {
                SelectedColumns = selected,
                RelativeColumns = relative,
                CurveColumns = curve,
                SNColumn = sn,
            };
        }
        #endregion

        #region 5_5 条件logisitic回归
        private static StatisticsMethod5_5 Create55(List<DFDataColumn> columns, M5_5VM para)
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
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var sn = columns.Where(c => para.SNField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod5_5()
            {
                SelectedColumns = selected,
                SNColumn = sn,
                ResultColumn = result,
                jyField = para.jyField
            };
        }
        #endregion

        #region 5_6 稳健(Robust)线性回归
        private static StatisticsMethod5_6 Create56(List<DFDataColumn> columns, M5_6VM para)
        {
            StatisticsMethod5_6 statsMe = new StatisticsMethod5_6();
            if ((columns == null && columns.Count <= 0) || para == null) return null;

            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            if (para.flField == 0)
            {
                statsMe.FLSign = 0;
                statsMe.FLFeild = new DFDataColumn { Name = "NA", Rem = "NA" };
            }
            else
            {
                statsMe.FLSign = 1;
                statsMe.FLFeild = columns.Where(c => para.flField == c.Id).Select(c => new DFDataColumn()
                {
                    Id = c.Id,
                    IsContinuous = c.IsContinuous ,
                    KindCount = c.KindCount ,
                    Name = c.Name,
                    Rem = c.Rem,
                }).FirstOrDefault();
            }
            var sn = columns.Where(c => para.SNField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            statsMe.SelectedColumns = selected;
            statsMe.RelativeColumns = relative;
            statsMe.SNFeild = sn;
            return statsMe;
        }
        #endregion

        #region 5_7 多分类logisitic回归
        private static StatisticsMethod5_7 Create57(List<DFDataColumn> columns, M5_7VM para)
        {

            StatisticsMethod5_7 statsMe = new StatisticsMethod5_7();
            if ((columns == null && columns.Count <= 0) || para == null) return null;

            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            if (para.FCField == 0)
            {
                statsMe.fcSign = 0;
                statsMe.FCColumn = new DFDataColumn { Name = "NA", Rem = "NA" };
            }
            else
            {
                statsMe.fcSign = 1;
                statsMe.FCColumn = columns.Where(c => para.FCField == c.Id).Select(c => new DFDataColumn()
                {
                    Id = c.Id,
                    IsContinuous = c.IsContinuous ,
                    KindCount = c.KindCount ,
                    Name = c.Name,
                    Rem = c.Rem,
                }).FirstOrDefault();
            }
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            statsMe.SelectedColumns = selected;
            statsMe.ResultColumn = result;
            return statsMe;

        }
        #endregion

        #region 5_8 线性或二次判别分析
        private static StatisticsMethod5_8 Create58(List<DFDataColumn> columns, M5_8VM para)
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
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod5_8()
            {
                SelectedColumns = selected,
                ResultColumn = result,
            };
        }
        #endregion

        #region 5_9 分类决策树(Recursive partition)
        private static StatisticsMethod5_9 Create59(List<DFDataColumn> columns, M5_9VM para)
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
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod5_9()
            {
                SelectedColumns = selected,
                ResultColumn = result,
                DataType_ClassMethod = para.DataType_ClassMethod,
            };
        }
        #endregion

        #region 5_10 自变量线性筛查
        private static StatisticsMethod5_10 Create510(List<DFDataColumn> columns, M5_10VM para)
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
            return new StatisticsMethod5_10()
            {
                SelectedColumns = selected,
                FilterValue = para.FilterValue,
            };
        }
        #endregion


        #region 6_1 Kaplan Meier生存曲线
        private static StatisticsMethod6_1 Create61(List<DFDataColumn> columns, M6_1VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;

            var grouping = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var begin = columns.Where(c => para.BeginTimeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var time = columns.Where(c => para.TimeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod6_1()
            {
                //SelectedColumns = selected,
                GroupingColumn = grouping,
                BeginTimeColumn = begin,
                LandmarkAnalysisTimeCuts = para.LandmarkAnalysisTimeCuts,
                ResultColumn = result,
                TimeColumn = time,
                TimePoints = para.TimePoints,
            };
        }
        #endregion

        #region 6_2 Cox回归模型
        private static StatisticsMethod6_2 Create62(List<DFDataColumn> columns, M6_2VM para)
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
            var begin = columns.Where(c => para.BeginTimeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var time = columns.Where(c => para.TimeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var layering = columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var strata = columns.Where(c => para.StrataField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod6_2()
            {
                SelectedColumns = selected,
                BeginTimeColumn = begin,
                ResultColumn = result,
                TimeColumn = time,
                LayeringColumn = layering,
                RepeatHandler = para.RepeatHandler,
                StrataColumn = strata,
            };
        }
        #endregion

        #region 6_3 竞争风险模型
        private static StatisticsMethod6_3 Create63(List<DFDataColumn> columns, M6_3VM para)
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
            var layering = columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var result = columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var time = columns.Where(c => para.TimeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod6_3()
            {
                SelectedColumns = selected,
                ResultColumn = result,
                TimeColumn = time,
                LayeringColumn = layering,
            };
        }
        #endregion

        #region 7_1 两样本比较的U检验
        private static StatisticsMethod7_1 Create71(List<DFDataColumn> columns, M7_1VM para)
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
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            return new StatisticsMethod7_1()
            {
                AlternativeHypothesis = para.AlternativeHypothesis,
                GroupingColumn = grouping,
                LayeringColumn = layering,
                SelectedColumns = selected,
            };
        }
        #endregion

        #region 7_2 非参数方差分析
        private static StatisticsMethod7_2 Create72(List<DFDataColumn> columns, M7_2VM para)
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
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            return new StatisticsMethod7_2()
            {
                GroupingColumn = grouping,
                LayeringColumn = layering,
                SelectedColumns = selected,
            };
        }
        #endregion

        #region 7_3 非参数曲线回归
        private static StatisticsMethod7_3 Create73(List<DFDataColumn> columns, M7_3VM para)
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
            var layering = columns.Where(c => para.GroupingField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var result = para.ResultField > 0 ? columns.Where(c => para.ResultField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            return new StatisticsMethod7_3()
            {
                ResultColumn = result,
                LayeringColumn = layering,
                SelectedColumns = selected,
            };
        }
        #endregion


        #region 8_1 多元正态性检验
        private static StatisticsMethod8_1 Create81(List<DFDataColumn> columns, M8_1VM para)
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
            return new StatisticsMethod8_1()
            {
                SelectedColumns = selected
            };
        }
        #endregion

        #region 8_2 Mahalanobis距离(异常值确定)
        private static StatisticsMethod8_2 Create82(List<DFDataColumn> columns, M8_2VM para)
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
            var sn = columns.Where(c => para.SNField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod8_2()
            {
                SelectedColumns = selected,
                SNColumn = sn,
                Average = para.Average,
            };
        }
        #endregion

        #region 8_3 多元Hotelling T平方检验
        private static StatisticsMethod8_3 Create83(List<DFDataColumn> columns, M8_3VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;

            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
                //GroupingTags = c.GroupingTags.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList(),
            }).ToList();
            var groupingCol = columns.Where(c => c.Id == para.GroupingId).FirstOrDefault();
            var layeringCol = columns.Where(c => c.Id == para.LayeringId).FirstOrDefault();
            var grouping = new DFDataColumn()
            {
                Id = groupingCol.Id,
                IsContinuous = groupingCol.IsContinuous ,
                KindCount = groupingCol.KindCount ,
                Name = groupingCol.Name,
                Rem = groupingCol.Rem,
            };
            var layering = para.LayeringId > 0 ? new DFDataColumn()
            {
                Id = layeringCol.Id,
                IsContinuous = layeringCol.IsContinuous ,
                KindCount = layeringCol.KindCount ,
                Name = layeringCol.Name,
                Rem = layeringCol.Rem,
            } : null;
            var groupingTags = groupingCol?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            var layeringTags = layeringCol?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            return new StatisticsMethod8_3()
            {
                SelectedColumns = selected,
                GroupingColumn = grouping,
                LayeringColumn = layering,
                GroupingTags = groupingTags,
                LayeringTags = layeringTags,
            };
        }
        #endregion

        #region 8_4 多元方差分析
        private static StatisticsMethod8_4 Create84(List<DFDataColumn> columns, M8_4VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;

            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
                //GroupingTags = c.GroupingTags.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList(),
            }).ToList();
            var groupingCol = columns.Where(c => c.Id == para.GroupingId).FirstOrDefault();
            var grouping2Col = columns.Where(c => c.Id == para.Grouping2Id).FirstOrDefault();
            var grouping = new DFDataColumn()
            {
                Id = groupingCol.Id,
                IsContinuous = groupingCol.IsContinuous ,
                KindCount = groupingCol.KindCount ,
                Name = groupingCol.Name,
                Rem = groupingCol.Rem,
            };
            var grouping2 = para.Grouping2Id > 0 ? new DFDataColumn()
            {
                Id = grouping2Col.Id,
                IsContinuous = grouping2Col.IsContinuous ,
                KindCount = grouping2Col.KindCount ,
                Name = grouping2Col.Name,
                Rem = grouping2Col.Rem,
            } : null;
            var groupingTags = groupingCol?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            var grouping2Tags = grouping2?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            return new StatisticsMethod8_4()
            {
                SelectedColumns = selected,
                GroupingColumn = grouping,
                GroupingIIColumn = grouping2,
                GroupingTags = groupingTags,
                GroupingIITags = grouping2Tags,
            };
        }
        #endregion

        #region 8_5 两样本多元反应曲线图比较
        private static StatisticsMethod8_5 Create85(List<DFDataColumn> columns, M8_5VM para)
        {
            if ((columns == null && columns.Count <= 0) || para == null) return null;

            var selected = columns.Where(c => para.SelectedFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
                //GroupingTags = c.GroupingTags.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList(),
            }).ToList();
            var groupingCol = columns.Where(c => c.Id == para.GroupingId).FirstOrDefault();
            var layeringCol = columns.Where(c => c.Id == para.LayeringId).FirstOrDefault();
            var grouping = new DFDataColumn()
            {
                Id = groupingCol.Id,
                IsContinuous = groupingCol.IsContinuous ,
                KindCount = groupingCol.KindCount ,
                Name = groupingCol.Name,
                Rem = groupingCol.Rem,
            };
            var layering = para.LayeringId > 0 ? new DFDataColumn()
            {
                Id = layeringCol.Id,
                IsContinuous = layeringCol.IsContinuous ,
                KindCount = layeringCol.KindCount ,
                Name = layeringCol.Name,
                Rem = layeringCol.Rem,
            } : null;
            var groupingTags = groupingCol?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            var layeringTags = layeringCol?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            return new StatisticsMethod8_5()
            {
                SelectedColumns = selected,
                GroupingColumn = grouping,
                LayeringColumn = layering,
                GroupingTags = groupingTags,
                LayeringTags = layeringTags,
            };
        }
        #endregion

        #region 8_6 广义估计方程多应变量回归
        private static StatisticsMethod8_6 Create86(List<DFDataColumn> columns, M8_6VM para)
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
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var exposed = columns.Where(c => para.ExposedField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var tag = columns.Where(c => c.Id == para.ExposedField).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList();
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }

            return new StatisticsMethod8_6()
            {
                SelectedColumns = selected,
                RelativeColumns = relative,
                ExposedColumn = exposed,
                ExposedGroupingTags = tag,
                LayeringColumn = layering,
                ColumnRelation = para.FieldRelation.ToString(),
            };
        }
        #endregion

        #region 8_8 Scree检验(确定因子数)
        private static StatisticsMethod8_8 Create88(List<DFDataColumn> columns, M8_8VM para)
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
            return new StatisticsMethod8_8()
            {
                SelectedColumns = selected
            };
        }
        #endregion

        #region 8_9 量大似然法因子分析(FA)
        private static StatisticsMethod8_9 Create89(List<DFDataColumn> columns, M8_9VM para)
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
            var sn = columns.Where(c => para.SNId == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod8_9()
            {
                SelectedColumns = selected,
                SNColumn = sn,
                Factor = para.Factor > 0 ? para.Factor.ToString() : null,
                Scores = para.Scores > 0 ? para.Scores.ToString() : null,
                Rotation = para.Rotation > 0 ? para.Rotation.ToString() : null,
            };
        }
        #endregion

        #region 8_10 验证性因子分析(CFA)
        private static StatisticsMethod8_10 Create810(List<DFDataColumn> columns, M8_10VM para)
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
            return new StatisticsMethod8_10()
            {
                SelectedColumns = selected,
                LatentNames = para.LatentNames,
            };
        }
        #endregion

        #region 8_11 主成分分析(PCA)
        private static StatisticsMethod8_11 Create811(List<DFDataColumn> columns, M8_11VM para)
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
            var sn = columns.Where(c => para.SNField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            return new StatisticsMethod8_11()
            {
                SelectedColumns = selected,
                SNColumn = sn,
            };
        }
        #endregion

        #region 8_12 多重对应分析(MCA)
        private static StatisticsMethod8_12 Create812(List<DFDataColumn> columns, M8_12VM para)
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
            var sn = columns.Where(c => para.SNField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault();
            var tags = new List<List<DFGroupingTag>>();
            selected.ForEach(s =>
            {
                tags.Add(columns.Where(c => c.Id == s.Id).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList());
            });
            return new StatisticsMethod8_12()
            {
                SelectedColumns = selected,
                SelectedGroupingTags = tags,
                SNColumn = sn,
            };
        }
        #endregion

        #region 9_1 Hardy-Weinberg平衡检验
        private static StatisticsMethod9_1 Create91(List<DFDataColumn> columns, M9_1VM para)
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
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            return new StatisticsMethod9_1()
            {
                SelectedColumns = selected,
                ColumnType = para.FieldType.ToString(),
                LayeringColumn = layering,
            };
        }
        #endregion

        #region 9_2 多位点单倍体型频率估计
        private static StatisticsMethod9_2 Create92(List<DFDataColumn> columns, M9_2VM para)
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
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            return new StatisticsMethod9_2()
            {
                SelectedColumns = selected,
                ColumnType = para.FieldType.ToString(),
                LayeringColumn = layering,
            };
        }
        #endregion

        #region 9_3 多位点单倍体型分数检验
        private static StatisticsMethod9_3 Create93(List<DFDataColumn> columns, M9_3VM para)
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
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var adjust = para.AdjustFields != null ? columns.Where(c => para.AdjustFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList() : null;
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            return new StatisticsMethod9_3()
            {
                SelectedColumns = selected,
                ColumnType = para.FieldType.ToString(),
                RelativeColumns = relative,
                AdjustColumns = adjust,
                LayeringColumn = layering,
                SimulationPValue = para.IsSimulateP == 1 ? true : false,
            };
        }
        #endregion

        #region 9_4 SNP与表型关联分析
        private static StatisticsMethod9_4 Create94(List<DFDataColumn> columns, M9_4VM para)
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
            var relative = columns.Where(c => para.RelativeFields.Contains(c.Id)).Select(c => new RelativeDFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList();
            var adjust = para.AdjustFields != null ? columns.Where(c => para.AdjustFields.Contains(c.Id)).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).ToList() : null;
            var layering = para.LayeringField > 0 ? columns.Where(c => para.LayeringField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            var family = para.FamilySnField > 0 ? columns.Where(c => para.FamilySnField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            var sn = para.SnField > 0 ? columns.Where(c => para.SnField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            var time = para.TimeField > 0 ? columns.Where(c => para.TimeField == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            var begin = para.BeginTime > 0 ? columns.Where(c => para.BeginTime == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            var end = para.EndTime > 0 ? columns.Where(c => para.EndTime == c.Id).Select(c => new DFDataColumn()
            {
                Id = c.Id,
                IsContinuous = c.IsContinuous ,
                KindCount = c.KindCount ,
                Name = c.Name,
                Rem = c.Rem,
            }).FirstOrDefault() : null;
            for (int i = 0; i < relative.Count; i++)
            {
                relative[i].DistroType = para.RelativeTypes[i];
                relative[i].LinkFunction = para.RelativeLinkFuncs[i];
            }
            var tags = new List<List<DFGroupingTag>>();
            selected.ForEach(s =>
            {
                tags.Add(columns.Where(c => c.Id == s.Id).FirstOrDefault()?.GroupingTags?.Select(g => new DFGroupingTag() { Id = g.Id, Index = g.Index, Name = g.Name, }).ToList());
            });
            return new StatisticsMethod9_4()
            {
                SelectedColumns = selected,
                RelativeColumns = relative,
                GroupingTags = tags,
                AdjustColumns = adjust,
                LayeringColumn = layering,
                FamilySnColumn = family,
                BeginTime = begin,
                EndTime = end,
                GeeType = para.GeeType,
                HasSNP = para.HasSnp,
                SnColumn = sn,
                TimeColumn = time,
            };
        }
        #endregion
    }


}
