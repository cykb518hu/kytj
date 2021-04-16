using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.Model
{

    public class StatisticsMethodVM
    {
        public string Node { get; set; }
        public string MethodCode { get; set; }
        public string MethodPath { get; set; }

        public List<long> SelectedFields { get; set; }

        public int GroupingField { get; set; }
        public int LayeringField { get; set; }
        /// <summary>
        /// 备择假设
        /// </summary>
        public int scField { get; set; }
        /// <summary>
        /// 检验方法
        /// </summary>
        public int jqField { get; set; }

        public decimal Average { get; set; }

        public int bzField { get; set; }
        /// <summary>
        /// 检验方法
        /// </summary>
        public int jyField { get; set; }

        public int jzfield { get; set; }

        public string rytxt { get; set; }

        public long RowField { get; set; }
        /// <summary>
        /// 列标签
        /// </summary>
        public long ColumnField { get; set; }

        public List<long> AndFields { get; set; }    //与变量

        public List<long> RelativeFields { get; set; }   //应变量
        public List<string> RelativeTypes { get; set; } //应变量的类型
        public List<string> RelativeLinkFuncs { get; set; } //应变量类型的关联函数
        public List<long> CurveFields { get; set; }  //曲线拟合自变量

        public int SNField { get; set; }    //编号    

        public int GeeField { get; set; }
        public int flField { get; set; }  //分类变量
        public string GeeType { get; set; }

        public int ResultField { get; set; }    //结果变量
        public int FCField { get; set; }    //分层变量

        public int DataType_ClassMethod { get; set; }

        public decimal FilterValue { get; set; }    //过滤值VIF
        public int TimeField { get; set; }  //时间变量
        public int BeginTimeField { get; set; } //或开始时间
        public int TimePoints { get; set; }
        public int LandmarkAnalysisTimeCuts { get; set; }

        public string RepeatHandler { get; set; }   //重复事件处理方法
        public int StrataField { get; set; }    //模型中的分层变量

        public string AlternativeHypothesis { get; set; }   //备择假设

        public int GroupingId { get; set; } //分组变量
        public int LayeringId { get; set; } //分层变量

        public int Grouping2Id { get; set; } //分层II变量


        public int ExposedField { get; set; }    //暴露变量
        public int FieldRelation { get; set; }    //变量相关类型

        public int SNId { get; set; }//编号
        public int Factor { get; set; } //因子
        public int Scores { get; set; } //Scores
        public int Rotation { get; set; }   //Rotation

        public string[] LatentNames { get; set; }    //自变量对应的潜变量名

        public int FieldType { get; set; }  //变量类型

        public List<long> AdjustFields { get; set; }    //调整变量
        public int IsSimulateP { get; set; }    //是否模拟P值

        public bool HasSnp { get; set; }    //是否snp
        public int FamilySnField { get; set; }  //家系编号

        public int BeginTime { get; set; }
        public int EndTime { get; set; }
        public int SnField { get; set; }

    }

    #region 统计方法VM
    /// <summary>
    /// 1-1 正态性检验
    /// </summary>
    public class M1_1VM
    {
        /// <summary>
        /// 所选字段ID
        /// </summary>
        public List<long> SelectedFields { get; set; }
    }

    public class M2_1VM : M1_1VM
    {
        public decimal Average { get; set; }
    }

    public class M2_2VM : M1_1VM
    {
        public int GroupingField { get; set; }
        public int LayeringField { get; set; }
        /// <summary>
        /// 备择假设
        /// </summary>
        public int bzField { get; set; }
        /// <summary>
        /// 检验方法
        /// </summary>
        public int jyField { get; set; }
    }

    public class M2_3VM : M2_2VM
    {
    }

    public class M2_4VM : M2_2VM {
        
        /// <summary>
        /// 校正方法
        /// </summary>
        public int jzfield { get; set; }
    }

    public class M3_1VM : M1_1VM {
        /// <summary>
        /// 如与频数比较
        /// </summary>
        public string rytxt { get; set; }
    }

    /// <summary>
    /// 卡方
    /// </summary>
    public class M3_2VM : M1_1VM
    {
        /// <summary>
        /// 行标签
        /// </summary>
        public long RowField { get; set; }
        /// <summary>
        /// 列标签
        /// </summary>
        public long ColumnField { get; set; }
        /// <summary>
        /// 检验方法
        /// </summary>
        public int jyField { get; set; }
    }

    /// <summary>
    /// 一般线性 相关
    /// </summary>
    public class M4_1VM : M1_1VM
    {
        public List<long> AndFields { get; set; }    //与变量
    }

    /// <summary>
    /// 序变量 相关
    /// </summary>
    public class M4_2VM : M4_1VM { }

    public class M4_3VM : M1_1VM
    {
        public int GroupingField { get; set; }    //组编号变量
    }

    /// <summary>
    /// 
    /// </summary>
    public class M5_1VM : M1_1VM
    {
        public List<long> RelativeFields { get; set; }   //应变量
        public List<string> RelativeTypes { get; set; } //应变量的类型
        public List<string> RelativeLinkFuncs { get; set; } //应变量类型的关联函数
        public int jyField { get; set; }  //筛选方法
    }

    public class M5_2VM : M5_1VM
    {
        public List<long> CurveFields { get; set; }  //曲线拟合自变量
    }

    public class M5_3VM : M5_1VM
    {
        public int GeeField { get; set; }
        public int flField { get; set; }  //分类变量
        public string GeeType { get; set; }
    }

    public class M5_4VM : M5_2VM
    {
        public int SNField { get; set; }    //编号    
    }

    public class M5_5VM : M1_1VM
    {
        public int ResultField { get; set; }    //结果变量(1/0)
        public int SNField { get; set; }    //配对组号变量
        public int jyField { get; set; }   //分析方法
    }

    public class M5_6VM : M5_1VM
    {
        public int SNField { get; set; }    //配对组号变量
        public int flField { get; set; }
    }

    public class M5_7VM : M1_1VM
    {
        public int ResultField { get; set; }    //分类结果变量
        public int FCField { get; set; }    //分层变量
    }

    public class M5_8VM : M5_7VM { }

    public class M5_9VM : M5_7VM
    {
        public int DataType_ClassMethod { get; set; }
    }

    public class M5_10VM : M1_1VM
    {
        public decimal FilterValue { get; set; }    //过滤值VIF
    }

    public class M6_1VM : M1_1VM
    {
        public int ResultField { get; set; }    //结果变量
        public int TimeField { get; set; }  //时间变量
        public int GroupingField { get; set; }  //分组变量
        public int BeginTimeField { get; set; } //或开始时间
        public int TimePoints { get; set; }
        public int LandmarkAnalysisTimeCuts { get; set; }
    }

    public class M6_2VM : M1_1VM
    {
        public int ResultField { get; set; }    //结果变量
        public string RepeatHandler { get; set; }   //重复事件处理方法
        public int TimeField { get; set; }  //时间变量
        public int LayeringField { get; set; }  //分层变量
        public int BeginTimeField { get; set; } //或开始时间
        public int StrataField { get; set; }    //模型中的分层变量
    }

    public class M6_3VM : M1_1VM
    {
        public int ResultField { get; set; }    //结果变量
        public int TimeField { get; set; }  //时间变量
        public int LayeringField { get; set; }  //分层变量
    }

    /// <summary>
    /// 两样本比较的U检验
    /// </summary>
    public class M7_1VM : M7_2VM
    {
        public string AlternativeHypothesis { get; set; }   //备择假设
    }

    /// <summary>
    /// 非参数方差分析
    /// </summary>
    public class M7_2VM : M1_1VM
    {
        public int GroupingField { get; set; }  //分组变量
        public int LayeringField { get; set; }  //分层变量
    }

    /// <summary>
    /// 非参数曲线回归
    /// </summary>
    public class M7_3VM : M1_1VM
    {
        public int ResultField { get; set; }    //结果变量
        public int GroupingField { get; set; }  //分组变量
    }

    /// <summary>
    /// 多元正态性检验
    /// </summary>
    public class M8_1VM : M1_1VM { }

    public class M8_2VM : M8_1VM
    {
        public int SNField { get; set; }    //组号变量
        public string Average { get; set; } //多列，用逗号分隔
    }

    public class M8_3VM : M8_1VM
    {
        public int GroupingId { get; set; } //分组变量
        public int LayeringId { get; set; } //分层变量
    }

    public class M8_4VM : M8_1VM
    {
        public int GroupingId { get; set; } //分组变量
        public int Grouping2Id { get; set; } //分层II变量
    }

    public class M8_5VM : M8_3VM { }

    public class M8_6VM : M8_1VM
    {
        public List<long> RelativeFields { get; set; }  //应变量
        public List<string> RelativeTypes { get; set; } //应变量的类型
        public List<string> RelativeLinkFuncs { get; set; } //应变量类型的关联函数
        public int ExposedField { get; set; }    //暴露变量
        public int FieldRelation { get; set; }    //变量相关类型
        public int LayeringField { get; set; }  //分层变量
    }

    public class M8_8VM : M8_1VM { }

    public class M8_9VM : M8_1VM
    {
        public int SNId { get; set; }//编号
        public int Factor { get; set; } //因子
        public int Scores { get; set; } //Scores
        public int Rotation { get; set; }   //Rotation
    }

    public class M8_10VM : M8_1VM
    {
        public string[] LatentNames { get; set; }    //自变量对应的潜变量名
    }

    public class M8_11VM : M8_1VM
    {
        public int SNField { get; set; }    //组号变量
    }

    public class M8_12VM : M8_11VM { }

    public class M9_1VM : M1_1VM
    {
        public int FieldType { get; set; }  //变量类型
        public int LayeringField { get; set; }  //分层变量
    }

    public class M9_2VM : M9_1VM { }

    public class M9_3VM : M9_1VM
    {
        public List<long> RelativeFields { get; set; }  //表型变量
        public List<string> RelativeTypes { get; set; } //表型变量的类型
        public List<string> RelativeLinkFuncs { get; set; } //表型变量类型的关联函数
        public List<long> AdjustFields { get; set; }    //调整变量
        public int IsSimulateP { get; set; }    //是否模拟P值
    }

    public class M9_4VM : M9_1VM
    {
        public List<long> RelativeFields { get; set; }  //表型变量
        public List<string> RelativeTypes { get; set; } //表型变量的类型
        public List<string> RelativeLinkFuncs { get; set; } //表型变量类型的关联函数
        public List<long> AdjustFields { get; set; }    //调整变量
        public bool HasSnp { get; set; }    //是否snp
        public int FamilySnField { get; set; }  //家系编号
        public string GeeType { get; set; }
        public int TimeField { get; set; }
        public int BeginTime { get; set; }
        public int EndTime { get; set; }
        public int LayeringField { get; set; }
        public int SnField { get; set; }
    }

    public class M1_2VM : M1_1VM
    {
        public int GroupingField { get; set; }
        public int LayeringField { get; set; }
        /// <summary>
        /// 备择假设
        /// </summary>
        public int scField { get; set; }
        /// <summary>
        /// 检验方法
        /// </summary>
        public int jqField { get; set; }
    }
    #endregion

    ///// <summary>
    ///// 统计方法调用
    ///// </summary>
    //public class MethodCallDTO
    //{
    //    public int ProjectId { get; set; }
    //    public object Parameters { get; set; }
    //}
}