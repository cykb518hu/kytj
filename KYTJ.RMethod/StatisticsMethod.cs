using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;

namespace KYTJ.RMethod
{
    public abstract class StatisticsMethod
    {
        protected Func<bool> BeforeExecute = null;
        protected Func<bool> OnExecuting = null;

        public  ILogger<StatisticsMethod> StaticLogger;


        /// <summary>
        /// 方法的全路径
        /// </summary>
        public string RScriptFileName { get; set; }
        public string RScriptWorkingDirectory { get; set; }

        /// <summary>
        /// 数据(string)及列
        /// </summary>
        protected MethodData MethodData { get; private set; }
        /// <summary>
        /// 数据表
        /// </summary>

        //public DataTable dt { get; set; }
        public bool Execute(MethodData methodData)
        {
            var result = false;
            MethodData = methodData;
            var isReady = true;
            if (BeforeExecute != null)
            {
                isReady = BeforeExecute();
            }
            if (isReady)
            {
                if (OnExecuting != null)
                {
                    result = OnExecuting();
                }
            }
            else
            {
                StaticLogger.LogWarning("计算数据不全");
            }
            return result;
        }

        protected bool DataValid()
        {
            var res = true;
            res = res && MethodData != null && MethodData.dt != null && MethodData.dt.Rows.Count != 0;
            res = res && MethodData.DataColumns != null && MethodData.DataColumns.Count > 0;
            return res;
        }

        protected bool WriteDataTo(MethodData methodData)
        {
            StaticLogger.LogInformation("开始写datatable数据");
            var fullPath = RScriptWorkingDirectory + "\\data.csv";
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(fullPath, false, Encoding.GetEncoding("gb2312")))
            {
                StringBuilder sb = new StringBuilder();
                string[] sdfr = Array.ConvertAll(methodData.dt.Columns.Cast<DataColumn>().ToArray(), r => RemoveSpecicalChar(r.ColumnName.ToUpper()));
                sb.AppendLine(string.Join(",", sdfr));
                DataRow[] drs = methodData.dt.Select("true");
                foreach (DataRow item in drs)
                {
                    for (int i = 0, len = item.ItemArray.Length; i < len; i++)
                        if (item.ItemArray[i].GetType() == typeof(string))
                            item.SetField<string>(i, item.ItemArray[i].ToString().Replace(",", "，")
                                .Replace((char)13, (char)0).Replace((char)10, (char)0));
                    sb.AppendLine(string.Join(",", item.ItemArray));
                }
                sw.WriteLine(sb.ToString());
            }
            return true;
        }
        protected bool WriteParamNew(List<string> param)
        {
            StaticLogger.LogInformation("开始写parameter数据");
            var fullPath = RScriptWorkingDirectory + "\\Parameters.csv";
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);//注册简体中文的支持
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(fullPath, false, Encoding.GetEncoding("gb2312")))
            {
                sw.WriteLine("p00");
                foreach (var item in param)
                {
                    sw.WriteLine($"\"{RemoveSpecicalChar(item)}\"");
                }
                sw.WriteLine();
            }
            return true;
        }

        protected bool WriteParametersTo(List<string> lines, int maxCount)
        {
                 StaticLogger.LogInformation("开始写parameter数据");
            var str = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,";
            var sb = new StringBuilder();
            for (int i = 0; i < 100; i++)
            {
                sb.Append(",p" + i.ToString("00"));
            }
            str = sb.ToString().Substring(",".Length);
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);//注册简体中文的支持
            var fullPath = RScriptWorkingDirectory + "\\Parameters.csv";
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(fullPath, false, Encoding.GetEncoding("gb2312")))
            {
                sw.WriteLine(str.Substring(0, maxCount * 4 - 1));
                foreach (var item in lines)
                {
                    sw.WriteLine(RemoveSpecicalChar(item));
                }
                sw.WriteLine();
            }
            return true;
        }
        protected bool ExecuteMethod()
        {
            var result =  RScriptRunner.RunFromCmd(Path.Combine(RScriptWorkingDirectory, RScriptFileName), RScriptWorkingDirectory);
            StaticLogger.LogInformation("计算完成:" + result);
            return string.IsNullOrEmpty(result);
        }

        protected string RemoveSpecicalChar(string str)
        {
            return str.Replace("@", "").Replace("#", "").Replace("￥", "").Replace("%", "").Replace("…", "").Replace("&", "").Replace("*", "").Replace("（", "").Replace("）", "").Replace("(", "").Replace(")", "");
        }
    }
}
