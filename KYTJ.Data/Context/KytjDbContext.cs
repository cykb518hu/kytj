using KYTJ.Infrastructure.Model;
using KYTJ.Model;
using SqlSugar;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace KYTJ.Data.Context
{
    public class KytjDbContext
    {
        public static string ResearchData { get; set; }
        public static string KyStaticManagement { get; set; }       
        public static string MySqlConnection { get; set; }

        public SqlSugarClient _dbResearch;
        public SqlSugarClient _dbKyStatic;
        public SqlSugarClient _dbMySql;
        public KytjDbContext()
        {
            _dbKyStatic = new SqlSugarClient(new ConnectionConfig()
            {
                ConnectionString = KyStaticManagement,  
                DbType = DbType.SqlServer, 
                InitKeyType = InitKeyType.SystemTable, 
                IsAutoCloseConnection = true 
            });
            _dbKyStatic.Aop.OnLogExecuted = (sql, pars) =>
            {
                //这里可以写log方法，比如把sql和pars存入数据库或者log文件
            };
            _dbResearch = new SqlSugarClient(new ConnectionConfig()
            {
                ConnectionString = ResearchData,
                DbType = DbType.SqlServer,
                InitKeyType = InitKeyType.SystemTable,
                IsAutoCloseConnection = true
            });
            _dbResearch.Aop.OnLogExecuted = (sql, pars) =>
            {
                //这里可以写log方法，比如把sql和pars存入数据库或者log文件
            };

            _dbMySql = new SqlSugarClient(new ConnectionConfig()
            {
                ConnectionString = MySqlConnection,
                DbType = DbType.MySql,
                InitKeyType = InitKeyType.SystemTable,
                IsAutoCloseConnection = true
            });
            _dbMySql.Aop.OnLogExecuted = (sql, pars) =>
            {
                //这里可以写log方法，比如把sql和pars存入数据库或者log文件
            };
        }
        public string GetSqlText(string fileName)
        {
            var path = AppContext.BaseDirectory + "//App_Data//sql//" + fileName;

            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);//注册简体中文的支持
            var str = File.ReadAllText(path, Encoding.GetEncoding("gb2312"));
            return str;
        }
    }


}
