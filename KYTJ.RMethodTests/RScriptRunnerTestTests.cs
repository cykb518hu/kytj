using Microsoft.VisualStudio.TestTools.UnitTesting;
using KYTJ.RMethod;
using System;
using System.Collections.Generic;
using System.Text;
using System.Data.SqlClient;
using System.Data;
using System.Diagnostics;

namespace KYTJ.RMethod.Tests
{
    [TestClass()]
    public class RScriptRunnerTestTests
    {
        [TestMethod()]
        public void RunFromCmdTest()
        {
            RScriptRunnerTest test = new RScriptRunnerTest();
            var FilePath = @"C:\Users\huzhe\source\repos\KYTJ.V2\KYTJ.Web/Output/1/1_1/637534985161508589\1.1.R";
            test.RunFromCmd(FilePath, System.IO.Path.GetDirectoryName(FilePath));
        }


        [TestMethod()]
        public void BulkInsertTest()
        {
            Console.WriteLine("开始时间:" + DateTime.Now);
            Debug.WriteLine("开始时间:" + DateTime.Now);
            DataTable dt = new DataTable();
            var new_d_col = new System.Data.DataColumn();
            new_d_col.ColumnName = "No";
            new_d_col.Caption = "No";
            new_d_col.DataType = typeof(long);
            dt.Columns.Add(new_d_col);

            new_d_col = new System.Data.DataColumn();
            new_d_col.ColumnName = "Dtime";
            new_d_col.Caption = "Dtime";
            new_d_col.DataType = typeof(DateTime);
            dt.Columns.Add(new_d_col);

            new_d_col = new System.Data.DataColumn();
            new_d_col.ColumnName = "MgrObjId";
            new_d_col.Caption = "MgrObjId";
            new_d_col.DataType = typeof(string);
            dt.Columns.Add(new_d_col);

            new_d_col = new System.Data.DataColumn();
            new_d_col.ColumnName = "Id";
            new_d_col.Caption = "Id";
            new_d_col.DataType = typeof(string);
            dt.Columns.Add(new_d_col);

            new_d_col = new System.Data.DataColumn();
            new_d_col.ColumnName = "Value";
            new_d_col.Caption = "Value";
            new_d_col.DataType = typeof(string);
            dt.Columns.Add(new_d_col);


            DataRow new_dr;
            for (int i = 0; i < 1000000; i++)
            {
                new_dr = dt.NewRow();
                new_dr["Dtime"] = DateTime.Now;
                new_dr["MgrObjId"] = "MgrObjId" + i.ToString();
                new_dr["Id"] = Guid.NewGuid().ToString();
                new_dr["Value"] = Guid.NewGuid().ToString();
                dt.Rows.Add(new_dr);
            }

            var connectionString = "Data Source=192.168.0.23;Initial Catalog=RareDisease;Persist Security Info=True;User ID=sa;Password=abc@123";// "Data Source=(localdb)\\MSSQLLocalDB;Initial Catalog=RareDisease;MultipleActiveResultSets=true;Persist Security Info=True;User ID=sa;Password=1989312hzw";

            var desTable = "His20140822";
            
            using (var sbc = new SqlBulkCopy(connectionString, SqlBulkCopyOptions.UseInternalTransaction)
            {
                BulkCopyTimeout = 300,
                NotifyAfter = dt.Rows.Count,
                BatchSize = 10000,
                DestinationTableName = desTable
            })
            {
                foreach (DataColumn column in dt.Columns)
                    sbc.ColumnMappings.Add(column.ColumnName, column.ColumnName);
                sbc.WriteToServer(dt);
            }
        }
    }
}