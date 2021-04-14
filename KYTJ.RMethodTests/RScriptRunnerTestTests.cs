using Microsoft.VisualStudio.TestTools.UnitTesting;
using KYTJ.RMethod;
using System;
using System.Collections.Generic;
using System.Text;

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
    }
}