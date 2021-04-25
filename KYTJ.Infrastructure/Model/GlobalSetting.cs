using System;
using System.Collections.Generic;
using System.Text;

namespace KYTJ.Infrastructure.Model
{
    public class GlobalSetting
    {
        public static string Logo { get; set; }

        public static string Title { get; set; }
        public static string SqlFilePath { get; set; }

        public static string RScriptRunnerPath { get; set; }

        public static string RScriptAcount { get; set; }

        public static int CacheExpire { get; set; }
    }
}
