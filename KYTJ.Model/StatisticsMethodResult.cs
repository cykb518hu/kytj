using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace KYTJ.Model
{

    public class StatisticsMethodResult
    {
       
        public StatisticsMethodResult()
        {
            Pictures = new List<string>();
        }
        public string OutputHTML { get; set; }
        public List<string> Pictures { get; set; }

        public string DownLoadZip { get; set; }
    }

}