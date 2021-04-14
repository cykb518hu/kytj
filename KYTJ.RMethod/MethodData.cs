using KYTJ.Model;
using System;
using System.Collections.Generic;


namespace KYTJ.RMethod
{
    public class MethodData
    {
        public MethodData()
        {
            this.DataColumns = new List<DFDataColumn>();
        }

        public System.Data.DataTable dt { get; set; }
        public List<DFDataColumn> DataColumns { get; set; }

    }
}
