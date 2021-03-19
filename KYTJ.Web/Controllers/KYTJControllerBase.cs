using Microsoft.AspNetCore.Mvc;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace KYTJ.Web
{
    public abstract class KYTJControllerBase : Controller
    {
        //public readonly JsonSerializerSettings _jsonSetting = new JsonSerializerSettings()
        //{
        //    DateFormatHandling = Newtonsoft.Json.DateFormatHandling.MicrosoftDateFormat,
        //    DateFormatString = "yyyy-MM-dd HH:mm:ss",
        //    ContractResolver = new Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
        //};
    }
}
