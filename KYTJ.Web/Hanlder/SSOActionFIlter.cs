using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Microsoft.Net.Http.Headers;
using Newtonsoft.Json;
using SSO.Client;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

namespace KYTJ.Web.Hanlder
{
    public class SSOActionFIlter:IActionFilter
    {
        //private readonly  IHttpClientFactory _httpClientFactory;

        // public SSOActionFIlter(IHttpClientFactory httpClientFactory)
        // {
        //     _httpClientFactory = httpClientFactory;
        // }

        private const string str_AuthenticateRequest = @"
            <!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 3.2 Final//EN'>
            <html>
            <body>         
            <form name='ClientPage' method='post' action='{0}' >                            
                <input type='hidden' id='ClientUrl' name='ClientUrl' />                     
                <input type='hidden' id='AppName' name='AppName' /> 
            </form>
            <script language='javascript'  type='text/javascript'>
               document.getElementById('AppName').value='{3}';
               if (window.frameElement && (window.frameElement.nodeName == 'IFRAME' || window.frameElement.nodeName == 'FRAME'))                                
                   document.getElementById('ClientUrl').value='{1}';
               else
                   document.getElementById('ClientUrl').value='{2}';
                  
                document.forms['ClientPage'].submit();
            </script>
            </body>
            </html>
            ";

        
        public void OnActionExecuting(ActionExecutingContext context)
        {
            var token = "";
            context.HttpContext.Request.Cookies.TryGetValue("token", out token);

            if (context.HttpContext.Request.Method == "GET")
            {
                if (string.IsNullOrEmpty(token))
                {
                    token = context.HttpContext.Request.Query["token"];
                }

                if (string.IsNullOrEmpty(token))
                {
                    token = context.HttpContext.Request.Query["tk"];
                }
            }
            if (context.HttpContext.Request.Method == "POST")
            {
                if (string.IsNullOrEmpty(token))
                {
                    token = context.HttpContext.Request.Form["token"];
                }
                if (string.IsNullOrEmpty(token))
                {
                    token = context.HttpContext.Request.Form["tk"];
                }
            }

            if (!string.IsNullOrEmpty(token))
            {
                using (var httpClient = new HttpClient())
                {
                    var url = $"{SSOClient.GetUserUrl}?token={token}";
                    var httpResponseMessage = httpClient.GetStringAsync(url).Result;
                    var user = JsonConvert.DeserializeObject<SSOUserModel>(httpResponseMessage);
                    if (user != null && !string.IsNullOrEmpty(user.Identity))
                    {
                        context.HttpContext.Response.Cookies.Append("token", token);
                        context.HttpContext.Response.Cookies.Append("Identity", user.Identity);
                        context.HttpContext.Response.Cookies.Append("UserName", user.UserName);
                    }
                    else
                    {
                        RedirectToSSO(context);
                    }
                }
               
            }
            else
            {
                RedirectToSSO(context);
            }

        }
        public void OnActionExecuted(ActionExecutedContext context)
        {

        }

        private void RedirectToSSO(ActionExecutingContext context)
        {
            string strPost = string.Format(str_AuthenticateRequest,
                 SSOClient.VerifyUrl,
                 SSOClient.ClientUrl, SSOClient.ClientUrl, SSOClient.AppName);
            byte[] data = System.Text.Encoding.UTF8.GetBytes(strPost);
            context.HttpContext.Response.Body.WriteAsync(data, 0, data.Length);
        }
    }
}
