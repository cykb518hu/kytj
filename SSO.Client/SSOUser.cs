using Microsoft.AspNetCore.Http;
using System;

namespace SSO.Client
{

    public interface ISSOUser
    {
        public string GetUserName();
        public string GetUserIdentity();
    }
    public class SSOUserRepository : ISSOUser
    {
        private readonly IHttpContextAccessor _httpContextAccessor;
        public SSOUserRepository(IHttpContextAccessor httpContextAccessor)
        {
            _httpContextAccessor = httpContextAccessor;
        }
        public string GetUserName()
        {
            var result = "";
            _httpContextAccessor.HttpContext.Request.Cookies.TryGetValue("UserName", out result);
            return result;
        }

        public string GetUserIdentity()
        {
            var result= "";
            _httpContextAccessor.HttpContext.Request.Cookies.TryGetValue("Identity", out result);
            return result;
        }
    }

    public class SSOUserModel
    {
        public string UserName { get; set; }
        public string Identity { get; set; }
    }

    public class SSOClient
    {
        public static string AppName { get; set; }
        public static string VerifyUrl { get; set; }
        public static string LogoutUrl { get; set; }
        public static string ClientUrl { get; set; }
        public static string GetUserUrl { get; set; }
    }
}
