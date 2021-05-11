using KYTJ.Data.Repository;
using KYTJ.Web.Models;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using SSO.Client;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;

namespace KYTJ.Web.Controllers
{
    public class HomeController : Controller
    {
        private readonly ILogger<HomeController> _logger;
        private readonly IProjectRepository _projectRepository;
        private readonly IHttpContextAccessor _httpContextAccessor;
        private readonly ISSOUser _ssoUser;
        public HomeController(ILogger<HomeController> logger, IProjectRepository projectRepository, IHttpContextAccessor httpContextAccessor, ISSOUser sSOUser)
        {
            _logger = logger;
            _projectRepository = projectRepository;
            _httpContextAccessor = httpContextAccessor;
            _ssoUser = sSOUser;
        }

        public IActionResult Index()
        {
            return View();
        }

        public IActionResult LogOut()
        {
            _httpContextAccessor.HttpContext.Response.Cookies.Delete("token");
            _httpContextAccessor.HttpContext.Response.Cookies.Delete("Identity");
            _httpContextAccessor.HttpContext.Response.Cookies.Delete("UserName");
            return Redirect(SSOClient.LogoutUrl);
        }


    }
}
