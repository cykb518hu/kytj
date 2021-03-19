using KYTJ.Data.Repository;
using KYTJ.Web.Models;
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
        public HomeController(ILogger<HomeController> logger, IProjectRepository projectRepository)
        {
            _logger = logger;
            _projectRepository = projectRepository;
        }

        public IActionResult Index()
        {
            return View();
        }

        public IActionResult Privacy()
        {
            return View();
        }

        [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
        public IActionResult Error()
        {
            return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
        }

        public IActionResult Project()
        {
            return View();
        }
        public JsonResult SearchProjectList(int pageIndex = 1, int pageSize =10)
        {
            try
            {
                var total = 0;

                var userName = SSOUser.GetUser();// HttpContext.User.Identity.Name;
                var data = _projectRepository.Search( userName, pageIndex, pageSize, ref total);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult DeleteProject(int id)
        {
            try
            {
                var data = _projectRepository.DeleteProject(id);
                var msg = "删除成功";
                if (!data)
                {
                    msg = "删除失败";
                }
                return Json(new { success = data, msg });

            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

        public JsonResult MaintainProject(int id,string projectName,string projectDesc)
        {
            try
            {
                var result = false;
                var userName = SSOUser.GetUser();
                if (id > 0)
                {
                    result = _projectRepository.EditProject(id, projectName, projectDesc);
                }
                else
                {
                    result = _projectRepository.AddProject(projectName, projectDesc, userName);
                }
                var msg = "操作成功";
                if (!result)
                {
                    msg = "操作失败";
                }
                return Json(new { success = result, msg });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }

    }
}
