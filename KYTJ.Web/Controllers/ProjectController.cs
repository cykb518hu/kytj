using KYTJ.Data.Repository;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using SSO.Client;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace KYTJ.Web.Controllers
{
    public class ProjectController : Controller
    {
        private readonly ILogger<ProjectController> _logger;
        private readonly IProjectRepository _projectRepository;
        private readonly ILogRepository _logRepository;
        public ProjectController(ILogger<ProjectController> logger, IProjectRepository projectRepository, ILogRepository logRepository)
        {
            _logger = logger;
            _projectRepository = projectRepository;
            _logRepository = logRepository;
        }
        public IActionResult Index()
        {
            _logRepository.Add("查看主页");
            return View();
        }

        public JsonResult Search(int pageIndex = 1, int pageSize = 10)
        {
            try
            {
                var total = 0;

                var userName = SSOUser.GetUserName();// HttpContext.User.Identity.Name;
                var data = _projectRepository.Search(userName, pageIndex, pageSize, ref total);
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
                _logRepository.Add($"删除项目,项目id:{id}");
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

        public JsonResult MaintainProject(int id, string projectName, string projectDesc)
        {
            try
            {
                var result = false;
                var userName = SSOUser.GetUserName();
                if (id > 0)
                {
                    _logRepository.Add($"修改项目", "", "", "", $"项目ID:{id}");
                    result = _projectRepository.UpdateProject(id, projectName, projectDesc);
                }
                else
                {
                    _logRepository.Add($"新增项目",projectName);
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

        public JsonResult GetProjectAndSub()
        {
            try
            {
                var total = 0;

                var userName = SSOUser.GetUserName();// HttpContext.User.Identity.Name;
                var data = _projectRepository.GetProjectAndSub(userName);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
    }
}
