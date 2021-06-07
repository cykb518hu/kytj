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
        private readonly ISSOUser _ssoUser;
        public ProjectController(ILogger<ProjectController> logger, IProjectRepository projectRepository, ILogRepository logRepository,ISSOUser sSOUser)
        {
            _logger = logger;
            _projectRepository = projectRepository;
            _logRepository = logRepository;
            _ssoUser = sSOUser;
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

                var userName = _ssoUser.GetUserIdentity();
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

        public async Task<JsonResult> MaintainProject(int id, string projectName, string projectDesc)
        {
            try
            {
                var result = false;
                var userName = _ssoUser.GetUserIdentity();
                var msg = "操作成功";
                if (id > 0)
                {
                    _logRepository.Add($"修改项目", projectName, "", $"项目id:{id},项目:{projectName},项目描述:{projectDesc}");
                    result = await _projectRepository.UpdateProjectAsync(id, projectName, projectDesc);
                }
                else
                {
                    _logRepository.Add($"新增项目", projectName, "", $"项目:{projectName},项目描述:{projectDesc}");
                    result = _projectRepository.AddProject(projectName, projectDesc, userName);
                }
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

        public async Task<JsonResult> GetProjectAndSub()
        {
            try
            {
                var total = 0;

                var userName = _ssoUser.GetUserIdentity();// HttpContext.User.Identity.Name;
                var data = await _projectRepository.GetProjectAndSubAsync(userName);
                return Json(new { success = true, data, total });
            }
            catch (Exception ex)
            {
                return Json(new { success = false, msg = ex.ToString() });
            }
        }
    }
}
