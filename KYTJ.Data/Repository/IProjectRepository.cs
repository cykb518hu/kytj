using KYTJ.Data.Context;
using KYTJ.Model;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Text;

namespace KYTJ.Data.Repository
{
    public interface IProjectRepository
    {
        List<ProjectModel> Search(string userName, int pageIndex, int pageSize, ref int totalCount);
        bool DeleteProject(int id);

        bool AddProject(string projectName, string projectDesc, string userName);
        bool EditProject(int id, string projectName, string projectDesc);
    }
    public class ProjectRepository: KytjDbContext,IProjectRepository
    {
        private readonly ILogger<ProjectRepository> _logger;
        public ProjectRepository(ILogger<ProjectRepository> logger)
        {
            _logger = logger;
        }
        public List<ProjectModel> Search(string userName, int pageIndex, int pageSize, ref int totalCount)
        {
            var resultsTask = new List<ProjectModel>();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask = query.Where(x => x.UserName == userName && x.IsDeleted == 0).OrderBy("CreateTime desc").ToPageList(pageIndex, pageSize, ref totalCount);
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目失败：" + ex.ToString());
            }
            return resultsTask;
        }

        public bool DeleteProject(int id)
        {
            var result = false;
            try
            {
                result = _dbKyStatic.Updateable<ProjectModel>(x => x.IsDeleted == 1).Where(x => x.Id == id).ExecuteCommandHasChange();
            }
            catch(Exception ex)
            {
                _logger.LogError("删除项目失败：" + ex.ToString());
            }
            return result;
        }
        public bool EditProject(int id, string projectName, string projectDesc)
        {
            var result = false;
            try
            {
                result = _dbKyStatic.Updateable<ProjectModel>()
                    .SetColumns(x=>new ProjectModel() { ProjectName=projectName,ProjectDesc=projectDesc,UpdateTime=DateTime.Now })
                    .Where(x => x.Id == id).ExecuteCommandHasChange();
            }
            catch (Exception ex)
            {
                _logger.LogError("编辑项目失败：" + ex.ToString());
            }
            return result;
        }
        public bool AddProject(string projectName, string projectDesc,string userName)
        {
            var result = false;
            try
            {
                ProjectModel project = new ProjectModel();
                project.ProjectName = projectName;
                project.ProjectDesc = projectDesc;
                project.UserName = userName;
                project.CreateTime = DateTime.Now;
                project.UpdateTime = DateTime.Now;
                project.DiseaseType = 1;
                project.DeptID = 1;
                int count = _dbKyStatic.Insertable(project).ExecuteCommand();
                result = count > 0 ? true : false;
            }
            catch (Exception ex)
            {
                _logger.LogError("新增项目失败：" + ex.ToString());
            }
            return result;
        }
    }
}
