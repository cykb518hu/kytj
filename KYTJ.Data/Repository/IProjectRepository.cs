using KYTJ.Data.Context;
using KYTJ.Model;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Threading.Tasks;
using SqlSugar;

namespace KYTJ.Data.Repository
{
    public interface IProjectRepository
    {
        Task<List<ProjectModel>> GetProjectAndSubAsync(string userName);
        List<ProjectModel> Search(string userName, int pageIndex, int pageSize, ref int totalCount);
        bool DeleteProject(int id);

        bool AddProject(string projectName, string projectDesc, string userName);
        Task<bool> UpdateProjectAsync(int id, string projectName, string projectDesc);

        ProjectModel GetProjectById(int id);

        List<ProjectModel> GetAllProjects();
        List<ProjectModel> GetAllProjectsByUser(string userName);
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
            //RefAsync<int> totalCount = 0;
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
            catch (Exception ex)
            {
                _logger.LogError("删除项目失败：" + ex.ToString());
            }
            return result;
        }
        public async Task<bool> UpdateProjectAsync(int id, string projectName, string projectDesc)
        {
            var result = false;
            try
            {
                result = await _dbKyStatic.Updateable<ProjectModel>()
                    .SetColumns(x=>new ProjectModel() { ProjectName=projectName,ProjectDesc=projectDesc,UpdateTime=DateTime.Now })
                    .Where(x => x.Id == id).ExecuteCommandHasChangeAsync();
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
                int count =  _dbKyStatic.Insertable(project).ExecuteCommand();
                result = count > 0 ? true : false;
            }
            catch (Exception ex)
            {
                _logger.LogError("新增项目失败：" + ex.ToString());
            }
            return result;
        }

        public async Task<List<ProjectModel>> GetProjectAndSubAsync(string userName)
        {
            var resultsTask = new List<ProjectModel>();
            try
            {
                var query =  _dbKyStatic.Queryable<ProjectModel>();
                resultsTask =  await query.Where(x => x.UserName == userName && x.IsDeleted == 0)
                    .Mapper(it => it.DataSetList, it => it.DataSetList.First().ProjectId)
                .Mapper((s, cache) =>
                {
                    foreach (var t in s.DataSetList)
                    {
                        t.ResultDataList = _dbKyStatic.Queryable<ResultData>().Where(o => o.DataSetId == t.DataSetId).ToList();
                    }
                }
                ).ToListAsync();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目及其数据失败：" + ex.ToString());
            }
            return resultsTask;
        }

        public ProjectModel GetProjectById(int id)
        {
            var resultsTask = new ProjectModel();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask =  query.Where(x => x.Id == id)
                .First();
            }
            catch (Exception ex)
            {
                _logger.LogError("GetProject：" + ex.ToString());
            }
            return resultsTask;
        }

        public List<ProjectModel> GetAllProjects()
        {
            var resultsTask = new List<ProjectModel>();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask =  query.Where(x => x.IsDeleted == 0).OrderBy("CreateTime desc").ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目失败GetAllProjects：" + ex.ToString());
            }
            return resultsTask;
        }
        public List<ProjectModel> GetAllProjectsByUser(string userName)
        {
            var resultsTask = new List<ProjectModel>();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask =  query.Where(x => x.IsDeleted == 0 && x.UserName == userName).ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目失败GetAllProjects：" + ex.ToString());
            }
            return resultsTask;
        }

    }
}
