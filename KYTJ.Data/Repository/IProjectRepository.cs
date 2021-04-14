using KYTJ.Data.Context;
using KYTJ.Model;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace KYTJ.Data.Repository
{
    public interface IProjectRepository
    {
        public List<ProjectModel> GetProjectAndSub(string userName);
        List<ProjectModel> Search(string userName, int pageIndex, int pageSize, ref int totalCount);
        bool DeleteProject(int id);

        bool AddProject(string projectName, string projectDesc, string userName);
        bool EditProject(int id, string projectName, string projectDesc);

        ProjectModel GetProject(int projectId);
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

        public List<ProjectModel> GetProjectAndSub(string userName)
        {
            var resultsTask = new List<ProjectModel>();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask = query.Where(x => x.UserName == userName && x.IsDeleted == 0)
                    .Mapper(it => it.DataSetList, it => it.DataSetList.First().ProjectId)
                .Mapper((s, cache) =>
                {
                    foreach (var t in s.DataSetList)
                    {
                        t.ResultDataList = _dbKyStatic.Queryable<ResultData>().Where(o => o.DataSetId == t.DataSetId).ToList();
                    }
                }
                ).ToList();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目及其数据失败：" + ex.ToString());
            }
            return resultsTask;
        }

        public ProjectModel GetProject(int projectId)
        {
            var resultsTask = new ProjectModel();
            try
            {
                var query = _dbKyStatic.Queryable<ProjectModel>();
                resultsTask = query.Where(x => x.Id == projectId)
                .ToList().FirstOrDefault();
            }
            catch (Exception ex)
            {
                _logger.LogError("查询项目及其数据失败GetProject：" + ex.ToString());
            }
            return resultsTask;
        }
    }
}
