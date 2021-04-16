
using KYTJ.Data.Context;
using KYTJ.Infrastructure.Model;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using KYTJ.Data.Repository;
using Newtonsoft;
using KYTJ.Infrastructure.Handler;
using KYTJ.Business.Repository;

namespace KYTJ.Web
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddControllersWithViews();
            services.AddSingleton<IHttpContextAccessor, HttpContextAccessor>();

            services.AddScoped<IProjectRepository, ProjectRepository>();
            services.AddScoped<ILogRepository, LogRepository>();
            services.AddScoped<IDataSetRepository, DataSetRepository>();
            services.AddScoped<IDataManageRepository, DataManageRepository>();
            services.AddScoped<IDataFlowRepository, DataFlowRepository>();
            services.AddScoped<ICacheHandler, LocalMemoryCache>();
            KytjDbContext.KyStaticManagement = Configuration.GetConnectionString("KyStaticManagement");
            KytjDbContext.MySqlConnection = Configuration.GetConnectionString("MysqlConnection");
            KytjDbContext.ResearchData = Configuration.GetConnectionString("ResearchData");
            services.AddControllers().AddNewtonsoftJson(options =>
            {
                options.SerializerSettings.DateFormatString = "yyyy-MM-dd HH:mm:ss";

                options.SerializerSettings.ContractResolver = new Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver();
            });
            GlobalSetting.Logo = Configuration.GetValue<string>("GlobalSetting:Logo");
            GlobalSetting.SqlFilePath = Configuration.GetValue<string>("GlobalSetting:SqlFilePath");
            GlobalSetting.RScriptRunnerPath = Configuration.GetValue<string>("GlobalSetting:RScriptRunnerPath");
            GlobalSetting.RScriptAcount = Configuration.GetValue<string>("GlobalSetting:RScriptAcount");
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            app.UseCors(options =>
            {
                options.AllowAnyHeader();
                options.AllowAnyMethod();
                options.AllowAnyOrigin();
            });
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                app.UseExceptionHandler("/Home/Error");
            }
            app.UseStaticFiles();

            app.UseRouting();

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllerRoute(
                    name: "default",
                    pattern: "{controller=Home}/{action=Index}/{id?}");
            });
        }
    }
}