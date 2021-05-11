
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
using KYTJ.Web.Service;
using SoapCore;
using System.ServiceModel;
using KYTJ.Web.Hanlder;
using SSO.Client;
using Microsoft.AspNetCore.CookiePolicy;

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
            services.AddControllersWithViews().AddRazorRuntimeCompilation();
            services.AddSingleton<IHttpContextAccessor, HttpContextAccessor>();

            services.AddTransient<IProjectRepository, ProjectRepository>();
            services.AddTransient<ILogRepository, LogRepository>();
            services.AddTransient<IDataSetRepository, DataSetRepository>();
            services.AddTransient<IDataManageRepository, DataManageRepository>();
            services.AddTransient<IDataFlowRepository, DataFlowRepository>();
            services.AddTransient<ICacheHandler, LocalMemoryCache>();
            services.AddTransient<IDataService, DataService>();
            services.AddTransient<ISSOUser, SSOUserRepository>();
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
            GlobalSetting.Title = Configuration.GetValue<string>("GlobalSetting:Title");
            GlobalSetting.CacheExpire = Configuration.GetValue<int>("GlobalSetting:CacheExpire");
            GlobalSetting.SiteUrl = Configuration.GetValue<string>("GlobalSetting:SiteUrl");

            SSOClient.AppName = Configuration.GetValue<string>("SSOClient:AppName");
            SSOClient.VerifyUrl = Configuration.GetValue<string>("SSOClient:VerifyUrl");
            SSOClient.LogoutUrl = Configuration.GetValue<string>("SSOClient:LogoutUrl");
            SSOClient.GetUserUrl = Configuration.GetValue<string>("SSOClient:GetUserUrl");
            SSOClient.ClientUrl = Configuration.GetValue<string>("SSOClient:ClientUrl");
            SSOClient.Enable = Configuration.GetValue<bool>("SSOClient:Enable");
            services.AddMvc(options => {
                options.Filters.Add(new SSOActionFIlter());
            });
            services.AddHttpClient();
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

            //ideally we should set this as httponly, since layout.cshtml need to read cookie via jquery 
            //so disable it temporarily 
            //app.UseCookiePolicy(new CookiePolicyOptions
            //{
            //    HttpOnly = HttpOnlyPolicy.Always
            //});

            app.UseAuthorization();
            app.UseSoapEndpoint<IDataService>("/DataService.asmx", new BasicHttpBinding(), SoapSerializer.XmlSerializer);
            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllerRoute(
                    name: "default",
                    pattern: "{controller=Home}/{action=Index}/{id?}");
            });
        }
    }
}
