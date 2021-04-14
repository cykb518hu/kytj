using KYTJ.Infrastructure.Model;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Web;

namespace KYTJ.RMethod
{
    /// <summary>
    /// This class runs R code from a file using the console.
    /// </summary>
    internal static class RScriptRunner
    {

        /// <summary>
        /// Runs an R script from a file using Rscript.exe.
        /// Example:  
        ///   RScriptRunner.RunFromCmd(curDirectory + @"\ImageClustering.r", curDirectory.Replace('\\','/'));
        /// Getting args passed from C# using R:
        ///   args = commandArgs(trailingOnly = TRUE)
        ///   print(args[1]);
        /// </summary>
        /// <param name="rCodeFilePath">File where your R code is located.</param>
        /// <param name="args">Multiple R args can be seperated by spaces.</param>
        /// <returns>Returns a string with the R responses.</returns>
        public static string RunFromCmd(string rCodeFilePath, params string[] args)
        {
            string file = rCodeFilePath;
            string result = string.Empty;
            try
            {
                var is64Bit = System.Environment.Is64BitProcess;
                var path = GlobalSetting.RScriptRunnerPath;
                var libPath = path + "library";
                var binPath = Path.Combine(path, "bin");
                binPath = Path.Combine(binPath, is64Bit ? "x64" : "i386");
                binPath = Path.Combine(binPath, "RScript.exe");
                var strCmdLine = @"""" + file + @""" """ + libPath + @"""";
                if (args.Any())
                {
                    strCmdLine += " " + string.Join(" ", args);
                }
                var info = new ProcessStartInfo();
                info.RedirectStandardInput = false;
                info.RedirectStandardError = true;
                info.RedirectStandardOutput = true;
                info.FileName = binPath; //"cmd";
                info.WorkingDirectory = Path.GetDirectoryName(file);
                info.Arguments = strCmdLine; //rCodeFilePath + " " + args;

                info.RedirectStandardInput = false;
                info.RedirectStandardOutput = true;
                info.UseShellExecute = false;
                info.CreateNoWindow = true;

                using (var proc = new Process())
                {
                    proc.StartInfo = info;
                    proc.Start();
                    result = proc.StandardOutput.ReadToEnd();
                    proc.Close();
                }
            }
            catch (Exception ex)
            {
                throw new Exception("R Script failed: " + result, ex);
            }
            return result;
        }
    }

    public  class RScriptRunnerTest
    {

        /// <summary>
        /// Runs an R script from a file using Rscript.exe.
        /// Example:  
        ///   RScriptRunner.RunFromCmd(curDirectory + @"\ImageClustering.r", curDirectory.Replace('\\','/'));
        /// Getting args passed from C# using R:
        ///   args = commandArgs(trailingOnly = TRUE)
        ///   print(args[1]);
        /// </summary>
        /// <param name="rCodeFilePath">File where your R code is located.</param>
        /// <param name="args">Multiple R args can be seperated by spaces.</param>
        /// <returns>Returns a string with the R responses.</returns>
        public  string RunFromCmd(string rCodeFilePath, params string[] args)
        {
            string file = rCodeFilePath;
            string result = string.Empty;
            try
            {
                var is64Bit = System.Environment.Is64BitProcess;
                var path = "C:\\Program Files\\R\\R-3.4.3\\";
                var libPath = path + "library";
                var binPath = Path.Combine(path, "bin");
                binPath = Path.Combine(binPath, is64Bit ? "x64" : "i386");
                binPath = Path.Combine(binPath, "RScript.exe");
                var strCmdLine = @"""" + file + @""" """ + libPath + @"""";
                if (args.Any())
                {
                    strCmdLine += " " + string.Join(" ", args);
                }
                var info = new ProcessStartInfo();
                info.RedirectStandardInput = false;
                info.RedirectStandardError = true;
                info.RedirectStandardOutput = true;
                info.FileName = binPath; //"cmd";
                info.WorkingDirectory = Path.GetDirectoryName(file);
                info.Arguments = strCmdLine; //rCodeFilePath + " " + args;

                info.RedirectStandardInput = false;
                info.RedirectStandardOutput = true;
                info.UseShellExecute = false;
                info.CreateNoWindow = true;

                using (var proc = new Process())
                {
                    proc.StartInfo = info;
                    proc.Start();
                    result = proc.StandardOutput.ReadToEnd();
                    proc.Close();
                }
            }
            catch (Exception ex)
            {
                throw new Exception("R Script failed: " + result, ex);
            }
            return result;
        }
    }
}