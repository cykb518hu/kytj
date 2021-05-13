﻿using KYTJ.Infrastructure.Model;
using Microsoft.Extensions.Caching.Memory;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;

namespace KYTJ.Infrastructure.Handler
{
    public interface ICacheHandler
    {
        public T Get<T>(string key);
        public void Set<T>(string key, T data);
    }

    public class LocalMemoryCache: ICacheHandler
    {
        private IMemoryCache _cache;
        private readonly ILogger<LocalMemoryCache> _logger;
        public LocalMemoryCache(IMemoryCache memoryCache, ILogger<LocalMemoryCache> logger)
        {
            _cache = memoryCache;
            _logger = logger;
        }

        public T Get<T>(string key)
        {
            T result = default(T);
            var data = _cache.Get(key);
            if (data == null)
            {
                _logger.LogWarning($"缓存{key}已经失效,或不存在");
            }
            else
            {
                result = (T)(data);
            }
            return result;
        }

        public void Set<T>(string key, T data)
        {
            TimeSpan ts = new TimeSpan(0, GlobalSetting.CacheExpire, 0);   
            _cache.Set(key, data, ts);
        }
    }
}
