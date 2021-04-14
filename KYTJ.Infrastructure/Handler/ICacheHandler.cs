using Microsoft.Extensions.Caching.Memory;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
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
                _logger.LogWarning($"缓存{key}已经失效");
            }
            else
            {
                result = (T)(data);
            }
            return result;
        }

        public void Set<T>(string key, T data)
        {
            _cache.Set(key, data);
        }
    }
}
