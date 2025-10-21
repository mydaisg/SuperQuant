# data/storage/cache_manager.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 2. 缓存管理器
CacheManager <- R6::R6Class(
  "CacheManager",
  public = list(
    initialize = function(cache_config_path = "config/cache.yml") {
      private$cache_config <- yaml::read_yaml(cache_config_path)
      private$initialize_cache()
    },
    
    # 获取缓存数据
    get = function(key, type = "data") {
      cache_key <- private$build_cache_key(key, type)
      
      # 检查内存缓存
      if (cache_key %in% names(private$memory_cache)) {
        cached_item <- private$memory_cache[[cache_key]]
        
        # 检查是否过期
        if (!private$is_expired(cached_item)) {
          cat("从内存缓存命中:", cache_key, "\n")
          return(cached_item$data)
        } else {
          # 移除过期缓存
          private$memory_cache[[cache_key]] <- NULL
        }
      }
      
      # 检查磁盘缓存
      disk_data <- private$get_from_disk_cache(cache_key)
      if (!is.null(disk_data)) {
        cat("从磁盘缓存命中:", cache_key, "\n")
        
        # 更新内存缓存
        private$set_memory_cache(cache_key, disk_data)
        return(disk_data)
      }
      
      cat("缓存未命中:", cache_key, "\n")
      return(NULL)
    },
    
    # 设置缓存数据
    set = function(key, data, type = "data", ttl = NULL) {
      if (is.null(ttl)) {
        ttl <- private$get_default_ttl(type)
      }
      
      cache_key <- private$build_cache_key(key, type)
      cache_item <- list(
        data = data,
        timestamp = Sys.time(),
        ttl = ttl
      )
      
      # 设置内存缓存
      private$set_memory_cache(cache_key, cache_item)
      
      # 设置磁盘缓存（如果数据大小合适）
      if (private$should_cache_to_disk(data, type)) {
        private$set_disk_cache(cache_key, cache_item)
      }
      
      cat("设置缓存:", cache_key, "\n")
    },
    
    # 批量获取缓存
    mget = function(keys, type = "data") {
      result <- list()
      for (key in keys) {
        result[[key]] <- self$get(key, type)
      }
      return(result)
    },
    
    # 批量设置缓存
    mset = function(key_data_list, type = "data", ttl = NULL) {
      for (key in names(key_data_list)) {
        self$set(key, key_data_list[[key]], type, ttl)
      }
    },
    
    # 清理过期缓存
    cleanup = function() {
      cat("开始缓存清理...\n")
      
      # 清理内存缓存
      memory_keys <- names(private$memory_cache)
      expired_count <- 0
      
      for (key in memory_keys) {
        if (private$is_expired(private$memory_cache[[key]])) {
          private$memory_cache[[key]] <- NULL
          expired_count <- expired_count + 1
        }
      }
      
      cat("清理了", expired_count, "个过期内存缓存项\n")
      
      # 清理磁盘缓存
      disk_cleanup_count <- private$cleanup_disk_cache()
      cat("清理了", disk_cleanup_count, "个过期磁盘缓存项\n")
    },
    
    # 获取缓存统计
    get_stats = function() {
      list(
        memory_cache_size = length(private$memory_cache),
        memory_cache_keys = names(private$memory_cache),
        disk_cache_size = private$get_disk_cache_size(),
        disk_cache_files = private$get_disk_cache_files()
      )
    },
    
    # 清空缓存
    clear = function(type = NULL) {
      if (is.null(type)) {
        # 清空所有缓存
        private$memory_cache <- list()
        private$clear_disk_cache()
        cat("清空所有缓存\n")
      } else {
        # 清空特定类型缓存
        keys_to_remove <- grep(paste0("^", type, ":"), names(private$memory_cache), value = TRUE)
        for (key in keys_to_remove) {
          private$memory_cache[[key]] <- NULL
        }
        private$clear_disk_cache_by_type(type)
        cat("清空", type, "类型缓存\n")
      }
    }
  ),
  
  private = list(
    cache_config = NULL,
    memory_cache = list(),
    cache_dir = NULL,
    
    initialize_cache = function() {
      # 创建缓存目录
      private$cache_dir <- private$cache_config$disk_cache$directory
      if (!dir.exists(private$cache_dir)) {
        dir.create(private$cache_dir, recursive = TRUE)
      }
      
      # 设置内存缓存大小限制
      private$memory_cache <- list()
    },
    
    build_cache_key = function(key, type) {
      paste(type, digest::digest(key), sep = ":")
    },
    
    get_default_ttl = function(type) {
      ttl_config <- private$cache_config$default_ttl
      
      switch(type,
             market_data = ttl_config$market_data,
             features = ttl_config$features,
             model = ttl_config$model,
             ttl_config$default
      )
    },
    
    is_expired = function(cache_item) {
      if (is.null(cache_item)) return(TRUE)
      
      elapsed_time <- as.numeric(Sys.time() - cache_item$timestamp, units = "secs")
      elapsed_time > cache_item$ttl
    },
    
    set_memory_cache = function(key, cache_item) {
      # 检查内存限制
      if (length(private$memory_cache) >= private$cache_config$memory_cache$max_items) {
        # 移除最旧的缓存项
        timestamps <- sapply(private$memory_cache, function(x) x$timestamp)
        oldest_key <- names(which.min(timestamps))
        private$memory_cache[[oldest_key]] <- NULL
      }
      
      private$memory_cache[[key]] <- cache_item
    },
    
    should_cache_to_disk = function(data, type) {
      size_limit <- private$cache_config$disk_cache$max_size_mb * 1024 * 1024
      
      # 估算对象大小
      object_size <- as.numeric(utils::object.size(data))
      
      # 检查类型是否允许磁盘缓存
      type_allowed <- type %in% private$cache_config$disk_cache$allowed_types
      
      type_allowed && object_size <= size_limit
    },
    
    set_disk_cache = function(key, cache_item) {
      cache_file <- file.path(private$cache_dir, paste0(key, ".rds"))
      
      tryCatch({
        saveRDS(cache_item, cache_file)
        TRUE
      }, error = function(e) {
        warning("磁盘缓存设置失败:", e$message)
        FALSE
      })
    },
    
    get_from_disk_cache = function(key) {
      cache_file <- file.path(private$cache_dir, paste0(key, ".rds"))
      
      if (file.exists(cache_file)) {
        tryCatch({
          cache_item <- readRDS(cache_file)
          
          # 检查是否过期
          if (!private$is_expired(cache_item)) {
            return(cache_item$data)
          } else {
            # 删除过期文件
            file.remove(cache_file)
          }
        }, error = function(e) {
          warning("读取磁盘缓存失败:", e$message)
        })
      }
      
      return(NULL)
    },
    
    cleanup_disk_cache = function() {
      cache_files <- list.files(private$cache_dir, pattern = "\\.rds$", full.names = TRUE)
      expired_count <- 0
      
      for (cache_file in cache_files) {
        tryCatch({
          cache_item <- readRDS(cache_file)
          if (private$is_expired(cache_item)) {
            file.remove(cache_file)
            expired_count <- expired_count + 1
          }
        }, error = function(e) {
          # 损坏的文件也删除
          file.remove(cache_file)
          expired_count <- expired_count + 1
        })
      }
      
      return(expired_count)
    },
    
    clear_disk_cache = function() {
      cache_files <- list.files(private$cache_dir, pattern = "\\.rds$", full.names = TRUE)
      file.remove(cache_files)
    },
    
    clear_disk_cache_by_type = function(type) {
      pattern <- paste0("^", type, ":.*\\.rds$")
      cache_files <- list.files(private$cache_dir, pattern = pattern, full.names = TRUE)
      file.remove(cache_files)
    },
    
    get_disk_cache_size = function() {
      cache_files <- list.files(private$cache_dir, pattern = "\\.rds$", full.names = TRUE)
      if (length(cache_files) == 0) return(0)
      
      file_sizes <- file.size(cache_files)
      sum(file_sizes) / (1024 * 1024)  # 转换为MB
    },
    
    get_disk_cache_files = function() {
      list.files(private$cache_dir, pattern = "\\.rds$")
    }
  )
)