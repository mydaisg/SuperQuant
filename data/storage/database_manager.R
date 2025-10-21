# data/storage/database_manager.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 1. 数据库管理器
DatabaseManager <- R6::R6Class(
  "DatabaseManager",
  public = list(
    initialize = function(config_path = "config/database.yml") {
      private$db_config <- yaml::read_yaml(config_path)
      private$initialize_connections()
      private$ensure_tables_exist()
    },
    
    # 存储市场数据
    store_market_data = function(data, data_type = "ohlc", replace = FALSE) {
      cat("存储", data_type, "数据...\n")
      
      table_name <- private$get_table_name(data_type)
      
      tryCatch({
        # 数据预处理
        db_data <- private$prepare_for_storage(data, data_type)
        
        if (replace) {
          # 替换模式
          DBI::dbWriteTable(
            private$db_conn, 
            table_name, 
            db_data, 
            overwrite = TRUE
          )
        } else {
          # 追加模式（去重）
          existing_data <- self$read_market_data(
            min(db_data$timestamp), 
            max(db_data$timestamp), 
            data_type
          )
          
          if (nrow(existing_data) > 0) {
            # 去重逻辑
            new_data <- private$deduplicate_data(db_data, existing_data)
          } else {
            new_data <- db_data
          }
          
          if (nrow(new_data) > 0) {
            DBI::dbAppendTable(private$db_conn, table_name, new_data)
            cat("成功存储", nrow(new_data), "条新数据\n")
          } else {
            cat("没有新数据需要存储\n")
          }
        }
        
        return(TRUE)
      }, error = function(e) {
        cat("存储数据失败:", e$message, "\n")
        return(FALSE)
      })
    },
    
    # 读取市场数据
    read_market_data = function(start_date, end_date, data_type = "ohlc", symbols = NULL) {
      table_name <- private$get_table_name(data_type)
      
      query <- paste(
        "SELECT * FROM", table_name,
        "WHERE timestamp >= $1 AND timestamp <= $2"
      )
      
      params <- list(start_date, end_date)
      
      if (!is.null(symbols)) {
        placeholders <- paste(rep("$", length(symbols)), seq_along(symbols) + 2, sep = "", collapse = ", ")
        query <- paste(query, "AND symbol IN (", placeholders, ")")
        params <- c(params, as.list(symbols))
      }
      
      query <- paste(query, "ORDER BY timestamp, symbol")
      
      tryCatch({
        data <- DBI::dbGetQuery(private$db_conn, query, params = params)
        cat("读取", nrow(data), "条", data_type, "数据\n")
        return(data)
      }, error = function(e) {
        cat("读取数据失败:", e$message, "\n")
        return(data.frame())
      })
    },
    
    # 批量存储
    store_batch_data = function(data_list, data_types, parallel = FALSE) {
      if (length(data_list) != length(data_types)) {
        stop("数据列表和类型列表长度不匹配")
      }
      
      results <- list()
      for (i in seq_along(data_list)) {
        results[[i]] <- self$store_market_data(
          data_list[[i]], 
          data_types[i]
        )
      }
      
      return(results)
    },
    
    # 获取最新数据时间戳
    get_latest_timestamp = function(data_type = "ohlc", symbol = NULL) {
      table_name <- private$get_table_name(data_type)
      
      query <- paste("SELECT MAX(timestamp) as latest FROM", table_name)
      if (!is.null(symbol)) {
        query <- paste(query, "WHERE symbol = $1")
        params <- list(symbol)
      } else {
        params <- list()
      }
      
      result <- DBI::dbGetQuery(private$db_conn, query, params = params)
      return(result$latest)
    },
    
    # 数据统计信息
    get_data_stats = function(data_type = "ohlc") {
      table_name <- private$get_table_name(data_type)
      
      stats_query <- paste(
        "SELECT 
          COUNT(*) as total_records,
          COUNT(DISTINCT symbol) as unique_symbols,
          MIN(timestamp) as earliest_date,
          MAX(timestamp) as latest_date,
          COUNT(DISTINCT DATE(timestamp)) as trading_days
        FROM", table_name
      )
      
      stats <- DBI::dbGetQuery(private$db_conn, stats_query)
      
      # 各标的统计数据
      symbol_stats_query <- paste(
        "SELECT 
          symbol,
          COUNT(*) as record_count,
          MIN(timestamp) as start_date,
          MAX(timestamp) as end_date
        FROM", table_name,
        "GROUP BY symbol
         ORDER BY record_count DESC"
      )
      
      symbol_stats <- DBI::dbGetQuery(private$db_conn, symbol_stats_query)
      
      return(list(
        overall = stats,
        by_symbol = symbol_stats
      ))
    },
    
    # 清理旧数据
    cleanup_old_data = function(before_date, data_type = "ohlc") {
      table_name <- private$get_table_name(data_type)
      
      delete_query <- paste(
        "DELETE FROM", table_name,
        "WHERE timestamp < $1"
      )
      
      tryCatch({
        result <- DBI::dbExecute(private$db_conn, delete_query, params = list(before_date))
        cat("删除了", result, "条", data_type, "数据\n")
        return(result)
      }, error = function(e) {
        cat("删除数据失败:", e$message, "\n")
        return(0)
      })
    }
  ),
  
  private = list(
    db_config = NULL,
    db_conn = NULL,
    
    initialize_connections = function() {
      tryCatch({
        private$db_conn <- DBI::dbConnect(
          RPostgres::Postgres(),
          dbname = private$db_config$database,
          host = private$db_config$host,
          port = private$db_config$port,
          user = private$db_config$username,
          password = private$db_config$password
        )
        cat("数据库连接成功\n")
      }, error = function(e) {
        stop("数据库连接失败: ", e$message)
      })
    },
    
    ensure_tables_exist = function() {
      # 创建OHLC数据表
      ohlc_table_sql <- "
      CREATE TABLE IF NOT EXISTS market_ohlc (
        id SERIAL PRIMARY KEY,
        symbol VARCHAR(20) NOT NULL,
        timestamp TIMESTAMP NOT NULL,
        open DECIMAL(15,6),
        high DECIMAL(15,6),
        low DECIMAL(15,6),
        close DECIMAL(15,6),
        volume BIGINT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(symbol, timestamp)
      );"
      
      # 创建特征数据表
      features_table_sql <- "
      CREATE TABLE IF NOT EXISTS market_features (
        id SERIAL PRIMARY KEY,
        symbol VARCHAR(20) NOT NULL,
        timestamp TIMESTAMP NOT NULL,
        feature_name VARCHAR(100) NOT NULL,
        feature_value DECIMAL(15,6),
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(symbol, timestamp, feature_name)
      );"
      
      # 创建元数据表
      metadata_table_sql <- "
      CREATE TABLE IF NOT EXISTS data_metadata (
        id SERIAL PRIMARY KEY,
        data_type VARCHAR(50) NOT NULL,
        symbol VARCHAR(20),
        start_date TIMESTAMP,
        end_date TIMESTAMP,
        record_count INTEGER,
        data_hash VARCHAR(64),
        quality_score DECIMAL(5,4),
        last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );"
      
      # 执行创建语句
      tables_sql <- list(
        ohlc = ohlc_table_sql,
        features = features_table_sql,
        metadata = metadata_table_sql
      )
      
      for (table_name in names(tables_sql)) {
        tryCatch({
          DBI::dbExecute(private$db_conn, tables_sql[[table_name]])
          cat("表", table_name, "就绪\n")
        }, error = function(e) {
          warning("创建表", table_name, "失败:", e$message)
        })
      }
      
      # 创建索引以提高查询性能
      index_sqls <- list(
        "CREATE INDEX IF NOT EXISTS idx_ohlc_symbol_time ON market_ohlc (symbol, timestamp)",
        "CREATE INDEX IF NOT EXISTS idx_ohlc_time ON market_ohlc (timestamp)",
        "CREATE INDEX IF NOT EXISTS idx_features_symbol_time ON market_features (symbol, timestamp)",
        "CREATE INDEX IF NOT EXISTS idx_metadata_type_symbol ON data_metadata (data_type, symbol)"
      )
      
      for (index_sql in index_sqls) {
        tryCatch({
          DBI::dbExecute(private$db_conn, index_sql)
        }, error = function(e) {
          warning("创建索引失败:", e$message)
        })
      }
    },
    
    get_table_name = function(data_type) {
      switch(data_type,
             ohlc = "market_ohlc",
             features = "market_features",
             fundamental = "market_fundamental",
             stop("未知的数据类型: ", data_type)
      )
    },
    
    prepare_for_storage = function(data, data_type) {
      # 添加存储时间戳
      data$created_at <- Sys.time()
      
      # 数据类型特定的预处理
      switch(data_type,
             ohlc = private$prepare_ohlc_data(data),
             features = private$prepare_features_data(data),
             data  # 默认不处理
      )
    },
    
    prepare_ohlc_data = function(data) {
      # 确保必要的列存在
      required_cols <- c("symbol", "timestamp", "open", "high", "low", "close", "volume")
      missing_cols <- setdiff(required_cols, names(data))
      
      if (length(missing_cols) > 0) {
        stop("OHLC数据缺少必要列: ", paste(missing_cols, collapse = ", "))
      }
      
      # 选择需要的列
      data[, required_cols]
    },
    
    prepare_features_data = function(data) {
      # 特征数据需要转换为长格式
      feature_cols <- setdiff(
        names(data), 
        c("symbol", "timestamp", "created_at", "is_outlier", "cleaning_timestamp", "data_hash")
      )
      
      feature_data <- data.frame()
      
      for (col in feature_cols) {
        if (is.numeric(data[[col]])) {
          feature_rows <- data.frame(
            symbol = data$symbol,
            timestamp = data$timestamp,
            feature_name = col,
            feature_value = data[[col]],
            created_at = data$created_at
          )
          feature_data <- rbind(feature_data, feature_rows)
        }
      }
      
      return(feature_data)
    },
    
    deduplicate_data = function(new_data, existing_data) {
      # 基于时间戳和符号去重
      if ("symbol" %in% names(new_data) && "symbol" %in% names(existing_data)) {
        existing_keys <- paste(existing_data$symbol, existing_data$timestamp)
        new_keys <- paste(new_data$symbol, new_data$timestamp)
      } else {
        existing_keys <- existing_data$timestamp
        new_keys <- new_data$timestamp
      }
      
      duplicate_indices <- new_keys %in% existing_keys
      
      if (any(duplicate_indices)) {
        cat("发现", sum(duplicate_indices), "条重复数据，已跳过\n")
        return(new_data[!duplicate_indices, ])
      } else {
        return(new_data)
      }
    }
  )
)