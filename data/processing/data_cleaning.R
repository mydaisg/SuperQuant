# /opt/github/data/processing/data_cleaning.R
#' 1. 数据清洗管道(数据清洗管理器)
DataCleaningManager <- R6::R6Class(
  "DataCleaningManager",
  public = list(
    initialize = function(config_path = "config/data_quality.yml") {
      private$quality_config <- yaml::read_yaml(config_path)
      private$initialize_cleaning_rules()
    },
    
    # 主清洗管道
    clean_market_data = function(raw_data, data_type = "ohlc") {
      cat("开始数据清洗流程...\n")
      
      # 多阶段清洗管道
      cleaned_data <- raw_data %>%
        private$validate_structure() %>%
        private$handle_missing_values() %>%
        private$remove_duplicates() %>%
        private$detect_outliers() %>%
        private$smooth_data() %>%
        private$validate_quality() %>%
        private$add_metadata()
      
      cat("数据清洗完成，原始数据:", nrow(raw_data), "行，清洗后:", nrow(cleaned_data), "行\n")
      return(cleaned_data)
    },
    
    # 批量清洗
    clean_batch_data = function(data_list, parallel = TRUE) {
      if (parallel && requireNamespace("future", quietly = TRUE)) {
        # 并行处理
        plan(multisession)
        cleaned_list <- future.apply::future_lapply(
          data_list, 
          self$clean_market_data,
          future.seed = TRUE
        )
        plan(sequential)
      } else {
        # 顺序处理
        cleaned_list <- lapply(data_list, self$clean_market_data)
      }
      
      return(cleaned_list)
    },
    
    # 数据质量报告
    generate_quality_report = function(raw_data, cleaned_data) {
      report <- list(
        timestamp = Sys.time(),
        initial_rows = nrow(raw_data),
        final_rows = nrow(cleaned_data),
        removed_rows = nrow(raw_data) - nrow(cleaned_data),
        removal_rate = round((nrow(raw_data) - nrow(cleaned_data)) / nrow(raw_data) * 100, 2),
        quality_metrics = private$calculate_quality_metrics(cleaned_data)
      )
      
      return(report)
    }
  ),
  
  private = list(
    quality_config = NULL,
    cleaning_rules = list(),
    
    initialize_cleaning_rules = function() {
      private$cleaning_rules <- list(
        ohlc = list(
          price_threshold = private$quality_config$price_thresholds,
          volume_threshold = private$quality_config$volume_thresholds,
          time_gaps = private$quality_config$time_gaps
        ),
        fundamental = list(
          sanity_checks = private$quality_config$fundamental_sanity
        )
      )
    },
    
    # 数据结构验证
    validate_structure = function(data) {
      cat("验证数据结构...\n")
      
      required_cols <- private$quality_config$required_columns
      missing_cols <- setdiff(required_cols, names(data))
      
      if (length(missing_cols) > 0) {
        stop("缺少必要列: ", paste(missing_cols, collapse = ", "))
      }
      
      # 检查数据类型
      type_issues <- private$check_column_types(data)
      if (length(type_issues) > 0) {
        warning("数据类型问题: ", paste(type_issues, collapse = "; "))
      }
      
      return(data)
    },
    
    # 处理缺失值
    handle_missing_values = function(data) {
      cat("处理缺失值...\n")
      rules <- private$cleaning_rules$ohlc
      
      # 按列应用不同的缺失值处理策略
      for (col in names(data)) {
        if (any(is.na(data[[col]]))) {
          na_count <- sum(is.na(data[[col]]))
          cat("列", col, "发现", na_count, "个缺失值\n")
          
          data[[col]] <- private$impute_missing_values(
            data[[col]], 
            col, 
            rules
          )
        }
      }
      
      # 移除全是NA的行
      complete_cases <- complete.cases(data[, private$quality_config$critical_columns])
      data <- data[complete_cases, ]
      
      return(data)
    },
    
    # 缺失值插补
    impute_missing_values = function(vector, col_name, rules) {
      na_positions <- is.na(vector)
      
      if (sum(na_positions) == 0) return(vector)
      
      # 根据列类型选择插补方法
      if (grepl("price|close|open|high|low", col_name, ignore.case = TRUE)) {
        # 价格数据：前向填充 + 后向填充
        vector <- zoo::na.locf(vector, na.rm = FALSE)
        vector <- zoo::na.locf(vector, fromLast = TRUE, na.rm = FALSE)
      } else if (grepl("volume|amount", col_name, ignore.case = TRUE)) {
        # 交易量：用0填充
        vector[na_positions] <- 0
      } else {
        # 其他数据：线性插值
        if (is.numeric(vector)) {
          vector <- approx(1:length(vector), vector, xout = 1:length(vector))$y
        }
      }
      
      return(vector)
    },
    
    # 去除重复数据
    remove_duplicates = function(data) {
      cat("去除重复数据...\n")
      
      # 基于时间戳去重
      if ("timestamp" %in% names(data)) {
        data <- data[!duplicated(data$timestamp), ]
      }
      
      # 基于OHLC去重
      key_cols <- c("open", "high", "low", "close", "volume")
      if (all(key_cols %in% names(data))) {
        data <- data[!duplicated(data[, key_cols]), ]
      }
      
      return(data)
    },
    
    # 异常值检测
    detect_outliers = function(data) {
      cat("检测异常值...\n")
      rules <- private$cleaning_rules$ohlc
      
      outlier_flags <- rep(FALSE, nrow(data))
      
      # 价格异常值检测
      if ("close" %in% names(data)) {
        price_outliers <- private$detect_price_outliers(data$close, rules$price_threshold)
        outlier_flags <- outlier_flags | price_outliers
      }
      
      # 交易量异常值检测
      if ("volume" %in% names(data)) {
        volume_outliers <- private$detect_volume_outliers(data$volume, rules$volume_threshold)
        outlier_flags <- outlier_flags | volume_outliers
      }
      
      # 记录但保留异常值（在元数据中标记）
      if (sum(outlier_flags) > 0) {
        cat("发现", sum(outlier_flags), "个异常值\n")
        data$is_outlier <- outlier_flags
      }
      
      return(data)
    },
    
    # 价格异常值检测
    detect_price_outliers = function(prices, thresholds) {
      returns <- c(NA, diff(log(prices)))
      returns[is.infinite(returns)] <- NA
      
      # 基于收益率的异常值检测
      mad_returns <- mad(returns, na.rm = TRUE)
      median_returns <- median(returns, na.rm = TRUE)
      
      outlier_threshold <- thresholds$price_jump * mad_returns
      outliers <- abs(returns - median_returns) > outlier_threshold
      
      return(outliers)
    },
    
    # 交易量异常值检测
    detect_volume_outliers = function(volumes, thresholds) {
      log_volume <- log(volumes + 1)  # 避免log(0)
      volume_mad <- mad(log_volume, na.rm = TRUE)
      volume_median <- median(log_volume, na.rm = TRUE)
      
      outliers <- abs(log_volume - volume_median) > thresholds$volume_spike * volume_mad
      return(outliers)
    },
    
    # 数据平滑
    smooth_data = function(data) {
      if (!private$quality_config$smoothing$enabled) {
        return(data)
      }
      
      cat("应用数据平滑...\n")
      
      # 只对标记为异常值的数据进行平滑
      if ("is_outlier" %in% names(data)) {
        outlier_indices <- which(data$is_outlier)
        
        for (col in private$quality_config$smoothing$columns) {
          if (col %in% names(data)) {
            data[outlier_indices, col] <- private$smooth_outliers(
              data[[col]], 
              outlier_indices
            )
          }
        }
      }
      
      return(data)
    },
    
    # 平滑异常值
    smooth_outliers = function(vector, outlier_indices) {
      if (length(outlier_indices) == 0) return(vector)
      
      for (idx in outlier_indices) {
        # 使用相邻点的均值替换异常值
        neighbors <- c(idx-1, idx+1)
        neighbors <- neighbors[neighbors >= 1 & neighbors <= length(vector)]
        
        if (length(neighbors) > 0) {
          vector[idx] <- mean(vector[neighbors], na.rm = TRUE)
        }
      }
      
      return(vector)
    },
    
    # 质量验证
    validate_quality = function(data) {
      cat("验证数据质量...\n")
      
      quality_checks <- list(
        has_negative_prices = any(data$close <= 0, na.rm = TRUE),
        has_extreme_returns = private$check_extreme_returns(data),
        time_gaps = private$check_time_gaps(data)
      )
      
      # 记录质量检查结果
      data$quality_checks <- I(list(quality_checks))
      
      # 如果有严重质量问题，发出警告
      if (quality_checks$has_negative_prices) {
        warning("数据中存在负价格！")
      }
      
      return(data)
    },
    
    # 检查极端收益率
    check_extreme_returns = function(data) {
      if (!"close" %in% names(data)) return(FALSE)
      
      returns <- c(NA, diff(log(data$close)))
      extreme_threshold <- private$cleaning_rules$ohlc$price_threshold$extreme_return
      
      any(abs(returns, na.rm = TRUE) > extreme_threshold)
    },
    
    # 检查时间间隔
    check_time_gaps = function(data) {
      if (!"timestamp" %in% names(data)) return(FALSE)
      
      time_diffs <- diff(data$timestamp)
      max_gap <- max(time_diffs, na.rm = TRUE)
      
      max_gap > private$cleaning_rules$ohlc$time_gaps$max_allowed_gap
    },
    
    # 添加元数据
    add_metadata = function(data) {
      data$cleaning_timestamp <- Sys.time()
      data$data_hash <- digest::digest(data)
      data$cleaning_version <- "1.0"
      
      return(data)
    },
    
    # 计算质量指标
    calculate_quality_metrics = function(data) {
      metrics <- list(
        completeness = mean(complete.cases(data)),
        uniqueness = length(unique(data$timestamp)) / nrow(data),
        price_consistency = private$calculate_price_consistency(data)
      )
      
      return(metrics)
    },
    
    # 计算价格一致性
    calculate_price_consistency = function(data) {
      if (!all(c("open", "high", "low", "close") %in% names(data))) {
        return(NA)
      }
      
      # 检查OHLC关系：low <= open,high,close <= high
      valid_ohlc <- with(data, {
        low <= pmin(open, high, close) & high >= pmax(open, high, close)
      })
      
      mean(valid_ohlc, na.rm = TRUE)
    },
    
    # 检查列数据类型
    check_column_types = function(data) {
      issues <- c()
      
      numeric_cols <- c("open", "high", "low", "close", "volume")
      for (col in numeric_cols) {
        if (col %in% names(data) && !is.numeric(data[[col]])) {
          issues <- c(issues, paste(col, "应该是数值型"))
        }
      }
      
      return(issues)
    }
  )
)