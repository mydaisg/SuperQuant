# data/processing/feature_engineering.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
##' 2. 特征工程引擎
FeatureEngineeringEngine <- R6::R6Class(
  "FeatureEngineeringEngine",
  public = list(
    initialize = function(feature_config_path = "config/features.yml") {
      private$feature_config <- yaml::read_yaml(feature_config_path)
      private$initialize_feature_groups()
    },
    
    # 生成所有特征
    generate_features = function(cleaned_data, feature_groups = NULL) {
      cat("开始特征工程...\n")
      
      if (is.null(feature_groups)) {
        feature_groups <- names(private$feature_groups)
      }
      
      feature_data <- cleaned_data
      
      # 按组生成特征
      for (group in feature_groups) {
        cat("生成特征组:", group, "\n")
        feature_data <- private$generate_feature_group(feature_data, group)
      }
      
      # 特征选择（如果配置）
      if (private$feature_config$feature_selection$enabled) {
        feature_data <- private$select_features(feature_data)
      }
      
      cat("特征工程完成，总特征数:", ncol(feature_data) - ncol(cleaned_data), "\n")
      return(feature_data)
    },
    
    # 批量特征生成
    generate_batch_features = function(data_list, parallel = TRUE) {
      if (parallel && requireNamespace("future", quietly = TRUE)) {
        plan(multisession)
        feature_list <- future.apply::future_lapply(
          data_list, 
          self$generate_features,
          future.seed = TRUE
        )
        plan(sequential)
      } else {
        feature_list <- lapply(data_list, self$generate_features)
      }
      
      return(feature_list)
    },
    
    # 特征重要性分析
    analyze_feature_importance = function(feature_data, target_column) {
      if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("需要安装randomForest包来进行特征重要性分析")
      }
      
      # 准备数据
      feature_cols <- setdiff(
        names(feature_data), 
        c("timestamp", "symbol", target_column, "is_outlier", "cleaning_timestamp", "data_hash")
      )
      
      model_data <- feature_data[complete.cases(feature_data[, c(feature_cols, target_column)]), ]
      
      if (nrow(model_data) < 10) {
        warning("数据量太少，无法进行特征重要性分析")
        return(NULL)
      }
      
      # 训练随机森林
      rf_model <- randomForest::randomForest(
        x = model_data[, feature_cols],
        y = model_data[[target_column]],
        importance = TRUE,
        ntree = 100
      )
      
      importance_df <- as.data.frame(randomForest::importance(rf_model))
      importance_df$feature <- rownames(importance_df)
      rownames(importance_df) <- NULL
      
      return(importance_df[order(-importance_df$IncNodePurity), ])
    }
  ),
  
  private = list(
    feature_config = NULL,
    feature_groups = list(),
    
    initialize_feature_groups = function() {
      private$feature_groups <- list(
        price_technical = private$feature_config$technical_indicators,
        volume_analysis = private$feature_config$volume_indicators,
        volatility_measures = private$feature_config$volatility_measures,
        pattern_recognition = private$feature_config$pattern_indicators
      )
    },
    
    # 生成特征组
    generate_feature_group = function(data, group_name) {
      switch(group_name,
             price_technical = private$add_technical_indicators(data),
             volume_analysis = private$add_volume_indicators(data),
             volatility_measures = private$add_volatility_measures(data),
             pattern_recognition = private$add_pattern_indicators(data),
             stop("未知的特征组: ", group_name)
      )
    },
    
    # 技术指标
    add_technical_indicators = function(data) {
      indicators <- private$feature_groups$price_technical
      
      # 移动平均线
      if (indicators$moving_averages$enabled) {
        periods <- indicators$moving_averages$periods
        for (period in periods) {
          data[[paste0("sma_", period)]] <- TTR::SMA(data$close, n = period)
          data[[paste0("ema_", period)]] <- TTR::EMA(data$close, n = period)
        }
      }
      
      # RSI
      if (indicators$rsi$enabled) {
        data$rsi <- TTR::RSI(data$close, n = indicators$rsi$period)
      }
      
      # MACD
      if (indicators$macd$enabled) {
        macd <- TTR::MACD(data$close, 
                          nFast = indicators$macd$fast, 
                          nSlow = indicators$macd$slow, 
                          nSig = indicators$macd$signal)
        data$macd <- macd$macd
        data$macd_signal <- macd$signal
        data$macd_histogram <- macd$macd - macd$signal
      }
      
      # 布林带
      if (indicators$bollinger_bands$enabled) {
        bb <- TTR::BBands(data$close, 
                          n = indicators$bollinger_bands$period,
                          sd = indicators$bollinger_bands$sd)
        data$bb_upper <- bb$up
        data$bb_middle <- bb$mavg
        data$bb_lower <- bb$dn
        data$bb_position <- (data$close - data$bb_lower) / (data$bb_upper - data$bb_lower)
      }
      
      return(data)
    },
    
    # 成交量指标
    add_volume_indicators = function(data) {
      indicators <- private$feature_groups$volume_analysis
      
      if (!"volume" %in% names(data)) {
        warning("缺少volume列，跳过成交量指标")
        return(data)
      }
      
      # 成交量移动平均
      if (indicators$volume_ma$enabled) {
        periods <- indicators$volume_ma$periods
        for (period in periods) {
          data[[paste0("volume_ma_", period)]] <- TTR::SMA(data$volume, n = period)
        }
      }
      
      # 量价关系
      data$price_volume_correlation <- private$calculate_volume_price_correlation(data)
      
      # 成交量比率
      if (indicators$volume_ratio$enabled) {
        data$volume_ratio <- data$volume / TTR::SMA(data$volume, n = indicators$volume_ratio$period)
      }
      
      return(data)
    },
    
    # 波动率指标
    add_volatility_measures = function(data) {
      indicators <- private$feature_groups$volatility_measures
      
      returns <- c(NA, diff(log(data$close)))
      
      # 历史波动率
      if (indicators$historical_volatility$enabled) {
        period <- indicators$historical_volatility$period
        data$historical_vol <- zoo::rollapply(
          returns, width = period, FUN = sd, na.rm = TRUE, fill = NA, align = "right"
        ) * sqrt(252)  # 年化
      }
      
      # ATR (平均真实波幅)
      if (indicators$atr$enabled && all(c("high", "low", "close") %in% names(data))) {
        data$atr <- TTR::ATR(data[, c("high", "low", "close")], n = indicators$atr$period)$atr
      }
      
      #  Parkinson波动率
      if (indicators$parkinson_volatility$enabled) {
        data$parkinson_vol <- private$calculate_parkinson_volatility(data)
      }
      
      return(data)
    },
    
    # 模式识别指标
    add_pattern_indicators = function(data) {
      indicators <- private$feature_groups$pattern_recognition
      
      # 价格位置特征
      if (indicators$price_position$enabled) {
        lookback <- indicators$price_position$lookback
        data$price_vs_high <- data$close / zoo::rollmax(data$close, k = lookback, fill = NA, align = "right")
        data$price_vs_low <- data$close / zoo::rollmin(data$close, k = lookback, fill = NA, align = "right")
      }
      
      # 动量特征
      if (indicators$momentum$enabled) {
        periods <- indicators$momentum$periods
        for (period in periods) {
          data[[paste0("momentum_", period)]] <- data$close / lag(data$close, period) - 1
        }
      }
      
      # 价格加速度
      if (indicators$acceleration$enabled) {
        data$price_acceleration <- c(NA, NA, diff(diff(log(data$close))))
      }
      
      return(data)
    },
    
    # 计算量价相关性
    calculate_volume_price_correlation = function(data, window = 20) {
      n <- nrow(data)
      correlation <- rep(NA, n)
      
      for (i in window:n) {
        window_data <- data[(i-window+1):i, ]
        cor_val <- cor(window_data$close, window_data$volume, use = "complete.obs")
        correlation[i] <- ifelse(is.na(cor_val), 0, cor_val)
      }
      
      return(correlation)
    },
    
    # 计算Parkinson波动率
    calculate_parkinson_volatility = function(data, window = 20) {
      hl_ratio <- log(data$high / data$low)^2
      parkinson_vol <- sqrt(1/(4 * window * log(2)) * zoo::rollsum(hl_ratio, k = window, fill = NA, align = "right"))
      return(parkinson_vol * sqrt(252))  # 年化
    },
    
    # 特征选择
    select_features = function(data) {
      selection_config <- private$feature_config$feature_selection
      
      # 移除低方差特征
      if (selection_config$remove_low_variance) {
        data <- private$remove_low_variance_features(data)
      }
      
      # 移除高相关性特征
      if (selection_config$remove_high_correlation) {
        data <- private$remove_high_correlation_features(data)
      }
      
      return(data)
    },
    
    # 移除低方差特征
    remove_low_variance_features = function(data) {
      feature_cols <- private$get_feature_columns(data)
      variances <- sapply(data[, feature_cols], var, na.rm = TRUE)
      
      low_var_features <- names(variances[variances < private$feature_config$feature_selection$variance_threshold])
      if (length(low_var_features) > 0) {
        cat("移除低方差特征:", paste(low_var_features, collapse = ", "), "\n")
        data <- data[, !names(data) %in% low_var_features]
      }
      
      return(data)
    },
    
    # 移除高相关性特征
    remove_high_correlation_features = function(data) {
      feature_cols <- private$get_feature_columns(data)
      correlation_matrix <- cor(data[, feature_cols], use = "complete.obs")
      
      high_corr_pairs <- which(abs(correlation_matrix) > private$feature_config$feature_selection$correlation_threshold & 
                                 correlation_matrix < 1, arr.ind = TRUE)
      
      if (nrow(high_corr_pairs) > 0) {
        features_to_remove <- unique(rownames(correlation_matrix)[high_corr_pairs[, 1]])
        cat("移除高相关性特征:", paste(features_to_remove, collapse = ", "), "\n")
        data <- data[, !names(data) %in% features_to_remove]
      }
      
      return(data)
    },
    
    # 获取特征列名
    get_feature_columns = function(data) {
      exclude_cols <- c("timestamp", "symbol", "is_outlier", "cleaning_timestamp", "data_hash", "quality_checks")
      setdiff(names(data), exclude_cols)
    }
  )
)