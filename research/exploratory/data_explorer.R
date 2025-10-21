# research/exploratory/data_explorer.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
##' 数据探索分析器
DataExplorer <- R6::R6Class(
  "DataExplorer",
  public = list(
    initialize = function(config_path = "config/exploration.yml") {
      private$exploration_config <- yaml::read_yaml(config_path)
      private$initialize_analysis_methods()
    },
    
    # 综合探索分析
    explore_dataset = function(data, target_symbol = NULL, analysis_types = NULL) {
      cat("开始数据集探索分析...\n")
      
      if (is.null(analysis_types)) {
        analysis_types <- private$exploration_config$default_analyses
      }
      
      results <- list(
        metadata = private$analyze_metadata(data),
        timestamp = Sys.time()
      )
      
      # 执行各类分析
      for (analysis in analysis_types) {
        cat("执行分析:", analysis, "\n")
        results[[analysis]] <- switch(analysis,
                                      descriptive_stats = private$descriptive_statistics(data),
                                      distribution_analysis = private$distribution_analysis(data),
                                      correlation_analysis = private$correlation_analysis(data),
                                      time_series_analysis = private$time_series_analysis(data),
                                      volatility_analysis = private$volatility_analysis(data),
                                      regime_analysis = private$regime_analysis(data),
                                      stop("未知的分析类型: ", analysis)
        )
      }
      
      # 生成综合报告
      results$summary <- private$generate_exploration_summary(results)
      
      return(results)
    },
    
    # 多资产对比分析
    compare_assets = function(data_list, symbols) {
      cat("开始多资产对比分析...\n")
      
      comparison_results <- list()
      
      # 收益率对比
      comparison_results$returns_comparison <- private$compare_returns(data_list, symbols)
      
      # 风险指标对比
      comparison_results$risk_comparison <- private$compare_risk_metrics(data_list, symbols)
      
      # 相关性结构分析
      comparison_results$correlation_structure <- private$analyze_correlation_structure(data_list, symbols)
      
      # 市场状态同步性
      comparison_results$market_regimes <- private$compare_market_regimes(data_list, symbols)
      
      return(comparison_results)
    },
    
    # 可视化探索
    create_exploration_plots = function(data, symbol, output_dir = "output/exploratory") {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      plots <- list()
      
      # 价格序列图
      plots$price_series <- private$plot_price_series(data, symbol)
      
      # 收益率分布图
      plots$return_distribution <- private$plot_return_distribution(data, symbol)
      
      # 波动率聚类图
      plots$volatility_clustering <- private$plot_volatility_clustering(data, symbol)
      
      # 自相关图
      plots$autocorrelation <- private$plot_autocorrelation(data, symbol)
      
      # 保存图片
      for (plot_name in names(plots)) {
        filename <- file.path(output_dir, paste0(symbol, "_", plot_name, ".png"))
        ggplot2::ggsave(filename, plots[[plot_name]], width = 10, height = 6, dpi = 300)
      }
      
      cat("探索图已保存至:", output_dir, "\n")
      return(plots)
    },
    
    # 异常模式检测
    detect_anomalous_patterns = function(data, symbol) {
      cat("检测异常模式...\n")
      
      patterns <- list()
      
      # 极端收益率检测
      patterns$extreme_returns <- private$detect_extreme_returns(data, symbol)
      
      # 波动率突变检测
      patterns$volatility_breaks <- private$detect_volatility_breaks(data, symbol)
      
      # 流动性异常检测
      patterns$liquidity_anomalies <- private$detect_liquidity_anomalies(data, symbol)
      
      # 季节性模式检测
      patterns$seasonal_patterns <- private$detect_seasonal_patterns(data, symbol)
      
      return(patterns)
    },
    
    # 生成探索报告
    generate_exploration_report = function(exploration_results, output_file = NULL) {
      cat("生成探索分析报告...\n")
      
      report <- list(
        executive_summary = private$create_executive_summary(exploration_results),
        detailed_analysis = exploration_results,
        recommendations = private$generate_recommendations(exploration_results),
        timestamp = Sys.time()
      )
      
      if (!is.null(output_file)) {
        # 保存为RDS
        saveRDS(report, output_file)
        cat("报告已保存至:", output_file, "\n")
      }
      
      return(report)
    }
  ),
  
  private = list(
    exploration_config = NULL,
    analysis_methods = list(),
    
    initialize_analysis_methods = function() {
      private$analysis_methods <- list(
        descriptive = c("mean", "median", "sd", "skewness", "kurtosis"),
        technical = c("momentum", "volatility", "trend"),
        statistical = c("stationarity", "normality", "autocorrelation")
      )
    },
    
    # 元数据分析
    analyze_metadata = function(data) {
      list(
        time_period = range(data$timestamp, na.rm = TRUE),
        total_observations = nrow(data),
        missing_values = sum(is.na(data)),
        completeness_ratio = 1 - sum(is.na(data)) / (nrow(data) * ncol(data)),
        numeric_columns = names(data)[sapply(data, is.numeric)],
        date_columns = names(data)[sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
      )
    },
    
    # 描述性统计
    descriptive_statistics = function(data) {
      numeric_data <- data[sapply(data, is.numeric)]
      
      if (ncol(numeric_data) == 0) {
        return(list())
      }
      
      stats_list <- list()
      
      for (col in names(numeric_data)) {
        vec <- numeric_data[[col]]
        vec <- vec[!is.na(vec)]
        
        if (length(vec) > 0) {
          stats_list[[col]] <- list(
            mean = mean(vec),
            median = median(vec),
            sd = sd(vec),
            mad = mad(vec),
            skewness = if (requireNamespace("e1071", quietly = TRUE)) e1071::skewness(vec) else NA,
            kurtosis = if (requireNamespace("e1071", quietly = TRUE)) e1071::kurtosis(vec) else NA,
            min = min(vec),
            max = max(vec),
            q1 = quantile(vec, 0.25),
            q3 = quantile(vec, 0.75),
            iqr = IQR(vec)
          )
        }
      }
      
      return(stats_list)
    },
    
    # 分布分析
    distribution_analysis = function(data) {
      numeric_data <- data[sapply(data, is.numeric)]
      distributions <- list()
      
      for (col in names(numeric_data)) {
        vec <- numeric_data[[col]]
        vec <- vec[!is.na(vec) & is.finite(vec)]
        
        if (length(vec) > 10) {  # 足够的数据点
          # 正态性检验
          normality_test <- tryCatch({
            shapiro.test(sample(vec, min(5000, length(vec))))  # Shapiro-Wilk限制5000个样本
          }, error = function(e) NULL)
          
          # 分布拟合
          distribution_fit <- private$fit_distributions(vec)
          
          distributions[[col]] <- list(
            normality_test = normality_test,
            distribution_fit = distribution_fit,
            histogram_data = hist(vec, plot = FALSE)
          )
        }
      }
      
      return(distributions)
    },
    
    # 拟合分布
    fit_distributions = function(data) {
      fits <- list()
      
      # 正态分布
      fits$normal <- list(
        mean = mean(data),
        sd = sd(data)
      )
      
      # t分布（如果安装了相关包）
      if (requireNamespace("MASS", quietly = TRUE)) {
        tryCatch({
          t_fit <- MASS::fitdistr(data, "t")
          fits$student_t <- as.list(t_fit$estimate)
        }, error = function(e) NULL)
      }
      
      return(fits)
    },
    
    # 相关性分析
    correlation_analysis = function(data) {
      numeric_data <- data[sapply(data, is.numeric)]
      
      if (ncol(numeric_data) < 2) {
        return(list())
      }
      
      correlation_matrix <- cor(numeric_data, use = "complete.obs")
      
      # 寻找高相关性对
      high_corr_pairs <- which(abs(correlation_matrix) > 0.7 & correlation_matrix < 1, arr.ind = TRUE)
      high_corr_list <- list()
      
      if (nrow(high_corr_pairs) > 0) {
        for (i in 1:nrow(high_corr_pairs)) {
          row_idx <- high_corr_pairs[i, 1]
          col_idx <- high_corr_pairs[i, 2]
          high_corr_list[[i]] <- list(
            variable1 = rownames(correlation_matrix)[row_idx],
            variable2 = colnames(correlation_matrix)[col_idx],
            correlation = correlation_matrix[row_idx, col_idx]
          )
        }
      }
      
      return(list(
        correlation_matrix = correlation_matrix,
        high_correlation_pairs = high_corr_list,
        average_correlation = mean(abs(correlation_matrix[lower.tri(correlation_matrix)]))
      ))
    },
    
    # 时间序列分析
    time_series_analysis = function(data) {
      if (!"close" %in% names(data)) {
        return(list())
      }
      
      prices <- data$close
      returns <- c(NA, diff(log(prices)))
      returns <- returns[!is.na(returns)]
      
      analysis <- list()
      
      # 平稳性检验
      if (requireNamespace("tseries", quietly = TRUE)) {
        analysis$stationarity_test <- tseries::adf.test(returns)
      }
      
      # 自相关分析
      analysis$autocorrelation <- list(
        returns_acf = acf(returns, plot = FALSE),
        squared_returns_acf = acf(returns^2, plot = FALSE)
      )
      
      # 季节性分析
      analysis$seasonality <- private$analyze_seasonality(data)
      
      return(analysis)
    },
    
    # 季节性分析
    analyze_seasonality = function(data) {
      if (!"timestamp" %in% names(data)) {
        return(list())
      }
      
      # 按月份、星期分析
      data$month <- lubridate::month(data$timestamp)
      data$weekday <- lubridate::wday(data$timestamp)
      
      monthly_returns <- tapply(data$close, data$month, function(x) {
        ret <- diff(log(x))
        mean(ret, na.rm = TRUE)
      })
      
      weekday_returns <- tapply(data$close, data$weekday, function(x) {
        ret <- diff(log(x))
        mean(ret, na.rm = TRUE)
      })
      
      return(list(
        monthly_pattern = monthly_returns,
        weekday_pattern = weekday_returns
      ))
    },
    
    # 波动率分析
    volatility_analysis = function(data) {
      if (!"close" %in% names(data)) {
        return(list())
      }
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      
      volatility_metrics <- list(
        historical_volatility = sd(returns) * sqrt(252),
        realized_volatility = private$calculate_realized_volatility(returns),
        volatility_clustering = private$test_volatility_clustering(returns),
        leverage_effect = private$test_leverage_effect(data$close)
      )
      
      return(volatility_metrics)
    },
    
    # 计算已实现波动率
    calculate_realized_volatility = function(returns, window = 21) {
      n <- length(returns)
      rv <- rep(NA, n)
      
      for (i in window:n) {
        rv[i] <- sqrt(sum(returns[(i-window+1):i]^2)) * sqrt(252)
      }
      
      return(rv)
    },
    
    # 测试波动率聚类
    test_volatility_clustering = function(returns) {
      # 使用Ljung-Box检验平方收益的自相关
      lb_test <- Box.test(returns^2, lag = 10, type = "Ljung-Box")
      
      return(list(
        ljung_box_statistic = lb_test$statistic,
        p_value = lb_test$p.value,
        has_clustering = lb_test$p.value < 0.05
      ))
    },
    
    # 测试杠杆效应
    test_leverage_effect = function(prices) {
      returns <- c(NA, diff(log(prices)))
      returns <- returns[!is.na(returns)]
      
      # 计算收益率与波动率的相关性
      volatility <- private$calculate_realized_volatility(returns, window = 5)
      valid_idx <- !is.na(volatility) & !is.na(returns)
      
      if (sum(valid_idx) > 10) {
        correlation <- cor(returns[valid_idx], volatility[valid_idx], use = "complete.obs")
      } else {
        correlation <- NA
      }
      
      return(list(
        correlation = correlation,
        has_leverage_effect = if (!is.na(correlation)) correlation < 0 else NA
      ))
    },
    
    # 市场状态分析
    regime_analysis = function(data) {
      if (!"close" %in% names(data)) {
        return(list())
      }
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      
      regimes <- list()
      
      # 简单的波动率状态识别
      volatility <- private$calculate_realized_volatility(returns, window = 20)
      volatility_quantiles <- quantile(volatility, probs = c(0.33, 0.67), na.rm = TRUE)
      
      regimes$volatility_regimes <- list(
        low_vol_threshold = volatility_quantiles[1],
        high_vol_threshold = volatility_quantiles[2],
        regime_proportions = table(cut(volatility, 
                                       breaks = c(-Inf, volatility_quantiles, Inf),
                                       labels = c("低波动", "中波动", "高波动"))) / length(volatility)
      )
      
      # 趋势状态识别
      price_trend <- private$identify_trend_regimes(data$close)
      regimes$trend_regimes <- price_trend
      
      return(regimes)
    },
    
    # 识别趋势状态
    identify_trend_regimes = function(prices, window = 50) {
      n <- length(prices)
      trends <- rep(NA, n)
      
      for (i in window:n) {
        window_prices <- prices[(i-window+1):i]
        trend_coef <- coef(lm(window_prices ~ seq_along(window_prices)))[2]
        trends[i] <- ifelse(trend_coef > 0, "上升", "下降")
      }
      
      return(table(trends, useNA = "ifany") / sum(!is.na(trends)))
    },
    
    # 收益率对比
    compare_returns = function(data_list, symbols) {
      return_stats <- list()
      
      for (i in seq_along(data_list)) {
        symbol <- symbols[i]
        data <- data_list[[i]]
        
        if ("close" %in% names(data)) {
          returns <- c(NA, diff(log(data$close)))
          returns <- returns[!is.na(returns)]
          
          return_stats[[symbol]] <- list(
            mean_return = mean(returns) * 252,
            annual_volatility = sd(returns) * sqrt(252),
            sharpe_ratio = mean(returns) / sd(returns) * sqrt(252),
            total_return = tail(data$close, 1) / head(data$close, 1) - 1
          )
        }
      }
      
      return(return_stats)
    },
    
    # 风险指标对比
    compare_risk_metrics = function(data_list, symbols) {
      risk_metrics <- list()
      
      for (i in seq_along(data_list)) {
        symbol <- symbols[i]
        data <- data_list[[i]]
        
        if ("close" %in% names(data)) {
          returns <- c(NA, diff(log(data$close)))
          returns <- returns[!is.na(returns)]
          
          risk_metrics[[symbol]] <- list(
            var_95 = quantile(returns, 0.05),
            cvar_95 = mean(returns[returns <= quantile(returns, 0.05)]),
            max_drawdown = private$calculate_max_drawdown(data$close),
            downside_deviation = private$calculate_downside_deviation(returns)
          )
        }
      }
      
      return(risk_metrics)
    },
    
    # 计算最大回撤
    calculate_max_drawdown = function(prices) {
      cumulative_max <- cummax(prices)
      drawdowns <- (prices - cumulative_max) / cumulative_max
      min(drawdowns, na.rm = TRUE)
    },
    
    # 计算下行偏差
    calculate_downside_deviation = function(returns, mar = 0) {
      downside_returns <- returns[returns < mar]
      if (length(downside_returns) > 0) {
        sqrt(mean(downside_returns^2))
      } else {
        0
      }
    },
    
    # 分析相关性结构
    analyze_correlation_structure = function(data_list, symbols) {
      n_assets <- length(data_list)
      correlation_matrix <- matrix(NA, n_assets, n_assets)
      rownames(correlation_matrix) <- symbols
      colnames(correlation_matrix) <- symbols
      
      return_matrix <- matrix(NA, n_assets, n_assets)
      
      for (i in 1:n_assets) {
        for (j in 1:n_assets) {
          if (i != j) {
            returns_i <- c(NA, diff(log(data_list[[i]]$close)))
            returns_j <- c(NA, diff(log(data_list[[j]]$close)))
            
            valid_idx <- !is.na(returns_i) & !is.na(returns_j)
            if (sum(valid_idx) > 10) {
              correlation_matrix[i, j] <- cor(returns_i[valid_idx], returns_j[valid_idx])
            }
          } else {
            correlation_matrix[i, j] <- 1
          }
        }
      }
      
      return(list(
        correlation_matrix = correlation_matrix,
        average_correlation = mean(correlation_matrix[lower.tri(correlation_matrix)], na.rm = TRUE),
        correlation_stability = private$test_correlation_stability(data_list, symbols)
      ))
    },
    
    # 测试相关性稳定性
    test_correlation_stability = function(data_list, symbols, window = 126) {
      # 计算滚动相关性
      n_assets <- length(data_list)
      rolling_correlations <- list()
      
      for (i in 1:(n_assets-1)) {
        for (j in (i+1):n_assets) {
          pair_name <- paste(symbols[i], symbols[j], sep = "_")
          returns_i <- c(NA, diff(log(data_list[[i]]$close)))
          returns_j <- c(NA, diff(log(data_list[[j]]$close)))
          
          valid_idx <- !is.na(returns_i) & !is.na(returns_j)
          if (sum(valid_idx) > window) {
            rolling_cor <- zoo::rollapply(
              data = cbind(returns_i[valid_idx], returns_j[valid_idx]),
              width = window,
              FUN = function(x) cor(x[,1], x[,2]),
              by.column = FALSE,
              align = "right"
            )
            rolling_correlations[[pair_name]] <- rolling_cor
          }
        }
      }
      
      # 计算相关性波动率作为稳定性指标
      stability <- sapply(rolling_correlations, sd, na.rm = TRUE)
      
      return(list(
        rolling_correlations = rolling_correlations,
        stability_metrics = stability,
        average_stability = mean(stability, na.rm = TRUE)
      ))
    },
    
    # 比较市场状态
    compare_market_regimes = function(data_list, symbols) {
      regimes_comparison <- list()
      
      for (i in seq_along(data_list)) {
        symbol <- symbols[i]
        regimes_comparison[[symbol]] <- private$regime_analysis(data_list[[i]])
      }
      
      return(regimes_comparison)
    },
    
    # 极端收益率检测
    detect_extreme_returns = function(data, symbol) {
      if (!"close" %in% names(data)) return(list())
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      
      threshold <- 3 * mad(returns)
      extreme_indices <- which(abs(returns) > threshold)
      
      return(list(
        extreme_indices = extreme_indices,
        extreme_dates = if ("timestamp" %in% names(data)) data$timestamp[extreme_indices + 1] else NULL,
        extreme_returns = returns[extreme_indices],
        threshold = threshold
      ))
    },
    
    # 波动率突变检测
    detect_volatility_breaks = function(data, symbol) {
      if (!"close" %in% names(data)) return(list())
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      
      # 使用CUSUM检测波动率变化
      squared_returns <- returns^2
      cusum <- cumsum(squared_returns - mean(squared_returns))
      
      break_points <- which(abs(cusum) == max(abs(cusum)))
      
      return(list(
        break_points = break_points,
        break_dates = if ("timestamp" %in% names(data)) data$timestamp[break_points + 1] else NULL,
        cusum_statistic = max(abs(cusum))
      ))
    },
    
    # 流动性异常检测
    detect_liquidity_anomalies = function(data, symbol) {
      if (!"volume" %in% names(data)) return(list())
      
      volume <- data$volume
      volume_changes <- c(NA, diff(log(volume)))
      volume_changes <- volume_changes[!is.na(volume_changes)]
      
      threshold <- 3 * mad(volume_changes)
      anomaly_indices <- which(abs(volume_changes) > threshold)
      
      return(list(
        anomaly_indices = anomaly_indices,
        anomaly_dates = if ("timestamp" %in% names(data)) data$timestamp[anomaly_indices + 1] else NULL,
        volume_changes = volume_changes[anomaly_indices],
        threshold = threshold
      ))
    },
    
    # 季节性模式检测
    detect_seasonal_patterns = function(data, symbol) {
      if (!"timestamp" %in% names(data)) return(list())
      
      # 分析月度效应
      data$month <- lubridate::month(data$timestamp)
      monthly_returns <- by(data, data$month, function(x) {
        if (nrow(x) > 1) {
          returns <- diff(log(x$close))
          mean(returns, na.rm = TRUE)
        } else {
          NA
        }
      })
      
      # 分析星期效应
      data$weekday <- lubridate::wday(data$timestamp)
      weekday_returns <- by(data, data$weekday, function(x) {
        if (nrow(x) > 1) {
          returns <- diff(log(x$close))
          mean(returns, na.rm = TRUE)
        } else {
          NA
        }
      })
      
      return(list(
        monthly_effects = as.list(monthly_returns),
        weekday_effects = as.list(weekday_returns)
      ))
    },
    
    # 绘图函数
    plot_price_series = function(data, symbol) {
      if (!"close" %in% names(data)) return(NULL)
      
      ggplot2::ggplot(data, ggplot2::aes(x = timestamp, y = close)) +
        ggplot2::geom_line(color = "steelblue") +
        ggplot2::labs(title = paste(symbol, "价格序列"), x = "日期", y = "价格") +
        ggplot2::theme_minimal()
    },
    
    plot_return_distribution = function(data, symbol) {
      if (!"close" %in% names(data)) return(NULL)
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      
      ggplot2::ggplot(data.frame(returns = returns), ggplot2::aes(x = returns)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 50, fill = "lightblue", alpha = 0.7) +
        ggplot2::geom_density(color = "darkblue") +
        ggplot2::labs(title = paste(symbol, "收益率分布"), x = "收益率", y = "密度") +
        ggplot2::theme_minimal()
    },
    
    plot_volatility_clustering = function(data, symbol) {
      if (!"close" %in% names(data)) return(NULL)
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      squared_returns <- returns^2
      
      acf_data <- acf(squared_returns, plot = FALSE)
      acf_df <- data.frame(
        lag = acf_data$lag,
        acf = acf_data$acf
      )
      
      ggplot2::ggplot(acf_df, ggplot2::aes(x = lag, y = acf)) +
        ggplot2::geom_col(fill = "orange", alpha = 0.7) +
        ggplot2::labs(title = paste(symbol, "波动率聚类(平方收益率ACF)"), x = "滞后", y = "自相关") +
        ggplot2::theme_minimal()
    },
    
    plot_autocorrelation = function(data, symbol) {
      if (!"close" %in% names(data)) return(NULL)
      
      returns <- c(NA, diff(log(data$close)))
      returns <- returns[!is.na(returns)]
      
      acf_data <- acf(returns, plot = FALSE)
      acf_df <- data.frame(
        lag = acf_data$lag,
        acf = acf_data$acf
      )
      
      ggplot2::ggplot(acf_df, ggplot2::aes(x = lag, y = acf)) +
        ggplot2::geom_col(fill = "green", alpha = 0.7) +
        ggplot2::labs(title = paste(symbol, "收益率自相关"), x = "滞后", y = "自相关") +
        ggplot2::theme_minimal()
    },
    
    # 生成执行摘要
    create_executive_summary = function(results) {
      summary <- list(
        analysis_date = results$timestamp,
        key_findings = list(),
        data_quality = results$metadata$completeness_ratio,
        recommendation_level = "NEUTRAL"  # 默认中性
      )
      
      # 从分析结果中提取关键发现
      if (!is.null(results$descriptive_stats)) {
        close_stats <- results$descriptive_stats$close
        if (!is.null(close_stats)) {
          summary$key_findings$return_characteristics <- list(
            annualized_return = close_stats$mean * 252,
            volatility = close_stats$sd * sqrt(252),
            sharpe_ratio = close_stats$mean / close_stats$sd * sqrt(252)
          )
        }
      }
      
      if (!is.null(results$volatility_analysis)) {
        summary$key_findings$volatility <- list(
          has_clustering = results$volatility_analysis$volatility_clustering$has_clustering,
          has_leverage_effect = results$volatility_analysis$leverage_effect$has_leverage_effect
        )
      }
      
      return(summary)
    },
    
    # 生成建议
    generate_recommendations = function(results) {
      recommendations <- list()
      
      # 基于分析结果生成策略建议
      if (!is.null(results$volatility_analysis)) {
        if (results$volatility_analysis$volatility_clustering$has_clustering) {
          recommendations <- c(recommendations, "考虑GARCH类模型进行波动率建模")
        }
      }
      
      if (!is.null(results$regime_analysis)) {
        if (results$regime_analysis$volatility_regimes$regime_proportions["高波动"] > 0.3) {
          recommendations <- c(recommendations, "考虑多状态模型或 regime-switching 策略")
        }
      }
      
      if (length(recommendations) == 0) {
        recommendations <- "数据特征正常，可考虑标准量化策略"
      }
      
      return(recommendations)
    }
  )
)