# strategies/alpha_research/alpha_research_engine.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' Alpha因子研究引擎
AlphaResearchEngine <- R6::R6Class(
  "AlphaResearchEngine",
  public = list(
    initialize = function(config_path = "config/alpha_research.yml") {
      private$research_config <- yaml::read_yaml(config_path)
      private$initialize_factor_library()
      private$initialize_evaluation_metrics()
    },
    
    # Alpha因子挖掘
    mine_alpha_factors = function(data, factor_types = NULL, target_column = "future_returns") {
      cat("开始Alpha因子挖掘...\n")
      
      if (is.null(factor_types)) {
        factor_types <- private$research_config$default_factor_types
      }
      
      factors <- list()
      performance <- list()
      
      # 生成各类因子
      for (factor_type in factor_types) {
        cat("生成", factor_type, "类型因子...\n")
        factors[[factor_type]] <- private$generate_factors_by_type(data, factor_type)
      }
      
      # 因子绩效评估
      for (factor_type in names(factors)) {
        cat("评估", factor_type, "因子绩效...\n")
        performance[[factor_type]] <- private$evaluate_factors(
          factors[[factor_type]], data, target_column
        )
      }
      
      # 因子组合与优化
      optimal_factors <- private$select_optimal_factors(performance)
      
      return(list(
        factors = factors,
        performance = performance,
        optimal_factors = optimal_factors,
        summary = private$generate_alpha_summary(performance)
      ))
    },
    
    # 因子有效性检验
    validate_factor_efficacy = function(factors, data, target_column = "future_returns", 
                                        test_periods = NULL) {
      cat("进行因子有效性检验...\n")
      
      if (is.null(test_periods)) {
        test_periods <- private$research_config$validation_periods
      }
      
      validation_results <- list()
      
      for (period_name in names(test_periods)) {
        period <- test_periods[[period_name]]
        cat("检验时期:", period_name, "\n")
        
        period_data <- data[
          data$timestamp >= period$start & data$timestamp <= period$end,
        ]
        
        period_factors <- factors
        # 确保因子数据与时期匹配
        for (factor_type in names(period_factors)) {
          period_factors[[factor_type]] <- period_factors[[factor_type]][
            period_factors[[factor_type]]$timestamp >= period$start & 
              period_factors[[factor_type]]$timestamp <= period$end,
          ]
        }
        
        validation_results[[period_name]] <- private$evaluate_factors(
          period_factors, period_data, target_column
        )
      }
      
      # 计算稳定性指标
      stability_metrics <- private$calculate_factor_stability(validation_results)
      
      return(list(
        period_results = validation_results,
        stability_metrics = stability_metrics,
        overall_efficacy = private$assess_overall_efficacy(validation_results)
      ))
    },
    
    # 因子组合优化
    optimize_factor_combination = function(factors, data, target_column = "future_returns", 
                                           optimization_method = "regression") {
      cat("优化因子组合...\n")
      
      # 准备因子数据
      factor_data <- private$prepare_factor_data(factors, data, target_column)
      
      # 根据优化方法选择组合策略
      combination_result <- switch(optimization_method,
                                   regression = private$regression_based_combination(factor_data),
                                   ic_weighted = private$ic_weighted_combination(factor_data),
                                   machine_learning = private$ml_based_combination(factor_data),
                                   stop("未知的优化方法: ", optimization_method)
      )
      
      # 验证组合效果
      validation <- private$validate_combination(combination_result, factor_data)
      
      return(list(
        combination_weights = combination_result$weights,
        combined_factor = combination_result$combined_factor,
        performance = combination_result$performance,
        validation = validation,
        optimization_method = optimization_method
      ))
    },
    
    # 生成Alpha研究报告
    generate_alpha_report = function(research_results, output_file = NULL) {
      cat("生成Alpha研究报告...\n")
      
      report <- list(
        research_summary = private$create_alpha_summary(research_results),
        factor_performance = research_results$performance,
        optimal_factors = research_results$optimal_factors,
        recommendations = private$generate_alpha_recommendations(research_results),
        research_timestamp = Sys.time()
      )
      
      if (!is.null(output_file)) {
        saveRDS(report, output_file)
        cat("Alpha研究报告已保存至:", output_file, "\n")
      }
      
      return(report)
    },
    
    # 实时Alpha监控
    monitor_alpha_decay = function(factors, data, window = 126) {
      cat("监控Alpha衰减...\n")
      
      decay_metrics <- list()
      
      for (factor_type in names(factors)) {
        factor_data <- factors[[factor_type]]
        decay_metrics[[factor_type]] <- private$calculate_alpha_decay(
          factor_data, data, window
        )
      }
      
      return(list(
        decay_metrics = decay_metrics,
        summary = private$summarize_alpha_decay(decay_metrics)
      ))
    }
  ),
  
  private = list(
    research_config = NULL,
    factor_library = list(),
    evaluation_metrics = list(),
    
    initialize_factor_library = function() {
      private$factor_library <- list(
        price_based = private$research_config$price_based_factors,
        volume_based = private$research_config$volume_based_factors,
        volatility_based = private$research_config$volatility_based_factors,
        fundamental_based = private$research_config$fundamental_based_factors
      )
    },
    
    initialize_evaluation_metrics = function() {
      private$evaluation_metrics <- list(
        primary = c("ic", "ir", "ic_ir"),
        risk_adjusted = c("sharpe", "max_drawdown", "calmar"),
        statistical = c("t_stat", "p_value", "r_squared")
      )
    },
    
    # 按类型生成因子
    generate_factors_by_type = function(data, factor_type) {
      switch(factor_type,
             price_based = private$generate_price_factors(data),
             volume_based = private$generate_volume_factors(data),
             volatility_based = private$generate_volatility_factors(data),
             fundamental_based = private$generate_fundamental_factors(data),
             technical = private$generate_technical_factors(data),
             stop("未知的因子类型: ", factor_type)
      )
    },
    
    # 价格类因子
    generate_price_factors = function(data) {
      factors <- list()
      
      # 动量因子
      factors$momentum_1m <- TTR::ROC(data$close, n = 21)
      factors$momentum_3m <- TTR::ROC(data$close, n = 63)
      factors$momentum_12m <- TTR::ROC(data$close, n = 252)
      
      # 均值回归因子
      factors$mean_reversion_5d <- private$calculate_mean_reversion(data$close, 5)
      factors$mean_reversion_20d <- private$calculate_mean_reversion(data$close, 20)
      
      # 价格形态因子
      factors$price_vs_high_20d <- data$close / zoo::rollmax(data$close, 20, fill = NA, align = "right")
      factors$price_vs_low_20d <- data$close / zoo::rollmin(data$close, 20, fill = NA, align = "right")
      
      # 收益偏度因子
      factors$return_skewness_20d <- zoo::rollapply(
        c(NA, diff(log(data$close))), 20, 
        function(x) e1071::skewness(x, na.rm = TRUE), 
        fill = NA, align = "right"
      )
      
      return(factors)
    },
    
    # 成交量类因子
    generate_volume_factors = function(data) {
      factors <- list()
      
      if (!"volume" %in% names(data)) {
        warning("缺少成交量数据，跳过成交量因子生成")
        return(factors)
      }
      
      # 量价背离因子
      factors$volume_price_divergence <- private$calculate_volume_price_divergence(data)
      
      # 成交量动量
      factors$volume_momentum_5d <- TTR::ROC(data$volume, n = 5)
      factors$volume_momentum_20d <- TTR::ROC(data$volume, n = 20)
      
      # 成交量波动率
      factors$volume_volatility_20d <- zoo::rollapply(
        data$volume, 20, sd, fill = NA, align = "right"
      )
      
      # 聪明钱指标
      factors$smart_money_indicator <- private$calculate_smart_money_indicator(data)
      
      return(factors)
    },
    
    # 波动率类因子
    generate_volatility_factors = function(data) {
      factors <- list()
      
      returns <- c(NA, diff(log(data$close)))
      
      # 历史波动率
      factors$historical_vol_20d <- zoo::rollapply(
        returns, 20, sd, fill = NA, align = "right"
      ) * sqrt(252)
      
      # 已实现波动率
      factors$realized_vol_5d <- private$calculate_realized_volatility(returns, 5)
      factors$realized_vol_20d <- private$calculate_realized_volatility(returns, 20)
      
      # 波动率风险溢价
      factors$volatility_risk_premium <- private$calculate_volatility_risk_premium(data)
      
      # 波动率偏度
      factors$volatility_skewness_20d <- zoo::rollapply(
        returns, 20, 
        function(x) e1071::skewness(x, na.rm = TRUE), 
        fill = NA, align = "right"
      )
      
      return(factors)
    },
    
    # 基本面因子（需要额外数据）
    generate_fundamental_factors = function(data) {
      factors <- list()
      
      # 这里需要接入基本面数据
      # 目前返回空列表，实际使用时应实现具体逻辑
      
      return(factors)
    },
    
    # 技术指标因子
    generate_technical_factors = function(data) {
      factors <- list()
      
      # RSI
      factors$rsi_14 <- TTR::RSI(data$close, n = 14)
      
      # MACD
      macd <- TTR::MACD(data$close, nFast = 12, nSlow = 26, nSig = 9)
      factors$macd <- macd$macd
      factors$macd_signal <- macd$signal
      
      # 布林带位置
      bb <- TTR::BBands(data$close, n = 20, sd = 2)
      factors$bollinger_position <- (data$close - bb$dn) / (bb$up - bb$dn)
      
      # ATR标准化
      atr <- TTR::ATR(data[, c("high", "low", "close")], n = 14)
      factors$atr_normalized <- atr$atr / data$close
      
      return(factors)
    },
    
    # 计算均值回归因子
    calculate_mean_reversion = function(prices, lookback) {
      sma <- TTR::SMA(prices, n = lookback)
      (prices - sma) / zoo::rollapply(prices, lookback, sd, fill = NA, align = "right")
    },
    
    # 计算量价背离
    calculate_volume_price_divergence = function(data) {
      price_roc <- TTR::ROC(data$close, n = 1)
      volume_roc <- TTR::ROC(data$volume, n = 1)
      
      # 价格上涨但成交量下降，或价格下跌但成交量上升
      divergence <- ifelse(
        price_roc > 0 & volume_roc < 0, -1,
        ifelse(price_roc < 0 & volume_roc > 0, 1, 0)
      )
      
      return(divergence)
    },
    
    # 计算聪明钱指标
    calculate_smart_money_indicator = function(data) {
      # 简化的聪明钱指标：大单净流入
      # 实际应用中需要level2数据
      if (!"large_volume" %in% names(data)) {
        return(rep(NA, nrow(data)))
      }
      
      large_volume_ratio <- data$large_volume / data$volume
      price_change <- c(NA, diff(log(data$close)))
      
      # 聪明钱在下跌时买入，上涨时卖出
      smart_money <- ifelse(
        price_change < 0, large_volume_ratio,
        ifelse(price_change > 0, -large_volume_ratio, 0)
      )
      
      return(smart_money)
    },
    
    # 计算已实现波动率
    calculate_realized_volatility = function(returns, window) {
      rv <- rep(NA, length(returns))
      
      for (i in window:length(returns)) {
        rv[i] <- sqrt(sum(returns[(i-window+1):i]^2, na.rm = TRUE)) * sqrt(252)
      }
      
      return(rv)
    },
    
    # 计算波动率风险溢价
    calculate_volatility_risk_premium = function(data) {
      # 简化实现：历史波动率与隐含波动率之差
      # 实际应用中需要期权数据
      historical_vol <- private$calculate_realized_volatility(
        c(NA, diff(log(data$close))), 20
      )
      
      # 假设隐含波动率（这里用常数代替）
      implied_vol <- 0.2  # 20%年化波动率
      
      premium <- historical_vol - implied_vol
      return(premium)
    },
    
    # 评估因子绩效
    evaluate_factors = function(factors, data, target_column) {
      performance <- list()
      
      for (factor_name in names(factors)) {
        factor_values <- factors[[factor_name]]
        
        # 确保长度匹配
        if (length(factor_values) != nrow(data)) {
          warning("因子", factor_name, "长度不匹配，跳过评估")
          next
        }
        
        # 计算IC（信息系数）
        ic_metrics <- private$calculate_information_coefficient(
          factor_values, data[[target_column]]
        )
        
        # 计算因子收益率
        factor_returns <- private$calculate_factor_returns(
          factor_values, data[[target_column]]
        )
        
        # 计算风险调整后收益
        risk_metrics <- private$calculate_risk_metrics(factor_returns)
        
        performance[[factor_name]] <- list(
          ic_metrics = ic_metrics,
          factor_returns = factor_returns,
          risk_metrics = risk_metrics,
          evaluation = private$evaluate_factor_quality(ic_metrics, risk_metrics)
        )
      }
      
      return(performance)
    },
    
    # 计算信息系数
    calculate_information_coefficient = function(factor_values, target_returns) {
      # 移除NA值
      valid_idx <- !is.na(factor_values) & !is.na(target_returns)
      factor_clean <- factor_values[valid_idx]
      target_clean <- target_returns[valid_idx]
      
      if (length(factor_clean) < 10) {
        return(list(ic = NA, ic_ir = NA, ic_pvalue = NA))
      }
      
      # 计算Rank IC
      ic <- cor(factor_clean, target_clean, use = "complete.obs", method = "spearman")
      
      # 计算ICIR（信息比率）
      # 这里简化处理，实际应该计算滚动IC的标准差
      ic_ir <- if (!is.na(ic)) ic / (1 / sqrt(length(factor_clean))) else NA
      
      # 计算显著性
      ic_test <- cor.test(factor_clean, target_clean, method = "spearman")
      
      return(list(
        ic = ic,
        ic_ir = ic_ir,
        ic_pvalue = ic_test$p.value,
        ic_significant = ic_test$p.value < 0.05
      ))
    },
    
    # 计算因子收益率
    calculate_factor_returns = function(factor_values, target_returns) {
      # 简单的因子组合收益率计算
      # 实际应用中应该构建多空组合
      valid_idx <- !is.na(factor_values) & !is.na(target_returns)
      factor_clean <- factor_values[valid_idx]
      target_clean <- target_returns[valid_idx]
      
      if (length(factor_clean) < 10) {
        return(NA)
      }
      
      # 因子方向（假设因子值越大，预期收益越高）
      factor_direction <- sign(factor_clean)
      factor_return <- mean(factor_direction * target_clean, na.rm = TRUE)
      
      return(factor_return)
    },
    
    # 计算风险指标
    calculate_risk_metrics = function(factor_returns) {
      if (is.na(factor_returns) || length(factor_returns) == 0) {
        return(list(sharpe = NA, max_drawdown = NA))
      }
      
      # 简化处理，实际应该基于因子组合的收益率序列
      sharpe <- factor_returns / sd(factor_returns, na.rm = TRUE) * sqrt(252)
      max_drawdown <- -0.1  # 简化处理
      
      return(list(
        sharpe = sharpe,
        max_drawdown = max_drawdown
      ))
    },
    
    # 评估因子质量
    evaluate_factor_quality = function(ic_metrics, risk_metrics) {
      quality_score <- 0
      
      # IC显著性
      if (!is.na(ic_metrics$ic_significant) && ic_metrics$ic_significant) {
        quality_score <- quality_score + 3
      }
      
      # IC绝对值
      if (!is.na(ic_metrics$ic) && abs(ic_metrics$ic) > 0.05) {
        quality_score <- quality_score + 2
      }
      
      # 夏普比率
      if (!is.na(risk_metrics$sharpe) && risk_metrics$sharpe > 0.5) {
        quality_score <- quality_score + 2
      }
      
      return(list(
        quality_score = quality_score,
        quality_grade = if (quality_score >= 5) "A" else if (quality_score >= 3) "B" else "C"
      ))
    },
    
    # 选择最优因子
    select_optimal_factors = function(performance) {
      optimal_factors <- list()
      
      for (factor_type in names(performance)) {
        type_performance <- performance[[factor_type]]
        best_score <- -Inf
        best_factor <- NULL
        
        for (factor_name in names(type_performance)) {
          factor_perf <- type_performance[[factor_name]]
          score <- factor_perf$evaluation$quality_score
          
          if (!is.na(score) && score > best_score) {
            best_score <- score
            best_factor <- factor_name
          }
        }
        
        if (!is.null(best_factor)) {
          optimal_factors[[factor_type]] <- list(
            factor_name = best_factor,
            performance = type_performance[[best_factor]],
            quality_score = best_score
          )
        }
      }
      
      return(optimal_factors)
    },
    
    # 准备因子数据
    prepare_factor_data = function(factors, data, target_column) {
      # 将所有因子合并为一个数据框
      factor_df <- data.frame(timestamp = data$timestamp)
      
      for (factor_type in names(factors)) {
        type_factors <- factors[[factor_type]]
        for (factor_name in names(type_factors)) {
          factor_df[[paste(factor_type, factor_name, sep = "_")]] <- type_factors[[factor_name]]
        }
      }
      
      # 添加目标变量
      factor_df[[target_column]] <- data[[target_column]]
      
      # 移除包含NA的行
      factor_df <- factor_df[complete.cases(factor_df), ]
      
      return(factor_df)
    },
    
    # 基于回归的因子组合
    regression_based_combination = function(factor_data) {
      # 提取因子和目标变量
      factor_cols <- setdiff(names(factor_data), c("timestamp", "future_returns"))
      X <- as.matrix(factor_data[, factor_cols])
      y <- factor_data$future_returns
      
      # 线性回归
      model <- lm(y ~ X)
      
      # 使用系数作为权重
      weights <- coef(model)[-1]  # 移除截距项
      names(weights) <- factor_cols
      
      # 计算组合因子
      combined_factor <- as.numeric(X %*% weights)
      
      return(list(
        weights = weights,
        combined_factor = combined_factor,
        performance = summary(model)$r.squared
      ))
    },
    
    # IC加权组合
    ic_weighted_combination = function(factor_data) {
      factor_cols <- setdiff(names(factor_data), c("timestamp", "future_returns"))
      X <- factor_data[, factor_cols]
      y <- factor_data$future_returns
      
      # 计算每个因子的IC
      ics <- sapply(X, function(x) cor(x, y, use = "complete.obs", method = "spearman"))
      
      # 使用IC绝对值作为权重
      weights <- abs(ics) / sum(abs(ics), na.rm = TRUE)
      
      # 计算组合因子
      combined_factor <- as.numeric(as.matrix(X) %*% weights)
      
      return(list(
        weights = weights,
        combined_factor = combined_factor,
        performance = mean(abs(ics), na.rm = TRUE)
      ))
    },
    
    # 基于机器学习的组合
    ml_based_combination = function(factor_data) {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop("需要安装glmnet包进行机器学习组合")
      }
      
      factor_cols <- setdiff(names(factor_data), c("timestamp", "future_returns"))
      X <- as.matrix(factor_data[, factor_cols])
      y <- factor_data$future_returns
      
      # 使用LASSO回归进行因子选择和组合
      cv_model <- glmnet::cv.glmnet(X, y, alpha = 1)
      best_lambda <- cv_model$lambda.min
      
      # 获取系数
      weights <- as.numeric(coef(cv_model, s = best_lambda))[-1]  # 移除截距
      names(weights) <- factor_cols
      
      # 计算组合因子
      combined_factor <- as.numeric(X %*% weights)
      
      return(list(
        weights = weights,
        combined_factor = combined_factor,
        performance = 1 - cv_model$cvm[which(cv_model$lambda == best_lambda)] / var(y)
      ))
    },
    
    # 验证组合效果
    validate_combination = function(combination_result, factor_data) {
      combined_factor <- combination_result$combined_factor
      actual_returns <- factor_data$future_returns
      
      # 计算组合因子的IC
      combination_ic <- cor(combined_factor, actual_returns, 
                            use = "complete.obs", method = "spearman")
      
      return(list(
        combination_ic = combination_ic,
        improvement_vs_single = combination_ic / mean(
          sapply(factor_data[, setdiff(names(factor_data), 
                                       c("timestamp", "future_returns"))], 
                 function(x) abs(cor(x, actual_returns, use = "complete.obs", 
                                     method = "spearman"))),
          na.rm = TRUE
        )
      ))
    },
    
    # 计算因子稳定性
    calculate_factor_stability = function(validation_results) {
      stability_metrics <- list()
      
      # 提取各时期的IC
      period_ics <- list()
      for (period_name in names(validation_results)) {
        period_ics[[period_name]] <- sapply(
          validation_results[[period_name]], 
          function(x) x$ic_metrics$ic
        )
      }
      
      # 计算IC稳定性（标准差）
      all_factors <- unique(unlist(lapply(period_ics, names)))
      
      for (factor in all_factors) {
        factor_ics <- sapply(period_ics, function(x) x[factor])
        stability_metrics[[factor]] <- sd(unlist(factor_ics), na.rm = TRUE)
      }
      
      return(stability_metrics)
    },
    
    # 评估整体有效性
    assess_overall_efficacy = function(validation_results) {
      # 计算平均IC和稳定性
      all_ics <- list()
      for (period in validation_results) {
        for (factor in names(period)) {
          all_ics[[factor]] <- c(all_ics[[factor]], period[[factor]]$ic_metrics$ic)
        }
      }
      
      efficacy_scores <- sapply(all_ics, function(x) {
        mean_ic <- mean(abs(x), na.rm = TRUE)
        ic_stability <- sd(x, na.rm = TRUE)
        mean_ic / (ic_stability + 0.001)  # 避免除零
      })
      
      return(list(
        efficacy_scores = efficacy_scores,
        top_factors = names(sort(efficacy_scores, decreasing = TRUE))[1:5]
      ))
    },
    
    # 计算Alpha衰减
    calculate_alpha_decay = function(factors, data, window) {
      decay_metrics <- list()
      
      n_periods <- floor(nrow(data) / window)
      
      for (i in 1:n_periods) {
        start_idx <- (i - 1) * window + 1
        end_idx <- min(i * window, nrow(data))
        
        period_data <- data[start_idx:end_idx, ]
        period_factors <- lapply(factors, function(x) x[start_idx:end_idx])
        
        period_performance <- private$evaluate_factors(
          period_factors, period_data, "future_returns"
        )
        
        decay_metrics[[paste("period", i)]] <- period_performance
      }
      
      # 计算衰减率
      decay_rates <- private$calculate_decay_rates(decay_metrics)
      
      return(list(
        period_metrics = decay_metrics,
        decay_rates = decay_rates
      ))
    },
    
    # 计算衰减率
    calculate_decay_rates = function(decay_metrics) {
      n_periods <- length(decay_metrics)
      
      decay_rates <- list()
      for (factor_type in names(decay_metrics[[1]])) {
        ics <- sapply(decay_metrics, function(x) x[[factor_type]]$ic_metrics$ic)
        
        if (length(ics) > 1 && !all(is.na(ics))) {
          # 线性衰减模型
          time_points <- 1:length(ics)
          decay_model <- lm(ics ~ time_points)
          decay_rate <- coef(decay_model)[2]  # 斜率
        } else {
          decay_rate <- NA
        }
        
        decay_rates[[factor_type]] <- decay_rate
      }
      
      return(decay_rates)
    },
    
    # 汇总Alpha衰减
    summarize_alpha_decay = function(decay_metrics) {
      summary <- list()
      
      for (factor_type in names(decay_metrics)) {
        decay_rates <- decay_metrics[[factor_type]]$decay_rates
        
        summary[[factor_type]] <- list(
          average_decay_rate = mean(unlist(decay_rates), na.rm = TRUE),
          decaying_factors = sum(sapply(decay_rates, function(x) !is.na(x) && x < 0)),
          stable_factors = sum(sapply(decay_rates, function(x) !is.na(x) && abs(x) < 0.01))
        )
      }
      
      return(summary)
    },
    
    # 创建Alpha摘要
    create_alpha_summary = function(research_results) {
      summary <- list(
        research_date = Sys.time(),
        n_factors_tested = length(unlist(research_results$factors)),
        best_factors = research_results$optimal_factors,
        overall_quality = private$calculate_overall_quality(research_results$performance)
      )
      
      return(summary)
    },
    
    # 计算整体质量
    calculate_overall_quality = function(performance) {
      quality_scores <- c()
      
      for (factor_type in names(performance)) {
        for (factor_name in names(performance[[factor_type]])) {
          score <- performance[[factor_type]][[factor_name]]$evaluation$quality_score
          if (!is.na(score)) {
            quality_scores <- c(quality_scores, score)
          }
        }
      }
      
      return(list(
        mean_quality_score = mean(quality_scores, na.rm = TRUE),
        high_quality_ratio = sum(quality_scores >= 5) / length(quality_scores)
      ))
    },
    
    # 生成Alpha建议
    generate_alpha_recommendations = function(research_results) {
      recommendations <- list()
      
      optimal_factors <- research_results$optimal_factors
      
      for (factor_type in names(optimal_factors)) {
        factor_info <- optimal_factors[[factor_type]]
        recommendations <- c(
          recommendations,
          paste(factor_type, "最佳因子:", factor_info$factor_name, 
                "(质量分数:", factor_info$quality_score, ")")
        )
      }
      
      # 基于整体质量的建议
      overall_quality <- private$calculate_overall_quality(research_results$performance)
      if (overall_quality$mean_quality_score > 4) {
        recommendations <- c(recommendations, "整体因子质量良好，建议进行实盘测试")
      } else {
        recommendations <- c(recommendations, "因子质量一般，建议继续挖掘新因子")
      }
      
      return(recommendations)
    },
    
    # 生成Alpha摘要
    generate_alpha_summary = function(performance) {
      summary <- list(
        total_factors = length(unlist(performance)),
        high_quality_count = sum(sapply(performance, function(x) 
          sum(sapply(x, function(y) y$evaluation$quality_score >= 5))),
          average_ic = mean(sapply(performance, function(x) 
            mean(sapply(x, function(y) abs(y$ic_metrics$ic)), na.rm = TRUE)),
            research_timestamp = Sys.time()
          )))
          
          return(summary)
    }
  ))