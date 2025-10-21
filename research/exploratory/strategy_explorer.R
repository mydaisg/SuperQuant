# research/exploratory/strategy_explorer.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
##' 策略探索分析器
StrategyExplorer <- R6::R6Class(
  "StrategyExplorer",
  public = list(
    initialize = function(backtest_engine, config_path = "config/strategy_exploration.yml") {
      private$backtest_engine <- backtest_engine
      private$exploration_config <- yaml::read_yaml(config_path)
    },
    
    # 探索策略参数空间
    explore_parameter_space = function(strategy, data, parameter_ranges, n_samples = 100) {
      cat("探索策略参数空间...\n")
      
      # 生成参数样本
      parameter_samples <- private$generate_parameter_samples(parameter_ranges, n_samples)
      
      results <- list()
      
      # 并行回测
      if (requireNamespace("future.apply", quietly = TRUE)) {
        plan(multisession)
        results$backtests <- future.apply::future_lapply(
          parameter_samples,
          function(params) {
            strategy_instance <- strategy$clone()
            strategy_instance$parameters <- params
            private$backtest_engine$run_backtest(strategy_instance, data)
          },
          future.seed = TRUE
        )
        plan(sequential)
      } else {
        results$backtests <- lapply(
          parameter_samples,
          function(params) {
            strategy_instance <- strategy$clone()
            strategy_instance$parameters <- params
            private$backtest_engine$run_backtest(strategy_instance, data)
          }
        )
      }
      
      # 分析结果
      results$analysis <- private$analyze_parameter_results(parameter_samples, results$backtests)
      
      return(results)
    },
    
    # 策略稳定性分析
    analyze_strategy_stability = function(strategy, data, time_periods) {
      cat("分析策略稳定性...\n")
      
      stability_results <- list()
      
      for (period_name in names(time_periods)) {
        period <- time_periods[[period_name]]
        cat("分析时期:", period_name, "\n")
        
        period_data <- data[
          data$timestamp >= period$start & data$timestamp <= period$end,
        ]
        
        backtest_result <- private$backtest_engine$run_backtest(
          strategy, period_data, period$start, period$end
        )
        
        stability_results[[period_name]] <- list(
          performance = backtest_result$performance,
          period = period,
          data_points = nrow(period_data)
        )
      }
      
      # 计算稳定性指标
      stability_metrics <- private$calculate_stability_metrics(stability_results)
      
      return(list(
        period_results = stability_results,
        stability_metrics = stability_metrics
      ))
    },
    
    # 策略对比分析
    compare_strategies = function(strategies, data, strategy_names = NULL) {
      cat("策略对比分析...\n")
      
      if (is.null(strategy_names)) {
        strategy_names <- paste0("Strategy_", seq_along(strategies))
      }
      
      comparison_results <- list()
      
      for (i in seq_along(strategies)) {
        strategy <- strategies[[i]]
        name <- strategy_names[i]
        
        cat("测试策略:", name, "\n")
        
        backtest_result <- private$backtest_engine$run_backtest(strategy, data)
        
        comparison_results[[name]] <- list(
          performance = backtest_result$performance,
          trades = nrow(backtest_result$trades),
          strategy_params = strategy$parameters
        )
      }
      
      # 排名和分析
      comparison_results$ranking <- private$rank_strategies(comparison_results)
      
      return(comparison_results)
    },
    
    # 生成策略探索报告
    generate_strategy_report = function(exploration_results, output_file = NULL) {
      cat("生成策略探索报告...\n")
      
      report <- list(
        exploration_summary = private$create_strategy_summary(exploration_results),
        detailed_results = exploration_results,
        recommendations = private$generate_strategy_recommendations(exploration_results),
        timestamp = Sys.time()
      )
      
      if (!is.null(output_file)) {
        saveRDS(report, output_file)
        cat("策略报告已保存至:", output_file, "\n")
      }
      
      return(report)
    }
  ),
  
  private = list(
    backtest_engine = NULL,
    exploration_config = NULL,
    
    # 生成参数样本
    generate_parameter_samples = function(parameter_ranges, n_samples) {
      samples <- list()
      
      for (i in 1:n_samples) {
        params <- list()
        for (param_name in names(parameter_ranges)) {
          range <- parameter_ranges[[param_name]]
          if (is.numeric(range)) {
            # 连续参数
            params[[param_name]] <- runif(1, range[1], range[2])
          } else if (is.character(range)) {
            # 分类参数
            params[[param_name]] <- sample(range, 1)
          } else if (is.integer(range)) {
            # 整数参数
            params[[param_name]] <- sample(range[1]:range[2], 1)
          }
        }
        samples[[i]] <- params
      }
      
      return(samples)
    },
    
    # 分析参数结果
    analyze_parameter_results = function(parameter_samples, backtest_results) {
      analysis <- list()
      
      # 提取绩效指标
      performance_metrics <- lapply(backtest_results, function(result) {
        result$performance
      })
      
      # 创建参数-绩效数据框
      param_perf_df <- data.frame()
      
      for (i in seq_along(parameter_samples)) {
        params <- parameter_samples[[i]]
        perf <- performance_metrics[[i]]
        
        row_data <- as.data.frame(params)
        row_data$sharpe_ratio <- perf$sharpe_ratio
        row_data$total_return <- perf$total_return
        row_data$max_drawdown <- perf$max_drawdown
        
        param_perf_df <- rbind(param_perf_df, row_data)
      }
      
      analysis$parameter_performance <- param_perf_df
      
      # 参数重要性分析
      analysis$parameter_importance <- private$analyze_parameter_importance(param_perf_df)
      
      # 最优参数识别
      analysis$optimal_parameters <- private$identify_optimal_parameters(param_perf_df)
      
      return(analysis)
    },
    
    # 分析参数重要性
    analyze_parameter_importance = function(param_perf_df) {
      # 移除绩效列
      perf_metrics <- c("sharpe_ratio", "total_return", "max_drawdown")
      param_cols <- setdiff(names(param_perf_df), perf_metrics)
      
      importance_analysis <- list()
      
      for (metric in perf_metrics) {
        # 对每个绩效指标，计算参数的相关性
        correlations <- sapply(param_cols, function(param) {
          if (is.numeric(param_perf_df[[param]])) {
            cor(param_perf_df[[param]], param_perf_df[[metric]], use = "complete.obs")
          } else {
            # 对于分类变量，使用方差分析
            anova_result <- aov(param_perf_df[[metric]] ~ factor(param_perf_df[[param]]))
            summary(anova_result)[[1]]$"F value"[1]
          }
        })
        
        importance_analysis[[metric]] <- sort(correlations, decreasing = TRUE)
      }
      
      return(importance_analysis)
    },
    
    # 识别最优参数
    identify_optimal_parameters = function(param_perf_df) {
      optimal <- list()
      
      # 按夏普比率排序
      sharpe_optimal <- param_perf_df[which.max(param_perf_df$sharpe_ratio), ]
      optimal$by_sharpe <- as.list(sharpe_optimal)
      
      # 按总收益排序
      return_optimal <- param_perf_df[which.max(param_perf_df$total_return), ]
      optimal$by_return <- as.list(return_optimal)
      
      # 按回撤控制排序（夏普比率正且回撤小）
      valid_sharpe <- param_perf_df[param_perf_df$sharpe_ratio > 0, ]
      if (nrow(valid_sharpe) > 0) {
        drawdown_optimal <- valid_sharpe[which.min(valid_sharpe$max_drawdown), ]
        optimal$by_drawdown <- as.list(drawdown_optimal)
      }
      
      return(optimal)
    },
    
    # 计算稳定性指标
    calculate_stability_metrics = function(stability_results) {
      periods <- names(stability_results)
      n_periods <- length(periods)
      
      # 提取各时期的夏普比率
      sharpe_ratios <- sapply(stability_results, function(x) x$performance$sharpe_ratio)
      total_returns <- sapply(stability_results, function(x) x$performance$total_return)
      
      metrics <- list(
        sharpe_stability = sd(sharpe_ratios) / mean(sharpe_ratios),
        return_stability = sd(total_returns) / mean(total_returns),
        positive_periods = sum(total_returns > 0) / n_periods,
        consistency_score = private$calculate_consistency_score(stability_results)
      )
      
      return(metrics)
    },
    
    # 计算一致性得分
    calculate_consistency_score = function(stability_results) {
      # 基于各时期排名的稳定性
      performances <- sapply(stability_results, function(x) x$performance$sharpe_ratio)
      ranks <- rank(performances)
      
      # 排名变化越小，一致性越高
      1 - (sd(ranks) / (length(ranks) - 1))
    },
    
    # 策略排名
    rank_strategies = function(comparison_results) {
      strategies <- names(comparison_results)
      if ("ranking" %in% strategies) {
        strategies <- setdiff(strategies, "ranking")
      }
      
      # 创建排名数据框
      ranking_df <- data.frame(
        strategy = strategies,
        sharpe_ratio = sapply(strategies, function(s) comparison_results[[s]]$performance$sharpe_ratio),
        total_return = sapply(strategies, function(s) comparison_results[[s]]$performance$total_return),
        max_drawdown = sapply(strategies, function(s) comparison_results[[s]]$performance$max_drawdown),
        trades = sapply(strategies, function(s) comparison_results[[s]]$trades)
      )
      
      # 按夏普比率排序
      ranking_df <- ranking_df[order(-ranking_df$sharpe_ratio), ]
      ranking_df$sharpe_rank <- 1:nrow(ranking_df)
      
      # 按总收益排序
      return_rank <- order(-ranking_df$total_return)
      ranking_df$return_rank <- return_rank
      
      # 综合排名（等权重）
      ranking_df$composite_rank <- (ranking_df$sharpe_rank + ranking_df$return_rank) / 2
      ranking_df <- ranking_df[order(ranking_df$composite_rank), ]
      
      return(ranking_df)
    },
    
    # 创建策略摘要
    create_strategy_summary = function(exploration_results) {
      summary <- list(
        analysis_date = Sys.time(),
        n_strategies_tested = if (!is.null(exploration_results$ranking)) {
          nrow(exploration_results$ranking)
        } else {
          length(exploration_results) - 1  # 减去ranking元素
        },
        best_strategy = if (!is.null(exploration_results$ranking)) {
          as.list(exploration_results$ranking[1, ])
        } else {
          NULL
        }
      )
      
      return(summary)
    },
    
    # 生成策略建议
    generate_strategy_recommendations = function(exploration_results) {
      recommendations <- list()
      
      if (!is.null(exploration_results$ranking)) {
        best_strategy <- exploration_results$ranking[1, ]
        
        recommendations <- c(
          recommendations,
          paste("推荐策略:", best_strategy$strategy),
          paste("夏普比率:", round(best_strategy$sharpe_ratio, 3)),
          paste("总收益:", round(best_strategy$total_return * 100, 2), "%")
        )
      }
      
      if (!is.null(exploration_results$stability_metrics)) {
        stability <- exploration_results$stability_metrics
        if (stability$sharpe_stability < 0.5) {
          recommendations <- c(recommendations, "策略表现稳定，适合实盘")
        } else {
          recommendations <- c(recommendations, "策略表现不稳定，需要进一步优化")
        }
      }
      
      return(recommendations)
    }
  )
)