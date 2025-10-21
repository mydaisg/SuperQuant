# research/optimization/parameter_optimizer.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
##' 参数优化引擎
ParameterOptimizer <- R6::R6Class(
  "ParameterOptimizer",
  public = list(
    initialize = function(backtest_engine, config_path = "config/optimization.yml") {
      private$backtest_engine <- backtest_engine
      private$optimization_config <- yaml::read_yaml(config_path)
      private$initialize_optimization_methods()
    },
    
    # 网格搜索优化
    grid_search = function(strategy, data, parameter_grid, objective_function = "sharpe_ratio") {
      cat("开始网格搜索优化...\n")
      
      # 生成所有参数组合
      parameter_combinations <- private$generate_grid_combinations(parameter_grid)
      
      results <- list()
      performance_metrics <- list()
      
      # 遍历所有参数组合
      for (i in seq_along(parameter_combinations)) {
        params <- parameter_combinations[[i]]
        cat(sprintf("测试参数组合 %d/%d\n", i, length(parameter_combinations)))
        
        strategy_instance <- strategy$clone()
        strategy_instance$parameters <- params
        
        backtest_result <- private$backtest_engine$run_backtest(strategy_instance, data)
        
        # 存储结果
        results[[i]] <- list(
          parameters = params,
          backtest_result = backtest_result
        )
        
        performance_metrics[[i]] <- backtest_result$performance[[objective_function]]
      }
      
      # 找到最优参数
      best_index <- which.max(performance_metrics)
      best_result <- results[[best_index]]
      
      optimization_result <- list(
        best_parameters = best_result$parameters,
        best_performance = best_result$backtest_result$performance,
        all_results = results,
        performance_metrics = unlist(performance_metrics),
        objective_function = objective_function
      )
      
      return(optimization_result)
    },
    
    # 随机搜索优化
    random_search = function(strategy, data, parameter_ranges, n_iter = 100, objective_function = "sharpe_ratio") {
      cat("开始随机搜索优化...\n")
      
      results <- list()
      performance_metrics <- numeric(n_iter)
      
      for (i in 1:n_iter) {
        # 生成随机参数
        params <- private$generate_random_parameters(parameter_ranges)
        
        strategy_instance <- strategy$clone()
        strategy_instance$parameters <- params
        
        backtest_result <- private$backtest_engine$run_backtest(strategy_instance, data)
        
        results[[i]] <- list(
          parameters = params,
          backtest_result = backtest_result
        )
        
        performance_metrics[i] <- backtest_result$performance[[objective_function]]
        
        if (i %% 10 == 0) {
          cat(sprintf("完成 %d/%d 次迭代，当前最佳: %.4f\n", 
                      i, n_iter, max(performance_metrics[1:i])))
        }
      }
      
      # 找到最优参数
      best_index <- which.max(performance_metrics)
      best_result <- results[[best_index]]
      
      optimization_result <- list(
        best_parameters = best_result$parameters,
        best_performance = best_result$backtest_result$performance,
        all_results = results,
        performance_metrics = performance_metrics,
        objective_function = objective_function,
        convergence = private$analyze_convergence(performance_metrics)
      )
      
      return(optimization_result)
    },
    
    # 贝叶斯优化
    bayesian_optimization = function(strategy, data, parameter_ranges, n_iter = 50, objective_function = "sharpe_ratio") {
      if (!requireNamespace("ParBayesianOptimization", quietly = TRUE)) {
        stop("需要安装ParBayesianOptimization包进行贝叶斯优化")
      }
      
      cat("开始贝叶斯优化...\n")
      
      # 定义目标函数
      objective_function_wrapper <- function(...) {
        params <- list(...)
        
        strategy_instance <- strategy$clone()
        strategy_instance$parameters <- params
        
        backtest_result <- private$backtest_engine$run_backtest(strategy_instance, data)
        performance <- backtest_result$performance[[objective_function]]
        
        # 处理NA值
        if (is.na(performance)) {
          return(-1000)  # 极低的分数
        }
        
        return(performance)
      }
      
      # 设置参数边界
      bounds <- list()
      for (param_name in names(parameter_ranges)) {
        bounds[[param_name]] <- parameter_ranges[[param_name]]
      }
      
      # 运行贝叶斯优化
      bayes_result <- ParBayesianOptimization::bayesOpt(
        FUN = objective_function_wrapper,
        bounds = bounds,
        initPoints = min(10, length(bounds) + 1),
        iters.n = n_iter,
        verbose = 1
      )
      
      # 提取最佳参数
      best_params <- as.list(ParBayesianOptimization::getBestPars(bayes_result))
      best_performance <- bayes_result$scoreSummary$Score[which.max(bayes_result$scoreSummary$Score)]
      
      optimization_result <- list(
        best_parameters = best_params,
        best_performance = list(!!objective_function := best_performance),
        bayesian_result = bayes_result,
        convergence = bayes_result$convergence,
        objective_function = objective_function
      )
      
      return(optimization_result)
    },
    
    # 遗传算法优化
    genetic_optimization = function(strategy, data, parameter_ranges, pop_size = 50, n_generations = 100, objective_function = "sharpe_ratio") {
      if (!requireNamespace("GA", quietly = TRUE)) {
        stop("需要安装GA包进行遗传算法优化")
      }
      
      cat("开始遗传算法优化...\n")
      
      # 定义适应度函数
      fitness_function <- function(x) {
        params <- private$decode_parameters(x, parameter_ranges)
        
        strategy_instance <- strategy$clone()
        strategy_instance$parameters <- params
        
        backtest_result <- private$backtest_engine$run_backtest(strategy_instance, data)
        performance <- backtest_result$performance[[objective_function]]
        
        if (is.na(performance)) {
          return(-1000)
        }
        
        return(performance)
      }
      
      # 设置遗传算法参数
      n_params <- length(parameter_ranges)
      
      ga_result <- GA::ga(
        type = "real-valued",
        fitness = fitness_function,
        lower = sapply(parameter_ranges, function(x) x[1]),
        upper = sapply(parameter_ranges, function(x) x[2]),
        popSize = pop_size,
        maxiter = n_generations,
        monitor = TRUE,
        names = names(parameter_ranges)
      )
      
      # 提取最佳参数
      best_params <- as.list(ga_result@solution[1, ])
      names(best_params) <- names(parameter_ranges)
      best_performance <- ga_result@fitnessValue
      
      optimization_result <- list(
        best_parameters = best_params,
        best_performance = list(!!objective_function := best_performance),
        genetic_algorithm_result = ga_result,
        convergence = ga_result@iter == n_generations,
        objective_function = objective_function
      )
      
      return(optimization_result)
    },
    
    # 多目标优化
    multi_objective_optimization = function(strategy, data, parameter_ranges, objectives = c("sharpe_ratio", "total_return"), n_iter = 100) {
      cat("开始多目标优化...\n")
      
      results <- list()
      objective_values <- matrix(NA, nrow = n_iter, ncol = length(objectives))
      colnames(objective_values) <- objectives
      
      for (i in 1:n_iter) {
        params <- private$generate_random_parameters(parameter_ranges)
        
        strategy_instance <- strategy$clone()
        strategy_instance$parameters <- params
        
        backtest_result <- private$backtest_engine$run_backtest(strategy_instance, data)
        
        results[[i]] <- list(
          parameters = params,
          backtest_result = backtest_result
        )
        
        for (j in seq_along(objectives)) {
          objective_values[i, j] <- backtest_result$performance[[objectives[j]]]
        }
      }
      
      # 帕累托前沿分析
      pareto_front <- private$calculate_pareto_front(objective_values, objectives)
      
      optimization_result <- list(
        pareto_front = pareto_front,
        all_results = results,
        objective_values = objective_values,
        objectives = objectives
      )
      
      return(optimization_result)
    },
    
    # 优化结果分析
    analyze_optimization_results = function(optimization_results, method) {
      cat("分析优化结果...\n")
      
      analysis <- list(
        method = method,
        best_parameters = optimization_results$best_parameters,
        best_performance = optimization_results$best_performance,
        parameter_sensitivity = private$analyze_parameter_sensitivity(optimization_results),
        robustness_check = private$perform_robustness_check(optimization_results),
        overfitting_risk = private$assess_overfitting_risk(optimization_results)
      )
      
      return(analysis)
    },
    
    # 生成优化报告
    generate_optimization_report = function(optimization_results, output_file = NULL) {
      cat("生成优化报告...\n")
      
      report <- list(
        optimization_summary = private$create_optimization_summary(optimization_results),
        detailed_results = optimization_results,
        recommendations = private$generate_optimization_recommendations(optimization_results),
        timestamp = Sys.time()
      )
      
      if (!is.null(output_file)) {
        saveRDS(report, output_file)
        cat("优化报告已保存至:", output_file, "\n")
      }
      
      return(report)
    }
  ),
  
  private = list(
    backtest_engine = NULL,
    optimization_config = NULL,
    optimization_methods = list(),
    
    initialize_optimization_methods = function() {
      private$optimization_methods <- list(
        grid_search = list(
          max_combinations = private$optimization_config$grid_search$max_combinations
        ),
        random_search = list(
          max_iterations = private$optimization_config$random_search$max_iterations
        ),
        bayesian = list(
          acquisition_function = private$optimization_config$bayesian$acquisition_function
        ),
        genetic = list(
          population_size = private$optimization_config$genetic$population_size
        )
      )
    },
    
    # 生成网格组合
    generate_grid_combinations = function(parameter_grid) {
      # 计算总组合数
      total_combinations <- prod(sapply(parameter_grid, length))
      
      if (total_combinations > private$optimization_methods$grid_search$max_combinations) {
        warning(sprintf("网格组合数(%d)超过限制，使用随机采样", total_combinations))
        return(private$sample_grid_combinations(parameter_grid))
      }
      
      # 生成所有组合
      expand.grid(parameter_grid, stringsAsFactors = FALSE)
    },
    
    # 采样网格组合
    sample_grid_combinations = function(parameter_grid, n_samples = 1000) {
      samples <- list()
      
      for (i in 1:n_samples) {
        params <- list()
        for (param_name in names(parameter_grid)) {
          values <- parameter_grid[[param_name]]
          params[[param_name]] <- sample(values, 1)
        }
        samples[[i]] <- params
      }
      
      return(samples)
    },
    
    # 生成随机参数
    generate_random_parameters = function(parameter_ranges) {
      params <- list()
      
      for (param_name in names(parameter_ranges)) {
        range <- parameter_ranges[[param_name]]
        
        if (is.numeric(range) && length(range) == 2) {
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
      
      return(params)
    },
    
    # 参数解码（用于遗传算法）
    decode_parameters = function(encoded_params, parameter_ranges) {
      params <- list()
      param_names <- names(parameter_ranges)
      
      for (i in seq_along(param_names)) {
        param_name <- param_names[i]
        range <- parameter_ranges[[param_name]]
        
        if (is.numeric(range) && length(range) == 2) {
          # 连续参数
          params[[param_name]] <- encoded_params[i]
        } else if (is.integer(range)) {
          # 整数参数 - 四舍五入
          params[[param_name]] <- round(encoded_params[i])
        }
        # 注意：遗传算法通常不适合分类参数
      }
      
      return(params)
    },
    
    # 分析收敛性
    analyze_convergence = function(performance_metrics) {
      n <- length(performance_metrics)
      if (n < 10) return(FALSE)
      
      # 检查最后10%的迭代是否没有显著改进
      last_10_percent <- floor(0.9 * n):n
      if (length(last_10_percent) < 5) return(FALSE)
      
      last_values <- performance_metrics[last_10_percent]
      improvement <- diff(last_values)
      
      # 如果最后阶段平均改进很小，认为收敛
      mean_improvement <- mean(improvement, na.rm = TRUE)
      initial_std <- sd(performance_metrics[1:10], na.rm = TRUE)
      
      convergence <- abs(mean_improvement) < 0.01 * initial_std
      
      return(list(
        converged = convergence,
        mean_final_improvement = mean_improvement,
        final_performance = tail(performance_metrics, 1)
      ))
    },
    
    # 计算帕累托前沿
    calculate_pareto_front = function(objective_values, objectives) {
      if (nrow(objective_values) == 0) return(list())
      
      # 简单的帕累托前沿计算
      is_pareto <- rep(TRUE, nrow(objective_values))
      
      for (i in 1:nrow(objective_values)) {
        for (j in 1:nrow(objective_values)) {
          if (i != j) {
            # 检查j是否支配i
            dominates <- all(objective_values[j, ] >= objective_values[i, ]) && 
              any(objective_values[j, ] > objective_values[i, ])
            
            if (dominates) {
              is_pareto[i] <- FALSE
              break
            }
          }
        }
      }
      
      pareto_indices <- which(is_pareto)
      
      return(list(
        indices = pareto_indices,
        solutions = objective_values[pareto_indices, , drop = FALSE],
        n_solutions = length(pareto_indices)
      ))
    },
    
    # 分析参数敏感性
    analyze_parameter_sensitivity = function(optimization_results) {
      if (is.null(optimization_results$all_results)) {
        return(list())
      }
      
      # 提取参数和绩效数据
      results <- optimization_results$all_results
      n_results <- length(results)
      
      # 创建分析数据框
      sensitivity_data <- data.frame()
      
      for (i in 1:n_results) {
        result <- results[[i]]
        params <- result$parameters
        performance <- result$backtest_result$performance$sharpe_ratio
        
        row_data <- as.data.frame(params)
        row_data$performance <- performance
        
        sensitivity_data <- rbind(sensitivity_data, row_data)
      }
      
      # 计算参数敏感性（与绩效的相关性）
      param_cols <- names(optimization_results$best_parameters)
      sensitivities <- sapply(param_cols, function(param) {
        if (is.numeric(sensitivity_data[[param]])) {
          cor(sensitivity_data[[param]], sensitivity_data$performance, use = "complete.obs")
        } else {
          # 对于分类变量，使用方差分析
          anova_result <- aov(performance ~ factor(sensitivity_data[[param]]))
          summary(anova_result)[[1]]$"F value"[1]
        }
      })
      
      return(list(
        sensitivities = sort(sensitivities, decreasing = TRUE),
        sensitivity_data = sensitivity_data
      ))
    },
    
    # 执行稳健性检查
    perform_robustness_check = function(optimization_results) {
      # 简单的稳健性检查：检查最优参数附近的表现
      robustness <- list(
        parameter_stability = "待实现",  # 实际实现需要更多上下文
        performance_consistency = "待实现"
      )
      
      return(robustness)
    },
    
    # 评估过拟合风险
    assess_overfitting_risk = function(optimization_results) {
      # 简单的过拟合风险评估
      if (is.null(optimization_results$all_results)) {
        return("数据不足")
      }
      
      n_results <- length(optimization_results$all_results)
      performances <- sapply(optimization_results$all_results, function(x) {
        x$backtest_result$performance$sharpe_ratio
      })
      
      # 如果最佳性能远高于中位数性能，可能过拟合
      best_perf <- max(performances, na.rm = TRUE)
      median_perf <- median(performances, na.rm = TRUE)
      
      overfitting_ratio <- best_perf / median_perf
      
      risk_level <- if (overfitting_ratio > 2) {
        "高"
      } else if (overfitting_ratio > 1.5) {
        "中"
      } else {
        "低"
      }
      
      return(list(
        overfitting_ratio = overfitting_ratio,
        risk_level = risk_level,
        best_performance = best_perf,
        median_performance = median_perf
      ))
    },
    
    # 创建优化摘要
    create_optimization_summary = function(optimization_results) {
      summary <- list(
        optimization_date = Sys.time(),
        best_performance = optimization_results$best_performance,
        parameters_optimized = length(optimization_results$best_parameters),
        method_used = if (!is.null(optimization_results$method)) optimization_results$method else "未知"
      )
      
      return(summary)
    },
    
    # 生成优化建议
    generate_optimization_recommendations = function(optimization_results) {
      recommendations <- list()
      
      if (!is.null(optimization_results$overfitting_risk)) {
        risk <- optimization_results$overfitting_risk$risk_level
        if (risk == "高") {
          recommendations <- c(recommendations, "检测到高过拟合风险，建议使用样本外测试")
        }
      }
      
      if (!is.null(optimization_results$best_performance)) {
        best_perf <- optimization_results$best_performance[[1]]
        recommendations <- c(
          recommendations,
          paste("优化后最佳绩效:", round(best_perf, 4))
        )
      }
      
      return(recommendations)
    }
  )
)