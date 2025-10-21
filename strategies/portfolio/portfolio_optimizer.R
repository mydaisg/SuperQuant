# portfolio/portfolio_optimizer.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 投资组合优化器
PortfolioOptimizer <- R6::R6Class(
  "PortfolioOptimizer",
  public = list(
    initialize = function(config_path = "config/portfolio.yml") {
      private$portfolio_config <- yaml::read_yaml(config_path)
      private$initialize_optimization_methods()
    },
    
    # 均值-方差优化
    mean_variance_optimization = function(returns, constraints = NULL) {
      cat("执行均值-方差优化...\n")
      
      if (is.null(constraints)) {
        constraints <- private$portfolio_config$default_constraints
      }
      
      # 计算输入参数
      mu <- colMeans(returns, na.rm = TRUE)
      Sigma <- cov(returns, use = "complete.obs")
      
      # 检查可行性
      feasibility_check <- private$check_optimization_feasibility(mu, Sigma, constraints)
      if (!feasibility_check$feasible) {
        warning("优化问题不可行: ", feasibility_check$reason)
        return(list(weights = rep(1/length(mu), length(mu)), status = "infeasible"))
      }
      
      # 使用二次规划求解
      optimization_result <- private$solve_quadratic_program(mu, Sigma, constraints)
      
      return(list(
        weights = optimization_result$weights,
        expected_return = optimization_result$expected_return,
        expected_risk = optimization_result$expected_risk,
        sharpe_ratio = optimization_result$sharpe_ratio,
        optimization_status = optimization_result$status
      ))
    },
    
    # 风险平价优化
    risk_parity_optimization = function(returns, constraints = NULL) {
      cat("执行风险平价优化...\n")
      
      if (is.null(constraints)) {
        constraints <- private$portfolio_config$risk_parity_constraints
      }
      
      Sigma <- cov(returns, use = "complete.obs")
      
      # 使用迭代方法求解风险平价
      weights <- private$solve_risk_parity(Sigma, constraints)
      
      # 计算预期绩效
      mu <- colMeans(returns, na.rm = TRUE)
      expected_return <- sum(weights * mu)
      expected_risk <- sqrt(t(weights) %*% Sigma %*% weights)
      
      return(list(
        weights = weights,
        expected_return = expected_return,
        expected_risk = expected_risk,
        sharpe_ratio = expected_return / expected_risk,
        risk_contributions = private$calculate_risk_contributions(weights, Sigma)
      ))
    },
    
    # 最大分散化优化
    maximum_diversification_optimization = function(returns, constraints = NULL) {
      cat("执行最大分散化优化...\n")
      
      Sigma <- cov(returns, use = "complete.obs")
      volatilities <- sqrt(diag(Sigma))
      
      # 求解最大分散化组合
      weights <- private$solve_maximum_diversification(Sigma, volatilities, constraints)
      
      # 计算分散化比率
      diversification_ratio <- private$calculate_diversification_ratio(weights, Sigma, volatilities)
      
      return(list(
        weights = weights,
        diversification_ratio = diversification_ratio,
        optimization_status = "completed"
      ))
    },
    
    # Black-Litterman优化
    black_litterman_optimization = function(returns, market_weights, views, view_confidences, 
                                            tau = 0.05) {
      cat("执行Black-Litterman优化...\n")
      
      # 计算市场隐含收益率
      implied_returns <- private$calculate_implied_returns(market_weights, returns, tau)
      
      # 结合观点
      bl_returns <- private$combine_views(implied_returns, views, view_confidences, returns, tau)
      
      # 使用新收益率进行均值-方差优化
      optimization_result <- self$mean_variance_optimization(
        matrix(bl_returns, nrow = 1)  # 伪收益率矩阵
      )
      
      return(list(
        weights = optimization_result$weights,
        implied_returns = implied_returns,
        bl_returns = bl_returns,
        optimization_result = optimization_result
      ))
    },
    
    # 稳健优化
    robust_optimization = function(returns, uncertainty_level = 0.1, constraints = NULL) {
      cat("执行稳健优化...\n")
      
      # 使用稳健估计量
      robust_mu <- private$robust_mean_estimation(returns)
      robust_Sigma <- private$robust_covariance_estimation(returns)
      
      # 考虑不确定性集合
      uncertainty_adjusted <- private$adjust_for_uncertainty(
        robust_mu, robust_Sigma, uncertainty_level
      )
      
      # 标准均值-方差优化
      optimization_result <- self$mean_variance_optimization(
        returns, constraints  # 使用原始returns保持格式
      )
      
      return(list(
        weights = optimization_result$weights,
        robust_mu = robust_mu,
        robust_Sigma = robust_Sigma,
        uncertainty_adjustment = uncertainty_adjusted,
        optimization_result = optimization_result
      ))
    },
    
    # 多周期优化
    multi_period_optimization = function(returns, rebalancing_periods, optimization_horizon) {
      cat("执行多周期优化...\n")
      
      optimization_results <- list()
      
      for (period in rebalancing_periods) {
        cat("优化时期:", period, "\n")
        
        # 提取时期数据
        period_returns <- returns[period$start:period$end, ]
        
        # 单期优化
        period_result <- self$mean_variance_optimization(period_returns)
        
        optimization_results[[period$name]] <- list(
          weights = period_result$weights,
          performance = period_result,
          period = period
        )
      }
      
      # 分析权重稳定性
      stability_analysis <- private$analyze_weights_stability(optimization_results)
      
      return(list(
        period_results = optimization_results,
        stability_analysis = stability_analysis,
        average_weights = private$calculate_average_weights(optimization_results)
      ))
    },
    
    # 组合绩效分析
    analyze_portfolio_performance = function(weights, returns, benchmark_returns = NULL) {
      cat("分析投资组合绩效...\n")
      
      # 计算组合收益率
      portfolio_returns <- rowSums(returns * matrix(weights, nrow = nrow(returns), 
                                                    ncol = ncol(returns), byrow = TRUE))
      
      performance_metrics <- private$calculate_performance_metrics(
        portfolio_returns, benchmark_returns
      )
      
      # 风险分解
      risk_decomposition <- private$decompose_portfolio_risk(weights, returns)
      
      # 持仓分析
      holding_analysis <- private$analyze_portfolio_holdings(weights, returns)
      
      return(list(
        portfolio_returns = portfolio_returns,
        performance_metrics = performance_metrics,
        risk_decomposition = risk_decomposition,
        holding_analysis = holding_analysis
      ))
    }
  ),
  
  private = list(
    portfolio_config = NULL,
    optimization_methods = list(),
    
    initialize_optimization_methods = function() {
      private$optimization_methods <- list(
        quadratic_programming = list(
          solver = "solve.QP",
          max_iterations = 1000
        ),
        risk_parity = list(
          convergence_tolerance = 1e-6,
          max_iterations = 100
        )
      )
    },
    
    # 检查优化可行性
    check_optimization_feasibility = function(mu, Sigma, constraints) {
      # 检查协方差矩阵正定性
      eigen_values <- eigen(Sigma, only.values = TRUE)$values
      if (any(eigen_values <= 0)) {
        return(list(feasible = FALSE, reason = "协方差矩阵非正定"))
      }
      
      # 检查约束可行性
      if (!is.null(constraints$min_weight) && !is.null(constraints$max_weight)) {
        if (any(constraints$min_weight > constraints$max_weight)) {
          return(list(feasible = FALSE, reason = "权重约束矛盾"))
        }
      }
      
      return(list(feasible = TRUE, reason = ""))
    },
    
    # 求解二次规划
    solve_quadratic_program = function(mu, Sigma, constraints) {
      n_assets <- length(mu)
      
      # 设置优化参数
      Dmat <- 2 * Sigma  # 二次项系数
      dvec <- rep(0, n_assets)  # 线性项系数（均值-方差中为0）
      
      # 约束条件
      Amat <- private$build_constraint_matrix(n_assets, constraints)
      bvec <- private$build_constraint_vector(n_assets, constraints)
      
      # 求解二次规划
      tryCatch({
        solution <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
        weights <- solution$solution
        
        # 计算预期绩效
        expected_return <- sum(weights * mu)
        expected_risk <- sqrt(t(weights) %*% Sigma %*% weights)
        sharpe_ratio <- expected_return / expected_risk
        
        return(list(
          weights = weights,
          expected_return = expected_return,
          expected_risk = expected_risk,
          sharpe_ratio = sharpe_ratio,
          status = "optimal"
        ))
      }, error = function(e) {
        # 如果优化失败，使用等权重
        warning("二次规划求解失败，使用等权重: ", e$message)
        weights <- rep(1/n_assets, n_assets)
        
        return(list(
          weights = weights,
          expected_return = sum(weights * mu),
          expected_risk = sqrt(t(weights) %*% Sigma %*% weights),
          sharpe_ratio = sum(weights * mu) / sqrt(t(weights) %*% Sigma %*% weights),
          status = "fallback"
        ))
      })
    },
    
    # 构建约束矩阵
    build_constraint_matrix = function(n_assets, constraints) {
      # 基础约束：权重和为1
      Amat <- matrix(1, nrow = n_assets, ncol = 1)
      
      # 权重上下限约束
      if (!is.null(constraints$min_weight)) {
        Amat <- cbind(Amat, diag(n_assets))  # 下界约束
      }
      if (!is.null(constraints$max_weight)) {
        Amat <- cbind(Amat, -diag(n_assets))  # 上界约束
      }
      
      return(Amat)
    },
    
    # 构建约束向量
    build_constraint_vector = function(n_assets, constraints) {
      # 权重和为1
      bvec <- 1
      
      # 权重下界
      if (!is.null(constraints$min_weight)) {
        bvec <- c(bvec, rep(constraints$min_weight, n_assets))
      }
      
      # 权重上界
      if (!is.null(constraints$max_weight)) {
        bvec <- c(bvec, rep(-constraints$max_weight, n_assets))
      }
      
      return(bvec)
    },
    
    # 求解风险平价
    solve_risk_parity = function(Sigma, constraints, max_iter = 100, tol = 1e-6) {
      n_assets <- ncol(Sigma)
      weights <- rep(1/n_assets, n_assets)  # 初始等权重
      
      for (iter in 1:max_iter) {
        # 计算风险贡献
        risk_contributions <- private$calculate_risk_contributions(weights, Sigma)
        
        # 计算目标风险贡献（等风险贡献）
        target_contributions <- rep(1/n_assets, n_assets)
        
        # 更新权重
        weight_changes <- target_contributions / risk_contributions - 1
        new_weights <- weights * (1 + 0.1 * weight_changes)  # 小步长更新
        
        # 应用约束
        if (!is.null(constraints$min_weight)) {
          new_weights <- pmax(new_weights, constraints$min_weight)
        }
        if (!is.null(constraints$max_weight)) {
          new_weights <- pmin(new_weights, constraints$max_weight)
        }
        
        # 归一化
        new_weights <- new_weights / sum(new_weights)
        
        # 检查收敛
        if (max(abs(new_weights - weights)) < tol) {
          break
        }
        
        weights <- new_weights
      }
      
      return(weights)
    },
    
    # 计算风险贡献
    calculate_risk_contributions = function(weights, Sigma) {
      portfolio_vol <- sqrt(t(weights) %*% Sigma %*% weights)
      marginal_contributions <- (Sigma %*% weights) / portfolio_vol
      risk_contributions <- weights * marginal_contributions
      
      return(as.numeric(risk_contributions / sum(risk_contributions)))
    },
    
    # 求解最大分散化组合
    solve_maximum_diversification = function(Sigma, volatilities, constraints) {
      n_assets <- length(volatilities)
      
      # 最大分散化等价于最小化波动率加权平均
      # 目标函数: min w'Σw / (w'σ)^2
      # 简化求解：使用逆波动率加权
      inv_vol_weights <- 1 / volatilities
      weights <- inv_vol_weights / sum(inv_vol_weights)
      
      # 应用约束
      if (!is.null(constraints$min_weight)) {
        weights <- pmax(weights, constraints$min_weight)
      }
      if (!is.null(constraints$max_weight)) {
        weights <- pmin(weights, constraints$max_weight)
      }
      
      # 重新归一化
      weights <- weights / sum(weights)
      
      return(weights)
    },
    
    # 计算分散化比率
    calculate_diversification_ratio = function(weights, Sigma, volatilities) {
      weighted_vol <- sum(weights * volatilities)
      portfolio_vol <- sqrt(t(weights) %*% Sigma %*% weights)
      
      return(weighted_vol / portfolio_vol)
    },
    
    # 计算隐含收益率
    calculate_implied_returns = function(market_weights, returns, tau) {
      Sigma <- cov(returns, use = "complete.obs")
      risk_aversion <- private$estimate_risk_aversion(market_weights, returns)
      
      implied_returns <- risk_aversion * Sigma %*% market_weights
      return(as.numeric(implied_returns))
    },
    
    # 估计风险厌恶系数
    estimate_risk_aversion = function(market_weights, returns) {
      Sigma <- cov(returns, use = "complete.obs")
      market_return <- mean(rowSums(returns * matrix(market_weights, nrow = nrow(returns), 
                                                     ncol = ncol(returns), byrow = TRUE)))
      market_variance <- t(market_weights) %*% Sigma %*% market_weights
      
      return(market_return / market_variance)
    },
    
    # 结合观点
    combine_views = function(implied_returns, views, view_confidences, returns, tau) {
      Sigma <- cov(returns, use = "complete.obs")
      n_assets <- length(implied_returns)
      
      # 构建观点矩阵
      P <- matrix(0, nrow = length(views), ncol = n_assets)
      Q <- numeric(length(views))
      Omega <- matrix(0, nrow = length(views), ncol = length(views))
      
      for (i in seq_along(views)) {
        view <- views[[i]]
        P[i, view$assets] <- view$weights
        Q[i] <- view$return
        Omega[i, i] <- view_confidences[i]
      }
      
      # Black-Litterman公式
      tau_Sigma <- tau * Sigma
      implied_var <- solve(solve(tau_Sigma) + t(P) %*% solve(Omega) %*% P)
      implied_mean <- implied_var %*% (solve(tau_Sigma) %*% implied_returns + 
                                         t(P) %*% solve(Omega) %*% Q)
      
      return(as.numeric(implied_mean))
    },
    
    # 稳健均值估计
    robust_mean_estimation = function(returns) {
      # 使用修剪均值
      trim_percentage <- 0.1  # 修剪10%
      robust_means <- apply(returns, 2, function(x) {
        x_clean <- x[!is.na(x)]
        mean(x_clean, trim = trim_percentage)
      })
      
      return(robust_means)
    },
    
    # 稳健协方差估计
    robust_covariance_estimation = function(returns) {
      # 使用MCD估计（如果可用）
      if (requireNamespace("MASS", quietly = TRUE)) {
        tryCatch({
          robust_cov <- MASS::cov.rob(returns, method = "mcd")$cov
          return(robust_cov)
        }, error = function(e) {
          warning("MCD估计失败，使用标准协方差: ", e$message)
        })
      }
      
      # 回退到标准协方差
      return(cov(returns, use = "complete.obs"))
    },
    
    # 不确定性调整
    adjust_for_uncertainty = function(mu, Sigma, uncertainty_level) {
      # 简单的不确定性调整：扩大置信区间
      adjusted_mu <- mu * (1 + uncertainty_level * rnorm(length(mu), 0, 0.1))
      adjusted_Sigma <- Sigma * (1 + uncertainty_level)
      
      return(list(
        adjusted_mu = adjusted_mu,
        adjusted_Sigma = adjusted_Sigma
      ))
    },
    
    # 分析权重稳定性
    analyze_weights_stability = function(optimization_results) {
      all_weights <- lapply(optimization_results, function(x) x$weights)
      weight_matrix <- do.call(cbind, all_weights)
      
      stability_metrics <- list(
        weight_volatility = apply(weight_matrix, 1, sd),
        turnover_rate = mean(apply(weight_matrix, 2, function(w) sum(abs(diff(w)))) / 2),
        concentration_index = mean(apply(weight_matrix, 2, function(w) sum(w^2)))
      )
      
      return(stability_metrics)
    },
    
    # 计算平均权重
    calculate_average_weights = function(optimization_results) {
      all_weights <- lapply(optimization_results, function(x) x$weights)
      weight_matrix <- do.call(cbind, all_weights)
      
      return(rowMeans(weight_matrix))
    },
    
    # 计算绩效指标
    calculate_performance_metrics = function(portfolio_returns, benchmark_returns) {
      metrics <- list()
      
      # 基本指标
      metrics$total_return <- prod(1 + portfolio_returns) - 1
      metrics$annual_return <- mean(portfolio_returns) * 252
      metrics$annual_volatility <- sd(portfolio_returns) * sqrt(252)
      metrics$sharpe_ratio <- metrics$annual_return / metrics$annual_volatility
      
      # 最大回撤
      metrics$max_drawdown <- private$calculate_max_drawdown(portfolio_returns)
      metrics$calmar_ratio <- metrics$annual_return / abs(metrics$max_drawdown)
      
      # 下行风险
      metrics$downside_deviation <- private$calculate_downside_deviation(portfolio_returns)
      metrics$sortino_ratio <- metrics$annual_return / metrics$downside_deviation
      
      # 与基准比较（如果有）
      if (!is.null(benchmark_returns)) {
        metrics$alpha <- private$calculate_alpha(portfolio_returns, benchmark_returns)
        metrics$beta <- private$calculate_beta(portfolio_returns, benchmark_returns)
        metrics$tracking_error <- private$calculate_tracking_error(portfolio_returns, benchmark_returns)
        metrics$information_ratio <- metrics$alpha / metrics$tracking_error
      }
      
      return(metrics)
    },
    
    # 计算最大回撤
    calculate_max_drawdown = function(returns) {
      cumulative_returns <- cumprod(1 + returns)
      cumulative_max <- cummax(cumulative_returns)
      drawdowns <- (cumulative_returns - cumulative_max) / cumulative_max
      
      return(min(drawdowns))
    },
    
    # 计算下行偏差
    calculate_downside_deviation = function(returns, mar = 0) {
      downside_returns <- returns[returns < mar]
      if (length(downside_returns) > 0) {
        sqrt(mean(downside_returns^2)) * sqrt(252)
      } else {
        0
      }
    },
    
    # 计算Alpha
    calculate_alpha = function(portfolio_returns, benchmark_returns) {
      excess_returns <- portfolio_returns - benchmark_returns
      mean(excess_returns) * 252
    },
    
    # 计算Beta
    calculate_beta = function(portfolio_returns, benchmark_returns) {
      cov_matrix <- cov(portfolio_returns, benchmark_returns)
      cov_matrix[1, 2] / var(benchmark_returns)
    },
    
    # 计算跟踪误差
    calculate_tracking_error = function(portfolio_returns, benchmark_returns) {
      active_returns <- portfolio_returns - benchmark_returns
      sd(active_returns) * sqrt(252)
    },
    
    # 分解组合风险
    decompose_portfolio_risk = function(weights, returns) {
      Sigma <- cov(returns, use = "complete.obs")
      portfolio_variance <- t(weights) %*% Sigma %*% weights
      
      # 边际风险贡献
      marginal_risk <- (Sigma %*% weights) / sqrt(portfolio_variance)
      
      # 风险贡献
      risk_contributions <- weights * marginal_risk
      
      return(list(
        total_risk = sqrt(portfolio_variance),
        marginal_risk_contributions = as.numeric(marginal_risk),
        risk_contributions = as.numeric(risk_contributions),
        percent_contributions = as.numeric(risk_contributions / sqrt(portfolio_variance))
      ))
    },
    
    # 分析组合持仓
    analyze_portfolio_holdings = function(weights, returns) {
      analysis <- list()
      
      analysis$concentration <- list(
        herfindahl_index = sum(weights^2),
        top_5_concentration = sum(sort(weights, decreasing = TRUE)[1:5]),
        effective_number = 1 / sum(weights^2)
      )
      
      analysis$sector_exposure <- private$calculate_sector_exposure(weights)
      analysis$factor_exposure <- private$calculate_factor_exposure(weights, returns)
      
      return(analysis)
    },
    
    # 计算行业暴露（简化）
    calculate_sector_exposure = function(weights) {
      # 这里需要标的的行业信息
      # 简化返回
      return(list(
        technology = sum(weights[1:floor(length(weights)/3)]),
        financials = sum(weights[(floor(length(weights)/3)+1):(2*floor(length(weights)/3))]),
        others = sum(weights[(2*floor(length(weights)/3)+1):length(weights)])
      ))
    },
    
    # 计算因子暴露
    calculate_factor_exposure = function(weights, returns) {
      # 简化实现：使用主成分分析
      pca <- prcomp(returns, scale. = TRUE)
      factor_exposures <- as.numeric(weights %*% pca$rotation[, 1:3])
      
      return(list(
        pca_factor_exposures = factor_exposures,
        explained_variance = summary(pca)$importance[2, 1:3]
      ))
    }
  )
)