# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 性能指标计算
#' 
#' 计算各种投资组合性能指标

#' 计算基本性能指标
calculate_performance_metrics <- function(returns, benchmark_returns = NULL) {
  metrics <- list()
  
  # 基本统计
  metrics$total_return <- prod(1 + returns) - 1
  metrics$annual_return <- (1 + metrics$total_return)^(252/length(returns)) - 1
  metrics$volatility <- sd(returns) * sqrt(252)
  metrics$sharpe_ratio <- metrics$annual_return / metrics$volatility
  
  # 回撤分析
  metrics$max_drawdown <- calculate_max_drawdown(returns)
  metrics$calmar_ratio <- metrics$annual_return / abs(metrics$max_drawdown)
  
  # 风险调整收益
  metrics$var_95 <- quantile(returns, 0.05)
  metrics$cvar_95 <- mean(returns[returns <= metrics$var_95])
  
  if (!is.null(benchmark_returns)) {
    metrics$alpha_beta <- calculate_alpha_beta(returns, benchmark_returns)
    metrics$information_ratio <- calculate_information_ratio(returns, benchmark_returns)
    metrics$tracking_error <- sd(returns - benchmark_returns) * sqrt(252)
  }
  
  return(metrics)
}

#' 计算最大回撤
calculate_max_drawdown <- function(returns) {
  cumulative_returns <- cumprod(1 + returns)
  running_max <- cummax(cumulative_returns)
  drawdowns <- (cumulative_returns - running_max) / running_max
  max_drawdown <- min(drawdowns)
  
  return(max_drawdown)
}

#' 计算Alpha和Beta
calculate_alpha_beta <- function(portfolio_returns, benchmark_returns) {
  if (length(portfolio_returns) != length(benchmark_returns)) {
    stop("Return series must have same length")
  }
  
  # 移除NA值
  valid_data <- complete.cases(portfolio_returns, benchmark_returns)
  portfolio_returns <- portfolio_returns[valid_data]
  benchmark_returns <- benchmark_returns[valid_data]
  
  # 线性回归计算beta
  model <- lm(portfolio_returns ~ benchmark_returns)
  beta <- coef(model)[2]
  alpha <- coef(model)[1] * 252  # 年化alpha
  
  return(list(alpha = alpha, beta = beta, r_squared = summary(model)$r.squared))
}