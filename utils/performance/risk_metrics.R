# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 风险指标计算
#' 
#' 计算各种风险管理指标

#' 计算VaR (风险价值)
calculate_var <- function(returns, confidence_level = 0.95, method = "historical") {
  if (method == "historical") {
    var <- quantile(returns, 1 - confidence_level, na.rm = TRUE)
  } else if (method == "parametric") {
    var <- mean(returns, na.rm = TRUE) + 
      qnorm(1 - confidence_level) * sd(returns, na.rm = TRUE)
  } else if (method == "modified") {
    # Cornish-Fisher扩展
    mu <- mean(returns, na.rm = TRUE)
    sigma <- sd(returns, na.rm = TRUE)
    skew <- moments::skewness(returns, na.rm = TRUE)
    kurt <- moments::kurtosis(returns, na.rm = TRUE)
    
    z <- qnorm(1 - confidence_level)
    z_adj <- z + (z^2 - 1) * skew / 6 + (z^3 - 3*z) * (kurt-3) / 24 - (2*z^3 - 5*z) * skew^2 / 36
    
    var <- mu + z_adj * sigma
  }
  
  return(var)
}

#' 计算预期短缺 (CVaR)
calculate_cvar <- function(returns, confidence_level = 0.95) {
  var <- calculate_var(returns, confidence_level, "historical")
  cvar <- mean(returns[returns <= var], na.rm = TRUE)
  return(cvar)
}

#' 计算下行风险
calculate_downside_risk <- function(returns, mar = 0) {
  downside_returns <- returns[returns < mar]
  if (length(downside_returns) == 0) return(0)
  
  downside_risk <- sqrt(mean((downside_returns - mar)^2)) * sqrt(252)
  return(downside_risk)
}

#' 计算索提诺比率
calculate_sortino_ratio <- function(returns, mar = 0) {
  annual_return <- (prod(1 + returns))^(252/length(returns)) - 1
  downside_risk <- calculate_downside_risk(returns, mar)
  
  if (downside_risk == 0) return(NA)
  sortino_ratio <- (annual_return - mar) / downside_risk
  return(sortino_ratio)
}