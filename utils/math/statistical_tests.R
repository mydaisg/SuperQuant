# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 统计检验工具
#' 
#' 执行各种统计检验

#' 正态性检验
test_normality <- function(returns, tests = c("shapiro", "jarque-bera", "anderson")) {
  results <- list()
  
  if ("shapiro" %in% tests) {
    results$shapiro <- shapiro.test(returns)
  }
  
  if ("jarque-bera" %in% tests) {
    results$jarque_bera <- tseries::jarque.bera.test(returns)
  }
  
  if ("anderson" %in% tests) {
    results$anderson <- nortest::ad.test(returns)
  }
  
  return(results)
}

#' 平稳性检验 (ADF检验)
test_stationarity <- function(series, type = "trend") {
  result <- tseries::adf.test(na.omit(series), alternative = "stationary")
  return(result)
}

#' 自相关检验
test_autocorrelation <- function(returns, max_lag = 20) {
  acf_results <- acf(returns, lag.max = max_lag, plot = FALSE)
  lb_test <- Box.test(returns, lag = max_lag, type = "Ljung-Box")
  
  return(list(
    acf = acf_results,
    ljung_box = lb_test
  ))
}