# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 报告模板生成器
#' 
#' 生成标准化的绩效和风险报告

#' 生成绩效报告
generate_performance_report <- function(portfolio_returns, benchmark_returns = NULL, 
                                        report_title = "投资组合绩效报告") {
  
  # 计算性能指标
  metrics <- calculate_performance_metrics(portfolio_returns, benchmark_returns)
  
  # 创建报告对象
  report <- list(
    title = report_title,
    period = paste(min(names(portfolio_returns)), "至", max(names(portfolio_returns))),
    metrics = metrics,
    charts = list()
  )
  
  # 添加净值曲线图表
  report$charts$equity_curve <- plot_equity_curve(portfolio_returns, benchmark_returns)
  
  # 添加回撤图表
  report$charts$drawdown_chart <- plot_drawdowns(portfolio_returns)
  
  # 添加月度收益热力图
  report$charts$monthly_returns_heatmap <- plot_monthly_returns_heatmap(portfolio_returns)
  
  return(report)
}

#' 生成风险报告
generate_risk_report <- function(portfolio_returns, positions, risk_factors = NULL) {
  report <- list(
    timestamp = Sys.time(),
    var_metrics = calculate_var_metrics(portfolio_returns),
    concentration_metrics = calculate_concentration_metrics(positions),
    liquidity_metrics = calculate_liquidity_metrics(positions)
  )
  
  if (!is.null(risk_factors)) {
    report$factor_exposure <- calculate_factor_exposure(portfolio_returns, risk_factors)
  }
  
  return(report)
}