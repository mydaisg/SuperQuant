# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 图表生成器
#' 
#' 生成标准化的金融图表

#' 绘制净值曲线
plot_equity_curve <- function(portfolio_returns, benchmark_returns = NULL) {
  portfolio_equity <- cumprod(1 + portfolio_returns)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = index(portfolio_equity), y = portfolio_equity), 
                       color = "blue", size = 1) +
    ggplot2::labs(title = "净值曲线", x = "日期", y = "净值") +
    ggplot2::theme_minimal()
  
  if (!is.null(benchmark_returns)) {
    benchmark_equity <- cumprod(1 + benchmark_returns)
    p <- p + ggplot2::geom_line(
      ggplot2::aes(x = index(benchmark_equity), y = benchmark_equity), 
      color = "red", linetype = "dashed"
    )
  }
  
  return(p)
}

#' 绘制回撤图表
plot_drawdowns <- function(returns) {
  equity <- cumprod(1 + returns)
  running_max <- cummax(equity)
  drawdowns <- (equity - running_max) / running_max
  
  drawdown_data <- data.frame(
    Date = index(returns),
    Drawdown = drawdowns
  )
  
  p <- ggplot2::ggplot(drawdown_data, ggplot2::aes(x = Date, y = Drawdown)) +
    ggplot2::geom_area(fill = "red", alpha = 0.3) +
    ggplot2::labs(title = "回撤分析", x = "日期", y = "回撤") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::percent)
  
  return(p)
}