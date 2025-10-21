# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 监控系统主程序
#' @description 启动完整的监控系统
#' @export

# 加载依赖
library(R6)
library(yaml)
library(shiny)
library(shinydashboard)

# 初始化各个组件
system_logger <- SystemLogger$new("config/logging.yml")
performance_logger <- PerformanceLogger$new(system_logger)
alert_manager <- AlertManager$new("config/alert_rules.yml")

# 启动监控面板
start_monitoring_system <- function(host = "0.0.0.0", port = 3838) {
  message("启动量化交易监控系统...")
  
  # 记录启动日志
  system_logger$info("监控系统启动", component = "system")
  
  # 启动dashboard
  start_dashboard(system_monitor = NULL, risk_manager = NULL, host = host, port = port)
}

# 测试函数
test_monitoring_system <- function() {
  # 测试日志系统
  system_logger$info("测试信息日志")
  system_logger$warn("测试警告日志") 
  system_logger$error("测试错误日志")
  system_logger$log_trade("ORDER_001", "AAPL", "BUY", 100, 150.25)
  
  # 测试性能日志
  performance_logger$log_strategy_performance("动量策略", list(
    returns = 0.05,
    sharpe = 1.8,
    max_drawdown = 0.03,
    volatility = 0.12
  ))
  
  # 测试警报系统
  test_data <- list(
    current_drawdown = 0.09,
    volatility = 0.28,
    position_violation = TRUE,
    violation_details = "AAPL头寸超限"
  )
  
  alerts <- alert_manager$check_alerts(test_data)
  message("触发警报数: ", length(alerts))
  
  # 查询日志
  logs <- system_logger$query_logs(level = c("INFO", "WARN", "ERROR"))
  message("查询到日志数: ", nrow(logs))
}

# 如果直接运行则执行测试
if (sys.nframe() == 0) {
  test_monitoring_system()
}