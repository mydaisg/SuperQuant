# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 交易执行系统主程序
#' @description 启动完整的交易执行系统
#' @export

# 加载依赖
library(R6)
library(yaml)

# 初始化执行系统
initialize_execution_system <- function(config_path = "config/trading_costs.yml", 
                                        use_simulated_broker = TRUE) {
  
  # 初始化系统日志
  system_logger <- get_system_logger()
  
  # 初始化成本模型
  cost_model <- TradingCostModel$new(config_path, system_logger)
  
  # 初始化券商接口
  if (use_simulated_broker) {
    broker_api <- SimulatedBroker$new(initial_balance = 1000000, system_logger = system_logger)
  } else {
    broker_api <- IBBroker$new(system_logger = system_logger)
    broker_api$connect()
  }
  
  # 初始化订单管理器
  order_manager <- OrderManager$new(broker_api, system_logger)
  
  # 初始化执行引擎
  execution_engine <- ExecutionEngine$new(order_manager, broker_api, cost_model, system_logger)
  
  # 初始化分析器
  execution_analyzer <- ExecutionAnalyzer$new(order_manager, cost_model, system_logger)
  slippage_analyzer <- SlippageAnalyzer$new(execution_analyzer, system_logger)
  
  system <- list(
    order_manager = order_manager,
    execution_engine = execution_engine,
    execution_analyzer = execution_analyzer,
    slippage_analyzer = slippage_analyzer,
    cost_model = cost_model,
    broker_api = broker_api,
    system_logger = system_logger
  )
  
  system_logger$info("交易执行系统初始化完成", component = "execution_system")
  
  return(system)
}

# 测试函数
test_execution_system <- function() {
  # 初始化系统
  system <- initialize_execution_system(use_simulated_broker = TRUE)
  
  # 执行测试交易
  order_ids <- list()
  
  # 市价单
  order_ids$market_buy <- system$execution_engine$execute_trade(
    symbol = "AAPL",
    quantity = 100,
    side = "BUY",
    order_type = "MARKET",
    strategy = "test_strategy"
  )$order_id
  
  # 限价单
  order_ids$limit_sell <- system$execution_engine$execute_trade(
    symbol = "GOOGL", 
    quantity = 50,
    side = "SELL",
    order_type = "LIMIT",
    price = 2800.0,
    strategy = "test_strategy"
  )$order_id
  
  # TWAP算法单
  order_ids$twap_buy <- system$execution_engine$execute_trade(
    symbol = "MSFT",
    quantity = 200,
    side = "BUY",
    order_type = "MARKET",
    strategy = "test_strategy",
    algorithm = "TWAP",
    urgency = "NORMAL"
  )$order_id
  
  # 等待订单执行（模拟）
  Sys.sleep(2)
  
  # 分析执行质量
  for (order_id in order_ids) {
    metrics <- system$execution_analyzer$analyze_order_execution(order_id)
    if (!is.null(metrics)) {
      message(sprintf("订单 %s 执行分析: 滑点 %.2f bps", order_id, metrics$slippage_bps))
    }
  }
  
  # 生成执行报告
  report <- system$execution_analyzer$generate_execution_report()
  message(sprintf("执行报告生成: %d个订单", report$summary$total_orders))
  
  # 生成滑点报告
  slippage_report <- system$slippage_analyzer$generate_slippage_report()
  message(sprintf("滑点报告: %d条记录", slippage_report$total_records))
  
  return(list(
    system = system,
    order_ids = order_ids,
    execution_report = report,
    slippage_report = slippage_report
  ))
}

# 如果直接运行则执行测试
if (sys.nframe() == 0) {
  test_results <- test_execution_system()
  message("交易执行系统测试完成")
}