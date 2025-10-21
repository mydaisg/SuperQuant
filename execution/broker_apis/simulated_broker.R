# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 模拟券商接口
#' @description 用于回测和模拟交易的券商接口
#' @export
SimulatedBroker <- R6::R6Class("SimulatedBroker",
                               inherit = BrokerInterface,
                               
                               public = list(
                                 
                                 #' @field account_balance 账户余额
                                 account_balance = 0,
                                 
                                 #' @field positions 持仓
                                 positions = list(),
                                 
                                 #' @field orders 订单记录
                                 orders = list(),
                                 
                                 #' @field market_data_provider 市场数据提供者
                                 market_data_provider = NULL,
                                 
                                 #' @field fill_probability 成交概率
                                 fill_probability = 0.8,
                                 
                                 #' @field fill_delay 成交延迟(秒)
                                 fill_delay = 1,
                                 
                                 #' @description 初始化模拟券商
                                 #' @param initial_balance 初始余额
                                 #' @param market_data_provider 市场数据提供者
                                 #' @param system_logger 系统日志
                                 initialize = function(initial_balance = 1000000, market_data_provider = NULL, 
                                                       system_logger = NULL) {
                                   super$initialize("SimulatedBroker", is_simulated = TRUE, system_logger = system_logger)
                                   self$account_balance <- initial_balance
                                   self$market_data_provider <- market_data_provider
                                   self$connected <- TRUE
                                 },
                                 
                                 #' @description 连接到模拟券商
                                 #' @return 总是返回TRUE
                                 connect = function() {
                                   self$connected <- TRUE
                                   
                                   if (!is.null(self$system_logger)) {
                                     self$system_logger$info(
                                       "连接到模拟券商",
                                       component = "simulated_broker"
                                     )
                                   }
                                   
                                   return(TRUE)
                                 },
                                 
                                 #' @description 断开连接
                                 disconnect = function() {
                                   self$connected <- FALSE
                                   
                                   if (!is.null(self$system_logger)) {
                                     self$system_logger$info(
                                       "断开模拟券商连接",
                                       component = "simulated_broker"
                                     )
                                   }
                                 },
                                 
                                 #' @description 提交订单
                                 #' @param order 订单对象
                                 #' @return 提交结果
                                 submit_order = function(order) {
                                   if (!self$connected) {
                                     return(list(success = FALSE, error_message = "未连接到券商"))
                                   }
                                   
                                   # 生成券商订单ID
                                   broker_order_id <- paste0("SIM_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", 
                                                             sprintf("%04d", sample(10000, 1)))
                                   
                                   # 保存订单
                                   self$orders[[broker_order_id]] <- list(
                                     broker_order_id = broker_order_id,
                                     order_id = order$order_id,
                                     symbol = order$symbol,
                                     quantity = order$quantity,
                                     order_type = order$order_type,
                                     side = order$side,
                                     price = order$price,
                                     status = "SUBMITTED",
                                     submitted_time = Sys.time(),
                                     fills = list()
                                   )
                                   
                                   # 模拟订单执行
                                   private$simulate_order_execution(broker_order_id)
                                   
                                   if (!is.null(self$system_logger)) {
                                     self$system_logger$info(
                                       sprintf("模拟券商订单提交: %s", broker_order_id),
                                       component = "simulated_broker",
                                       details = list(
                                         symbol = order$symbol,
                                         quantity = order$quantity,
                                         side = order$side
                                       )
                                     )
                                   }
                                   
                                   return(list(
                                     success = TRUE,
                                     broker_order_id = broker_order_id,
                                     error_message = NULL
                                   ))
                                 },
                                 
                                 #' @description 取消订单
                                 #' @param broker_order_id 券商订单ID
                                 #' @return 取消结果
                                 cancel_order = function(broker_order_id) {
                                   if (!self$connected) {
                                     return(list(success = FALSE, error_message = "未连接到券商"))
                                   }
                                   
                                   if (is.null(self$orders[[broker_order_id]])) {
                                     return(list(success = FALSE, error_message = "订单不存在"))
                                   }
                                   
                                   order <- self$orders[[broker_order_id]]
                                   
                                   # 检查订单状态是否可以取消
                                   if (!order$status %in% c("SUBMITTED", "PARTIALLY_FILLED")) {
                                     return(list(success = FALSE, error_message = "订单无法取消"))
                                   }
                                   
                                   # 更新订单状态
                                   order$status <- "CANCELED"
                                   order$canceled_time <- Sys.time()
                                   self$orders[[broker_order_id]] <- order
                                   
                                   if (!is.null(self$system_logger)) {
                                     self$system_logger$info(
                                       sprintf("模拟券商订单取消: %s", broker_order_id),
                                       component = "simulated_broker"
                                     )
                                   }
                                   
                                   return(list(success = TRUE, error_message = NULL))
                                 },
                                 
                                 #' @description 获取订单状态
                                 #' @param broker_order_id 券商订单ID
                                 #' @return 订单状态
                                 get_order_status = function(broker_order_id) {
                                   if (is.null(self$orders[[broker_order_id]])) {
                                     return(list(success = FALSE, error_message = "订单不存在"))
                                   }
                                   
                                   order <- self$orders[[broker_order_id]]
                                   
                                   return(list(
                                     success = TRUE,
                                     status = order$status,
                                     filled_quantity = sum(sapply(order$fills, function(f) f$quantity)),
                                     remaining_quantity = order$quantity - sum(sapply(order$fills, function(f) f$quantity)),
                                     avg_fill_price = ifelse(length(order$fills) > 0, 
                                                             sum(sapply(order$fills, function(f) f$price * f$quantity)) / 
                                                               sum(sapply(order$fills, function(f) f$quantity)), 0),
                                     fill_price = ifelse(length(order$fills) > 0, order$fills[[length(order$fills)]]$price, NULL),
                                     fill_quantity = ifelse(length(order$fills) > 0, order$fills[[length(order$fills)]]$quantity, NULL),
                                     fill_time = ifelse(length(order$fills) > 0, order$fills[[length(order$fills)]]$time, NULL)
                                   ))
                                 },
                                 
                                 #' @description 获取账户信息
                                 #' @return 账户信息
                                 get_account_info = function() {
                                   # 计算总资产
                                   total_value <- self$account_balance
                                   for (symbol in names(self$positions)) {
                                     position <- self$positions[[symbol]]
                                     market_data <- self$get_market_data(symbol)
                                     if (!is.null(market_data)) {
                                       total_value <- total_value + position$quantity * market_data$price
                                     }
                                   }
                                   
                                   list(
                                     account_value = total_value,
                                     cash_balance = self$account_balance,
                                     buying_power = self$account_balance * 2,  # 模拟融资
                                     margin_available = self$account_balance
                                   )
                                 },
                                 
                                 #' @description 获取持仓信息
                                 #' @return 持仓列表
                                 get_positions = function() {
                                   self$positions
                                 },
                                 
                                 #' @description 获取市场数据
                                 #' @param symbol 交易标的
                                 #' @return 市场数据
                                 get_market_data = function(symbol) {
                                   if (!is.null(self$market_data_provider)) {
                                     return(self$market_data_provider$get_price(symbol))
                                   } else {
                                     # 默认模拟价格
                                     base_prices <- list(
                                       "AAPL" = 150.0,
                                       "GOOGL" = 2800.0,
                                       "MSFT" = 330.0,
                                       "TSLA" = 250.0
                                     )
                                     
                                     base_price <- ifelse(!is.null(base_prices[[symbol]]), base_prices[[symbol]], 100.0)
                                     
                                     # 添加随机波动
                                     price <- base_price * runif(1, 0.95, 1.05)
                                     
                                     return(list(
                                       symbol = symbol,
                                       price = price,
                                       bid = price * 0.999,
                                       ask = price * 1.001,
                                       volume = sample(1000000:5000000, 1),
                                       timestamp = Sys.time()
                                     ))
                                   }
                                 },
                                 
                                 #' @description 重置模拟环境
                                 #' @param initial_balance 初始余额
                                 reset = function(initial_balance = 1000000) {
                                   self$account_balance <- initial_balance
                                   self$positions <- list()
                                   self$orders <- list()
                                   
                                   if (!is.null(self$system_logger)) {
                                     self$system_logger$info(
                                       sprintf("重置模拟券商，初始余额: %.2f", initial_balance),
                                       component = "simulated_broker"
                                     )
                                   }
                                 }
                               ),
                               
                               private = list(
                                 
                                 #' @description 模拟订单执行
                                 #' @param broker_order_id 券商订单ID
                                 simulate_order_execution = function(broker_order_id) {
                                   order <- self$orders[[broker_order_id]]
                                   
                                   # 模拟执行延迟
                                   later::later(function() {
                                     # 检查订单是否仍然存在且未完成
                                     if (is.null(self$orders[[broker_order_id]]) || 
                                         !self$orders[[broker_order_id]]$status %in% c("SUBMITTED", "PARTIALLY_FILLED")) {
                                       return()
                                     }
                                     
                                     # 获取当前市场数据
                                     market_data <- self$get_market_data(order$symbol)
                                     if (is.null(market_data)) return()
                                     
                                     # 决定是否成交
                                     if (runif(1) <= self$fill_probability) {
                                       # 计算成交价格
                                       if (order$order_type == "MARKET") {
                                         fill_price <- ifelse(order$side == "BUY", market_data$ask, market_data$bid)
                                       } else {
                                         # 限价单检查价格条件
                                         if ((order$side == "BUY" && order$price >= market_data$ask) ||
                                             (order$side == "SELL" && order$price <= market_data$bid)) {
                                           fill_price <- ifelse(order$side == "BUY", market_data$ask, market_data$bid)
                                         } else {
                                           # 价格条件不满足，不成交
                                           return()
                                         }
                                       }
                                       
                                       # 计算成交数量
                                       filled_qty <- order$quantity
                                       if (order$order_type == "MARKET" && runif(1) > 0.3) {
                                         # 市场单可能部分成交
                                         filled_qty <- floor(order$quantity * runif(1, 0.5, 1.0))
                                       }
                                       
                                       # 创建成交记录
                                       fill <- list(
                                         price = fill_price,
                                         quantity = filled_qty,
                                         time = Sys.time()
                                       )
                                       
                                       # 更新订单
                                       order$fills <- c(order$fills, list(fill))
                                       total_filled <- sum(sapply(order$fills, function(f) f$quantity))
                                       
                                       if (total_filled >= order$quantity) {
                                         order$status <- "FILLED"
                                       } else {
                                         order$status <- "PARTIALLY_FILLED"
                                       }
                                       
                                       # 更新持仓和现金
                                       self$update_position_and_cash(order$symbol, order$side, fill_price, filled_qty)
                                       
                                       # 保存更新后的订单
                                       self$orders[[broker_order_id]] <- order
                                       
                                       if (!is.null(self$system_logger)) {
                                         self$system_logger$info(
                                           sprintf("模拟订单成交: %s - %d @ %.4f", broker_order_id, filled_qty, fill_price),
                                           component = "simulated_broker"
                                         )
                                       }
                                     }
                                   }, delay = self$fill_delay)
                                 },
                                 
                                 #' @description 更新持仓和现金
                                 #' @param symbol 交易标的
                                 #' @param side 买卖方向
                                 #' @param price 价格
                                 #' @param quantity 数量
                                 update_position_and_cash = function(symbol, side, price, quantity) {
                                   # 更新现金
                                   cash_impact <- price * quantity
                                   if (side == "BUY") {
                                     self$account_balance <- self$account_balance - cash_impact
                                   } else {
                                     self$account_balance <- self$account_balance + cash_impact
                                   }
                                   
                                   # 更新持仓
                                   if (is.null(self$positions[[symbol]])) {
                                     self$positions[[symbol]] <- list(
                                       symbol = symbol,
                                       quantity = 0,
                                       avg_cost = 0
                                     )
                                   }
                                   
                                   position <- self$positions[[symbol]]
                                   
                                   if (side == "BUY") {
                                     total_cost <- position$quantity * position$avg_cost + cash_impact
                                     new_quantity <- position$quantity + quantity
                                     position$avg_cost <- total_cost / new_quantity
                                     position$quantity <- new_quantity
                                   } else {
                                     position$quantity <- position$quantity - quantity
                                     if (position$quantity == 0) {
                                       position$avg_cost <- 0
                                     }
                                   }
                                   
                                   self$positions[[symbol]] <- position
                                 }
                               )
)