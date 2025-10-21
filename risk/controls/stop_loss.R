# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 止损控制
#' @description 实现多种止损策略
#' @export
StopLossControl <- R6::R6Class("StopLossControl",
                               public = list(
                                 
                                 #' @field config 止损配置
                                 config = NULL,
                                 
                                 #' @field stop_loss_levels 止损水平记录
                                 stop_loss_levels = list(),
                                 
                                 #' @description 初始化止损控制器
                                 #' @param config_path 配置文件路径
                                 initialize = function(config_path = "config/risk_limits.yml") {
                                   self$config <- yaml::read_yaml(config_path)$stop_loss
                                 },
                                 
                                 #' @description 设置固定百分比止损
                                 #' @param symbol 交易标的
                                 #' @param entry_price 入场价格
                                 #' @param position_type 头寸类型 ("long" 或 "short")
                                 set_fixed_stop_loss = function(symbol, entry_price, position_type = "long") {
                                   stop_loss_pct <- self$config$fixed_percentage
                                   
                                   if (position_type == "long") {
                                     stop_loss_price <- entry_price * (1 - stop_loss_pct)
                                   } else {
                                     stop_loss_price <- entry_price * (1 + stop_loss_pct)
                                   }
                                   
                                   self$stop_loss_levels[[symbol]] <- list(
                                     type = "fixed",
                                     price = stop_loss_price,
                                     entry_price = entry_price,
                                     position_type = position_type,
                                     timestamp = Sys.time()
                                   )
                                   
                                   message(sprintf("为 %s 设置固定止损: %.4f", symbol, stop_loss_price))
                                 },
                                 
                                 #' @description 设置移动止损
                                 #' @param symbol 交易标的
                                 #' @param current_price 当前价格
                                 #' @param position_type 头寸类型
                                 #' @param trailing_pct 移动止损百分比
                                 set_trailing_stop_loss = function(symbol, current_price, position_type = "long", 
                                                                   trailing_pct = NULL) {
                                   if (is.null(trailing_pct)) {
                                     trailing_pct <- self$config$trailing_percentage
                                   }
                                   
                                   if (position_type == "long") {
                                     stop_loss_price <- current_price * (1 - trailing_pct)
                                   } else {
                                     stop_loss_price <- current_price * (1 + trailing_pct)
                                   }
                                   
                                   self$stop_loss_levels[[symbol]] <- list(
                                     type = "trailing",
                                     price = stop_loss_price,
                                     current_price = current_price,
                                     position_type = position_type,
                                     trailing_pct = trailing_pct,
                                     timestamp = Sys.time()
                                   )
                                   
                                   message(sprintf("为 %s 设置移动止损: %.4f", symbol, stop_loss_price))
                                 },
                                 
                                 #' @description 更新移动止损
                                 #' @param symbol 交易标的
                                 #' @param current_price 当前价格
                                 update_trailing_stop = function(symbol, current_price) {
                                   if (is.null(self$stop_loss_levels[[symbol]])) return(FALSE)
                                   
                                   stop_info <- self$stop_loss_levels[[symbol]]
                                   
                                   if (stop_info$type != "trailing") return(FALSE)
                                   
                                   trailing_pct <- stop_info$trailing_pct
                                   position_type <- stop_info$position_type
                                   
                                   if (position_type == "long") {
                                     new_stop_price <- current_price * (1 - trailing_pct)
                                     # 只向上移动止损
                                     if (new_stop_price > stop_info$price) {
                                       self$stop_loss_levels[[symbol]]$price <- new_stop_price
                                       self$stop_loss_levels[[symbol]]$current_price <- current_price
                                       return(TRUE)
                                     }
                                   } else {
                                     new_stop_price <- current_price * (1 + trailing_pct)
                                     # 只向下移动止损
                                     if (new_stop_price < stop_info$price) {
                                       self$stop_loss_levels[[symbol]]$price <- new_stop_price
                                       self$stop_loss_levels[[symbol]]$current_price <- current_price
                                       return(TRUE)
                                     }
                                   }
                                   
                                   return(FALSE)
                                 },
                                 
                                 #' @description 检查止损触发
                                 #' @param symbol 交易标的
                                 #' @param current_price 当前价格
                                 #' @return 逻辑值，是否触发止损
                                 check_stop_loss = function(symbol, current_price) {
                                   if (is.null(self$stop_loss_levels[[symbol]])) return(FALSE)
                                   
                                   stop_info <- self$stop_loss_levels[[symbol]]
                                   position_type <- stop_info$position_type
                                   
                                   if (position_type == "long") {
                                     triggered <- current_price <= stop_info$price
                                   } else {
                                     triggered <- current_price >= stop_info$price
                                   }
                                   
                                   if (triggered) {
                                     message(sprintf("止损触发: %s, 价格: %.4f, 止损价: %.4f", 
                                                     symbol, current_price, stop_info$price))
                                     # 移除止损记录
                                     self$stop_loss_levels[[symbol]] <- NULL
                                   }
                                   
                                   return(triggered)
                                 },
                                 
                                 #' @description 获取止损信息
                                 #' @param symbol 交易标的
                                 #' @return 止损信息列表
                                 get_stop_loss_info = function(symbol = NULL) {
                                   if (is.null(symbol)) {
                                     return(self$stop_loss_levels)
                                   } else {
                                     return(self$stop_loss_levels[[symbol]])
                                   }
                                 }
                               )
)