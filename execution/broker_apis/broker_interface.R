# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 券商接口基类
#' @description 定义券商API的统一接口
#' @export
BrokerInterface <- R6::R6Class("BrokerInterface",
                               public = list(
                                 
                                 #' @field broker_name 券商名称
                                 broker_name = NULL,
                                 
                                 #' @field is_simulated 是否为模拟交易
                                 is_simulated = FALSE,
                                 
                                 #' @field connected 连接状态
                                 connected = FALSE,
                                 
                                 #' @field system_logger 系统日志
                                 system_logger = NULL,
                                 
                                 #' @description 初始化券商接口
                                 #' @param broker_name 券商名称
                                 #' @param is_simulated 是否为模拟交易
                                 #' @param system_logger 系统日志
                                 initialize = function(broker_name, is_simulated = FALSE, system_logger = NULL) {
                                   self$broker_name <- broker_name
                                   self$is_simulated <- is_simulated
                                   self$system_logger <- system_logger
                                 },
                                 
                                 #' @description 连接到券商
                                 #' @return 逻辑值，连接是否成功
                                 connect = function() {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 断开连接
                                 disconnect = function() {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 提交订单
                                 #' @param order 订单对象
                                 #' @return 提交结果
                                 submit_order = function(order) {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 取消订单
                                 #' @param broker_order_id 券商订单ID
                                 #' @return 取消结果
                                 cancel_order = function(broker_order_id) {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 获取订单状态
                                 #' @param broker_order_id 券商订单ID
                                 #' @return 订单状态
                                 get_order_status = function(broker_order_id) {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 获取账户信息
                                 #' @return 账户信息
                                 get_account_info = function() {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 获取持仓信息
                                 #' @return 持仓列表
                                 get_positions = function() {
                                   stop("必须由子类实现")
                                 },
                                 
                                 #' @description 获取市场数据
                                 #' @param symbol 交易标的
                                 #' @return 市场数据
                                 get_market_data = function(symbol) {
                                   stop("必须由子类实现")
                                 }
                               )
)