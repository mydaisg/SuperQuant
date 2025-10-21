# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 滑点分析器
#' @description 专门分析交易滑点
#' @export
SlippageAnalyzer <- R6::R6Class("SlippageAnalyzer",
                                public = list(
                                  
                                  #' @field execution_analyzer 执行分析器
                                  execution_analyzer = NULL,
                                  
                                  #' @field slippage_data 滑点数据
                                  slippage_data = data.frame(),
                                  
                                  #' @field system_logger 系统日志
                                  system_logger = NULL,
                                  
                                  #' @description 初始化滑点分析器
                                  #' @param execution_analyzer 执行分析器
                                  #' @param system_logger 系统日志
                                  initialize = function(execution_analyzer = NULL, system_logger = NULL) {
                                    self$execution_analyzer <- execution_analyzer
                                    self$system_logger <- system_logger
                                    private$initialize_slippage_data()
                                  },
                                  
                                  #' @description 分析滑点模式
                                  #' @param order_ids 订单ID列表
                                  #' @return 滑点分析结果
                                  analyze_slippage_patterns = function(order_ids) {
                                    slippage_data <- data.frame()
                                    
                                    for (order_id in order_ids) {
                                      metrics <- self$execution_analyzer$analyze_order_execution(order_id)
                                      if (!is.null(metrics)) {
                                        slippage_record <- data.frame(
                                          order_id = order_id,
                                          symbol = metrics$symbol,
                                          side = metrics$side,
                                          order_type = metrics$order_type,
                                          strategy = metrics$strategy,
                                          quantity = metrics$total_quantity,
                                          slippage_bps = metrics$slippage_bps,
                                          execution_time = metrics$execution_time,
                                          hour = as.numeric(format(metrics$analysis_time, "%H")),
                                          stringsAsFactors = FALSE
                                        )
                                        slippage_data <- rbind(slippage_data, slippage_record)
                                      }
                                    }
                                    
                                    if (nrow(slippage_data) == 0) {
                                      return(list(
                                        total_orders = 0,
                                        message = "没有可分析的订单"
                                      ))
                                    }
                                    
                                    # 分析滑点模式
                                    patterns <- private$calculate_slippage_patterns(slippage_data)
                                    
                                    # 保存数据
                                    self$slippage_data <- rbind(self$slippage_data, slippage_data)
                                    
                                    if (!is.null(self$system_logger)) {
                                      self$system_logger$info(
                                        sprintf("滑点分析完成: %d个订单，平均滑点: %.2f bps", 
                                                nrow(slippage_data), patterns$overall$avg_slippage),
                                        component = "slippage_analyzer"
                                      )
                                    }
                                    
                                    return(list(
                                      total_orders = nrow(slippage_data),
                                      slippage_data = slippage_data,
                                      patterns = patterns
                                    ))
                                  },
                                  
                                  #' @description 生成滑点报告
                                  #' @return 滑点报告
                                  generate_slippage_report = function() {
                                    if (nrow(self$slippage_data) == 0) {
                                      return(list(message = "没有滑点数据"))
                                    }
                                    
                                    report <- list(
                                      generation_time = Sys.time(),
                                      total_records = nrow(self$slippage_data),
                                      overall_stats = private$calculate_overall_stats(),
                                      by_symbol = private$analyze_slippage_by_symbol(),
                                      by_side = private$analyze_slippage_by_side(),
                                      by_order_type = private$analyze_slippage_by_order_type(),
                                      by_strategy = private$analyze_slippage_by_strategy(),
                                      by_hour = private$analyze_slippage_by_hour(),
                                      recommendations = private$generate_slippage_recommendations()
                                    )
                                    
                                    return(report)
                                  },
                                  
                                  #' @description 获取滑点数据
                                  #' @param filters 过滤条件
                                  #' @return 滑点数据
                                  get_slippage_data = function(filters = NULL) {
                                    data <- self$slippage_data
                                    
                                    if (!is.null(filters)) {
                                      for (filter_name in names(filters)) {
                                        if (filter_name %in% colnames(data)) {
                                          data <- data[data[[filter_name]] %in% filters[[filter_name]], ]
                                        }
                                      }
                                    }
                                    
                                    return(data)
                                  }
                                ),
                                
                                private = list(
                                  
                                  #' @description 初始化滑点数据
                                  initialize_slippage_data = function() {
                                    self$slippage_data <- data.frame(
                                      order_id = character(),
                                      symbol = character(),
                                      side = character(),
                                      order_type = character(),
                                      strategy = character(),
                                      quantity = numeric(),
                                      slippage_bps = numeric(),
                                      execution_time = numeric(),
                                      hour = numeric(),
                                      stringsAsFactors = FALSE
                                    )
                                  },
                                  
                                  #' @description 计算滑点模式
                                  #' @param slippage_data 滑点数据
                                  calculate_slippage_patterns = function(slippage_data) {
                                    list(
                                      overall = list(
                                        avg_slippage = mean(slippage_data$slippage_bps, na.rm = TRUE),
                                        median_slippage = median(slippage_data$slippage_bps, na.rm = TRUE),
                                        std_slippage = sd(slippage_data$slippage_bps, na.rm = TRUE),
                                        worst_slippage = max(slippage_data$slippage_bps, na.rm = TRUE),
                                        best_slippage = min(slippage_data$slippage_bps, na.rm = TRUE)
                                      ),
                                      
                                      correlation = list(
                                        quantity_vs_slippage = cor(slippage_data$quantity, slippage_data$slippage_bps, use = "complete.obs"),
                                        time_vs_slippage = cor(slippage_data$execution_time, slippage_data$slippage_bps, use = "complete.obs")
                                      )
                                    )
                                  },
                                  
                                  #' @description 计算总体统计
                                  calculate_overall_stats = function() {
                                    data <- self$slippage_data
                                    list(
                                      total_orders = nrow(data),
                                      avg_slippage_bps = mean(data$slippage_bps, na.rm = TRUE),
                                      median_slippage_bps = median(data$slippage_bps, na.rm = TRUE),
                                      positive_slippage_pct = sum(data$slippage_bps > 0) / nrow(data) * 100,
                                      negative_slippage_pct = sum(data$slippage_bps < 0) / nrow(data) * 100
                                    )
                                  },
                                  
                                  #' @description 按标的分析滑点
                                  analyze_slippage_by_symbol = function() {
                                    data <- self$slippage_data
                                    symbols <- unique(data$symbol)
                                    result <- list()
                                    
                                    for (symbol in symbols) {
                                      symbol_data <- data[data$symbol == symbol, ]
                                      result[[symbol]] <- list(
                                        order_count = nrow(symbol_data),
                                        avg_slippage_bps = mean(symbol_data$slippage_bps, na.rm = TRUE),
                                        median_slippage_bps = median(symbol_data$slippage_bps, na.rm = TRUE)
                                      )
                                    }
                                    
                                    return(result)
                                  },
                                  
                                  #' @description 按买卖方向分析滑点
                                  analyze_slippage_by_side = function() {
                                    data <- self$slippage_data
                                    sides <- unique(data$side)
                                    result <- list()
                                    
                                    for (side in sides) {
                                      side_data <- data[data$side == side, ]
                                      result[[side]] <- list(
                                        order_count = nrow(side_data),
                                        avg_slippage_bps = mean(side_data$slippage_bps, na.rm = TRUE),
                                        median_slippage_bps = median(side_data$slippage_bps, na.rm = TRUE)
                                      )
                                    }
                                    
                                    return(result)
                                  },
                                  
                                  #' @description 按订单类型分析滑点
                                  analyze_slippage_by_order_type = function() {
                                    data <- self$slippage_data
                                    order_types <- unique(data$order_type)
                                    result <- list()
                                    
                                    for (order_type in order_types) {
                                      type_data <- data[data$order_type == order_type, ]
                                      result[[order_type]] <- list(
                                        order_count = nrow(type_data),
                                        avg_slippage_bps = mean(type_data$slippage_bps, na.rm = TRUE),
                                        median_slippage_bps = median(type_data$slippage_bps, na.rm = TRUE)
                                      )
                                    }
                                    
                                    return(result)
                                  },
                                  
                                  #' @description 按策略分析滑点
                                  analyze_slippage_by_strategy = function() {
                                    data <- self$slippage_data
                                    strategies <- unique(data$strategy)
                                    result <- list()
                                    
                                    for (strategy in strategies) {
                                      strategy_data <- data[data$strategy == strategy, ]
                                      result[[strategy]] <- list(
                                        order_count = nrow(strategy_data),
                                        avg_slippage_bps = mean(strategy_data$slippage_bps, na.rm = TRUE),
                                        median_slippage_bps = median(strategy_data$slippage_bps, na.rm = TRUE)
                                      )
                                    }
                                    
                                    return(result)
                                  },
                                  
                                  #' @description 按小时分析滑点
                                  analyze_slippage_by_hour = function() {
                                    data <- self$slippage_data
                                    hours <- 0:23
                                    result <- list()
                                    
                                    for (hour in hours) {
                                      hour_data <- data[data$hour == hour, ]
                                      if (nrow(hour_data) > 0) {
                                        result[[as.character(hour)]] <- list(
                                          order_count = nrow(hour_data),
                                          avg_slippage_bps = mean(hour_data$slippage_bps, na.rm = TRUE),
                                          median_slippage_bps = median(hour_data$slippage_bps, na.rm = TRUE)
                                        )
                                      }
                                    }
                                    
                                    return(result)
                                  },
                                  
                                  #' @description 生成滑点优化建议
                                  generate_slippage_recommendations = function() {
                                    data <- self$slippage_data
                                    recommendations <- list()
                                    
                                    # 分析模式并生成建议
                                    by_symbol <- private$analyze_slippage_by_symbol()
                                    by_hour <- private$analyze_slippage_by_hour()
                                    
                                    # 找出滑点最高的标的
                                    high_slippage_symbols <- names(which(sapply(by_symbol, function(x) x$avg_slippage_bps) > 5))
                                    if (length(high_slippage_symbols) > 0) {
                                      recommendations <- c(recommendations, 
                                                           sprintf("考虑优化 %s 的交易执行，当前平均滑点较高", 
                                                                   paste(high_slippage_symbols, collapse = ", ")))
                                    }
                                    
                                    # 找出滑点最高的时段
                                    high_slippage_hours <- names(which(sapply(by_hour, function(x) x$avg_slippage_bps) > 5))
                                    if (length(high_slippage_hours) > 0) {
                                      recommendations <- c(recommendations,
                                                           sprintf("避免在 %s 时段进行大额交易，滑点较高",
                                                                   paste(high_slippage_hours, collapse = ", ")))
                                    }
                                    
                                    return(recommendations)
                                  }
                                )
)