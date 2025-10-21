# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 头寸限制控制
#' @description 监控和管理交易头寸限制
#' @export
PositionLimitControl <- R6::R6Class("PositionLimitControl",
                                    public = list(
                                      
                                      #' @field config 配置参数
                                      config = NULL,
                                      
                                      #' @description 初始化头寸限制控制器
                                      #' @param config_path 配置文件路径
                                      initialize = function(config_path = "config/risk_limits.yml") {
                                        self$config <- yaml::read_yaml(config_path)$position_limits
                                      },
                                      
                                      #' @description 检查单个头寸限制
                                      #' @param symbol 交易标的
                                      #' @param position 当前头寸
                                      #' @param order_size 订单大小
                                      #' @return 逻辑值，是否通过检查
                                      check_single_position = function(symbol, position, order_size) {
                                        max_position <- self$config$max_position_per_symbol
                                        proposed_position <- position + order_size
                                        
                                        if (abs(proposed_position) > max_position) {
                                          warning(sprintf("头寸限制违反: %s, 建议头寸: %.2f, 最大允许: %.2f", 
                                                          symbol, proposed_position, max_position))
                                          return(FALSE)
                                        }
                                        return(TRUE)
                                      },
                                      
                                      #' @description 检查组合总头寸限制
                                      #' @param portfolio_positions 组合头寸列表
                                      #' @param new_orders 新订单列表
                                      #' @return 逻辑值，是否通过检查
                                      check_portfolio_position = function(portfolio_positions, new_orders) {
                                        max_total_position <- self$config$max_total_position
                                        
                                        current_total <- sum(abs(portfolio_positions))
                                        new_total <- sum(abs(new_orders))
                                        proposed_total <- current_total + new_total
                                        
                                        if (proposed_total > max_total_position) {
                                          warning(sprintf("组合头寸限制违反: 建议总头寸: %.2f, 最大允许: %.2f", 
                                                          proposed_total, max_total_position))
                                          return(FALSE)
                                        }
                                        return(TRUE)
                                      },
                                      
                                      #' @description 检查集中度限制
                                      #' @param portfolio_positions 组合头寸
                                      #' @param portfolio_value 组合价值
                                      #' @return 逻辑值，是否通过检查
                                      check_concentration = function(portfolio_positions, portfolio_value) {
                                        concentration_limit <- self$config$concentration_limit
                                        
                                        if (length(portfolio_positions) == 0) return(TRUE)
                                        
                                        position_values <- abs(portfolio_positions)
                                        concentration_ratios <- position_values / portfolio_value
                                        
                                        violations <- concentration_ratios > concentration_limit
                                        
                                        if (any(violations)) {
                                          high_concentration <- names(which(violations))
                                          warning(sprintf("集中度限制违反: %s", paste(high_concentration, collapse = ", ")))
                                          return(FALSE)
                                        }
                                        return(TRUE)
                                      }
                                    )
)