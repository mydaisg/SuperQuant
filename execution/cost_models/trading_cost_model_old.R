# execution/cost_models/trading_cost_model.R
# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 交易成本模型
TradingCostModel <- R6::R6Class(
  "TradingCostModel",
  public = list(
    initialize = function(config_path = "config/trading_costs.yml") {
      private$cost_config <- yaml::read_yaml(config_path)
      private$initialize_cost_components()
    },
    
    # 估算交易成本
    estimate_trading_costs = function(order, market_data, urgency = "NORMAL") {
      cat("估算交易成本...\n")
      
      costs <- list()
      
      # 佣金和费用
      costs$commission <- private$calculate_commission(order)
      costs$fees <- private$calculate_fees(order)
      
      # 市场影响成本
      costs$market_impact <- private$estimate_market_impact(order, market_data, urgency)
      
      # 机会成本
      costs$opportunity_cost <- private$estimate_opportunity_cost(order, market_data)
      
      # 时间成本
      costs$timing_cost <- private$estimate_timing_cost(order, urgency)
      
      # 总成本
      costs$total_cost <- sum(unlist(costs), na.rm = TRUE)
      costs$total_cost_bps <- costs$total_cost / (order$quantity * market_data$current_price) * 10000
      
      return(costs)
    },
    
    # 优化执行策略
    optimize_execution_strategy = function(order, market_data, constraints = NULL) {
      cat("优化执行策略...\n")
      
      if (is.null(constraints)) {
        constraints <- private$cost_config$execution_constraints
      }
      
      # 评估不同执行策略
      strategies <- private$evaluate_execution_strategies(order, market_data, constraints)
      
      # 选择最优策略
      optimal_strategy <- private$select_optimal_strategy(strategies)
      
      return(list(
        optimal_strategy = optimal_strategy,
        all_strategies = strategies,
        optimization_metrics = private$calculate_optimization_metrics(strategies)
      ))
    },
    
    # 分析历史交易成本
    analyze_historical_costs = function(historical_trades, benchmark = NULL) {
      cat("分析历史交易成本...\n")
      
      analysis <- list()
      
      # 成本分解
      analysis$cost_decomposition <- private$decompose_historical_costs(historical_trades)
      
      # 趋势分析
      analysis$cost_trends <- private$analyze_cost_trends(historical_trades)
      
      # 与基准比较
      if (!is.null(benchmark)) {
        analysis$benchmark_comparison <- private$compare_with_benchmark(historical_trades, benchmark)
      }
      
      # 改进建议
      analysis$recommendations <- private$generate_cost_recommendations(analysis)
      
      return(analysis)
    },
    
    # 实时成本监控
    monitor_live_costs = function(executing_orders, market_data) {
      cat("监控实时交易成本...\n")
      
      live_monitoring <- list()
      
      for (order in executing_orders) {
        order_costs <- self$estimate_trading_costs(order, market_data)
        
        live_monitoring[[order$order_id]] <- list(
          order_info = order,
          estimated_costs = order_costs,
          cost_alerts = private$check_cost_alerts(order_costs)
        )
      }
      
      return(live_monitoring)
    }
  ),
  
  private = list(
    cost_config = NULL,
    cost_components = list(),
    
    initialize_cost_components = function() {
      private$cost_components <- list(
        explicit = c("commission", "fees", "taxes"),
        implicit = c("market_impact", "spread", "timing", "opportunity")
      )
    },
    
    calculate_commission = function(order) {
      commission_config <- private$cost_config$commission
      
      # 固定部分
      fixed_commission <- commission_config$fixed
      
      # 可变部分
      variable_commission <- order$quantity * commission_config$per_share * 
        ifelse(order$quantity > 0, 1, -1)
      
      # 总额部分
      total_commission <- abs(order$quantity) * order$limit_price * commission_config$percentage
      
      return(fixed_commission + variable_commission + total_commission)
    },
    
    calculate_fees = function(order) {
      fees_config <- private$cost_config$fees
      
      total_fees <- 0
      
      # 监管费
      total_fees <- total_fees + fees_config$regulatory * abs(order$quantity)
      
      # 交易活动费
      total_fees <- total_fees + fees_config$trading_activity * abs(order$quantity)
      
      # 结算费
      total_fees <- total_fees + fees_config$clearing
      
      return(total_fees)
    },
    
    estimate_market_impact = function(order, market_data, urgency) {
      impact_config <- private$cost_config$market_impact
      
      # 获取市场数据
      current_price <- market_data$current_price
      average_volume <- market_data$average_volume
      volatility <- market_data$volatility
      
      # 订单规模相对于平均交易量
      order_to_volume_ratio <- abs(order$quantity) / average_volume
      
      # 基础市场影响模型
      base_impact <- impact_config$base_rate * order_to_volume_ratio^impact_config$exponent
      
      # 波动率调整
      volatility_adjustment <- (volatility / impact_config$base_volatility)^impact_config$volatility_sensitivity
      
      # 紧急程度调整
      urgency_multiplier <- switch(urgency,
                                   LOW = impact_config$urgency_multipliers$low,
                                   NORMAL = impact_config$urgency_multipliers$normal,
                                   HIGH = impact_config$urgency_multipliers$high,
                                   impact_config$urgency_multipliers$normal
      )
      
      # 总市场影响
      total_impact <- base_impact * volatility_adjustment * urgency_multiplier * current_price
      
      return(total_impact * abs(order$quantity))
    },
    
    estimate_opportunity_cost = function(order, market_data) {
      # 简化实现：基于价格趋势估算机会成本
      price_trend <- market_data$price_trend  # 预期价格变动百分比
      execution_delay <- private$cost_config$opportunity_cost$average_delay
      
      expected_price_change <- price_trend * execution_delay / 365
      opportunity_cost <- expected_price_change * order$quantity * market_data$current_price
      
      return(abs(opportunity_cost))
    },
    
    estimate_timing_cost = function(order, urgency) {
      timing_config <- private$cost_config$timing_cost
      
      # 基于紧急程度的时间成本
      base_timing_cost <- timing_config$base_rate
      
      urgency_adjustment <- switch(urgency,
                                   LOW = timing_config$urgency_multipliers$low,
                                   NORMAL = timing_config$urgency_multipliers$normal,
                                   HIGH = timing_config$urgency_multipliers$high,
                                   timing_config$urgency_multipliers$normal
      )
      
      return(base_timing_cost * urgency_adjustment)
    },
    
    evaluate_execution_strategies = function(order, market_data, constraints) {
      strategies <- list()
      
      # 策略1: 立即执行（市价单）
      strategies$immediate <- private$evaluate_immediate_execution(order, market_data)
      
      # 策略2: 被动执行（限价单）
      strategies$passive <- private$evaluate_passive_execution(order, market_data, constraints)
      
      # 策略3: VWAP执行
      strategies$vwap <- private$evaluate_vwap_execution(order, market_data, constraints)
      
      # 策略4: TWAP执行
      strategies$twap <- private$evaluate_twap_execution(order, market_data, constraints)
      
      return(strategies)
    },
    
    evaluate_immediate_execution = function(order, market_data) {
      # 市价单：高市场影响，低机会成本
      immediate_order <- order
      immediate_order$order_type <- "MARKET"
      immediate_order$urgency <- "HIGH"
      
      costs <- self$estimate_trading_costs(immediate_order, market_data, "HIGH")
      
      return(list(
        strategy = "IMMEDIATE",
        order_type = "MARKET",
        estimated_costs = costs,
        expected_duration = private$cost_config$execution_times$immediate,
        fill_probability = 1.0
      ))
    },
    
    evaluate_passive_execution = function(order, market_data, constraints) {
      # 限价单：低市场影响，高机会成本
      passive_order <- order
      passive_order$order_type <- "LIMIT"
      passive_order$urgency <- "LOW"
      
      # 设置合理的限价
      if (order$quantity > 0) {
        # 买单：设置低于当前价
        passive_order$limit_price <- market_data$current_price * (1 - constraints$price_improvement)
      } else {
        # 卖单：设置高于当前价
        passive_order$limit_price <- market_data$current_price * (1 + constraints$price_improvement)
      }
      
      costs <- self$estimate_trading_costs(passive_order, market_data, "LOW")
      
      return(list(
        strategy = "PASSIVE",
        order_type = "LIMIT",
        estimated_costs = costs,
        expected_duration = private$cost_config$execution_times$passive,
        fill_probability = constraints$fill_probability_passive
      ))
    },
    
    evaluate_vwap_execution = function(order, market_data, constraints) {
      # VWAP执行：平均市场影响
      vwap_order <- order
      vwap_order$execution_algorithm <- "VWAP"
      
      # 估算VWAP成本（简化）
      base_costs <- self$estimate_trading_costs(order, market_data, "NORMAL")
      vwap_costs <- base_costs
      vwap_costs$market_impact <- base_costs$market_impact * 0.7  # VWAP通常降低市场影响
      vwap_costs$total_cost <- sum(unlist(vwap_costs), na.rm = TRUE)
      
      return(list(
        strategy = "VWAP",
        order_type = "ALGORITHMIC",
        estimated_costs = vwap_costs,
        expected_duration = private$cost_config$execution_times$vwap,
        fill_probability = constraints$fill_probability_vwap
      ))
    },
    
    evaluate_twap_execution = function(order, market_data, constraints) {
      # TWAP执行：时间加权平均
      twap_order <- order
      twap_order$execution_algorithm <- "TWAP"
      
      # 估算TWAP成本
      base_costs <- self$estimate_trading_costs(order, market_data, "NORMAL")
      twap_costs <- base_costs
      twap_costs$market_impact <- base_costs$market_impact * 0.8
      twap_costs$total_cost <- sum(unlist(twap_costs), na.rm = TRUE)
      
      return(list(
        strategy = "TWAP",
        order_type = "ALGORITHMIC",
        estimated_costs = twap_costs,
        expected_duration = private$cost_config$execution_times$twap,
        fill_probability = constraints$fill_probability_twap
      ))
    },
    
    select_optimal_strategy = function(strategies) {
      # 基于总成本选择最优策略
      best_strategy <- NULL
      best_cost <- Inf
      
      for (strategy_name in names(strategies)) {
        strategy <- strategies[[strategy_name]]
        total_cost <- strategy$estimated_costs$total_cost
        
        if (total_cost < best_cost && strategy$fill_probability > 0.5) {
          best_cost <- total_cost
          best_strategy <- strategy
        }
      }
      
      return(best_strategy)
    },
    
    calculate_optimization_metrics = function(strategies) {
      metrics <- list()
      
      costs <- sapply(strategies, function(s) s$estimated_costs$total_cost_bps)
      durations <- sapply(strategies, function(s) s$expected_duration)
      fill_probs <- sapply(strategies, function(s) s$fill_probability)
      
      metrics$cost_range <- range(costs, na.rm = TRUE)
      metrics$duration_range <- range(durations, na.rm = TRUE)
      metrics$average_fill_probability <- mean(fill_probs, na.rm = TRUE)
      metrics$cost_duration_tradeoff <- cor(costs, durations, use = "complete.obs")
      
      return(metrics)
    },
    
    decompose_historical_costs = function(historical_trades) {
      decomposition <- list(
        total_commission = sum(sapply(historical_trades, function(t) t$commission), na.rm = TRUE),
        total_fees = sum(sapply(historical_trades, function(t) t$fees), na.rm = TRUE),
        total_market_impact = sum(sapply(historical_trades, function(t) t$market_impact), na.rm = TRUE),
        average_cost_bps = mean(sapply(historical_trades, function(t) t$total_cost_bps), na.rm = TRUE)
      )
      
      decomposition$explicit_vs_implicit <- list(
        explicit = decomposition$total_commission + decomposition$total_fees,
        implicit = decomposition$total_market_impact
      )
      
      return(decomposition)
    },
    
    analyze_cost_trends = function(historical_trades) {
      if (length(historical_trades) == 0) {
        return(list())
      }
      
      # 按时间分组
      trade_dates <- sapply(historical_trades, function(t) as.Date(t$execution_time))
      unique_dates <- unique(trade_dates)
      
      daily_costs <- sapply(unique_dates, function(date) {
        date_trades <- historical_trades[trade_dates == date]
        mean(sapply(date_trades, function(t) t$total_cost_bps), na.rm = TRUE)
      })
      
      # 趋势分析
      if (length(daily_costs) > 1) {
        trend_model <- lm(daily_costs ~ seq_along(daily_costs))
        trend_slope <- coef(trend_model)[2]
      } else {
        trend_slope <- NA
      }
      
      return(list(
        daily_costs = daily_costs,
        trend_slope = trend_slope,
        is_improving = if (!is.na(trend_slope)) trend_slope < 0 else NA
      ))
    },
    
    compare_with_benchmark = function(historical_trades, benchmark) {
      actual_costs <- sapply(historical_trades, function(t) t$total_cost_bps)
      benchmark_costs <- benchmark$costs
      
      comparison <- list(
        mean_actual = mean(actual_costs, na.rm = TRUE),
        mean_benchmark = mean(benchmark_costs, na.rm = TRUE),
        cost_difference = mean(actual_costs, na.rm = TRUE) - mean(benchmark_costs, na.rm = TRUE),
        outperformance_ratio = mean(actual_costs, na.rm = TRUE) / mean(benchmark_costs, na.rm = TRUE)
      )
      
      # 统计显著性检验
      if (length(actual_costs) > 1 && length(benchmark_costs) > 1) {
        t_test <- t.test(actual_costs, benchmark_costs)
        comparison$significant_difference <- t_test$p.value < 0.05
        comparison$p_value <- t_test$p.value
      }
      
      return(comparison)
    },
    
    generate_cost_recommendations = function(analysis) {
      recommendations <- list()
      
      # 基于成本分解的建议
      cost_decomp <- analysis$cost_decomposition
      if (cost_decomp$explicit_vs_implicit$explicit > cost_decomp$explicit_vs_implicit$implicit) {
        recommendations <- c(recommendations, "显性成本较高，考虑协商更优的佣金费率")
      }
      
      if (cost_decomp$total_market_impact > cost_decomp$total_commission) {
        recommendations <- c(recommendations, "市场冲击成本较高，优化执行策略")
      }
      
      # 基于趋势的建议
      if (!is.na(analysis$cost_trends$is_improving) && !analysis$cost_trends$is_improving) {
        recommendations <- c(recommendations, "成本呈上升趋势，需要关注执行质量")
      }
      
      # 基于基准比较的建议
      if (!is.null(analysis$benchmark_comparison)) {
        bench_comp <- analysis$benchmark_comparison
        if (bench_comp$outperformance_ratio > 1.1) {
          recommendations <- c(recommendations, "成本显著高于基准，需要改进执行流程")
        }
      }
      
      if (length(recommendations) == 0) {
        recommendations <- "成本控制良好，继续保持"
      }
      
      return(recommendations)
    },
    
    check_cost_alerts = function(cost_estimate) {
      alerts <- list()
      
      # 检查总成本是否超过阈值
      if (cost_estimate$total_cost_bps > private$cost_config$alerts$total_cost_threshold_bps) {
        alerts <- c(alerts, paste("总成本过高:", round(cost_estimate$total_cost_bps, 2), "bps"))
      }
      
      # 检查市场冲击是否异常
      market_impact_ratio <- cost_estimate$market_impact / cost_estimate$total_cost
      if (market_impact_ratio > private$cost_config$alerts$market_impact_ratio_threshold) {
        alerts <- c(alerts, "市场冲击成本占比异常")
      }
      
      return(alerts)
    }
  )
)