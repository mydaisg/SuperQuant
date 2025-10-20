# strategies/alpha_research/strategy_framework.R
#' 策略基类
BaseStrategy <- R6::R6Class(
  "BaseStrategy",
  public = list(
    name = "base_strategy",
    parameters = list(),
    
    initialize = function(name, parameters = list()) {
      self$name <- name
      self$parameters <- parameters
    },
    
    # 策略逻辑 - 子类需要重写
    generate_signals = function(data) {
      stop("Method generate_signals must be implemented by subclass")
    },
    
    # 参数优化
    optimize_parameters = function(data, parameter_grid) {
      best_performance <- -Inf
      best_params <- NULL
      
      for (params in parameter_grid) {
        self$parameters <- params
        signals <- self$generate_signals(data)
        performance <- private$evaluate_parameters(signals, data)
        
        if (performance > best_performance) {
          best_performance <- performance
          best_params <- params
        }
      }
      
      return(list(
        best_parameters = best_params,
        best_performance = best_performance
      ))
    }
  )
)

#' 均值回归策略
MeanReversionStrategy <- R6::R6Class(
  "MeanReversionStrategy",
  inherit = BaseStrategy,
  public = list(
    initialize = function(lookback_period = 20, zscore_threshold = 2.0) {
      super$initialize("mean_reversion", list(
        lookback_period = lookback_period,
        zscore_threshold = zscore_threshold
      ))
    },
    
    generate_signals = function(data) {
      prices <- data$close
      
      # 计算Z-score
      rolling_mean <- zoo::rollmean(prices, self$parameters$lookback_period, fill = NA, align = "right")
      rolling_sd <- zoo::rollapply(prices, self$parameters$lookback_period, sd, fill = NA, align = "right")
      
      zscore <- (prices - rolling_mean) / rolling_sd
      
      # 生成信号
      signals <- ifelse(
        zscore > self$parameters$zscore_threshold, -1,  # 超买，卖出信号
        ifelse(
          zscore < -self$parameters$zscore_threshold, 1,  # 超卖，买入信号
          0  # 持有
        )
      )
      
      return(data.frame(
        date = index(data),
        signal = signals,
        zscore = zscore
      ))
    }
  )
)

#' 动量策略
MomentumStrategy <- R6::R6Class(
  "MomentumStrategy",
  inherit = BaseStrategy,
  public = list(
    initialize = function(lookback_period = 12, holding_period = 1) {
      super$initialize("momentum", list(
        lookback_period = lookback_period,
        holding_period = holding_period
      ))
    },
    
    generate_signals = function(data) {
      returns <- TTR::ROC(data$close, n = 1)
      momentum <- TTR::ROC(data$close, n = self$parameters$lookback_period)
      
      # 动量排名信号
      signals <- ifelse(
        momentum > 0, 1,  # 正动量，买入
        ifelse(
          momentum < 0, -1,  # 负动量，卖出
          0  # 持有
        )
      )
      
      return(data.frame(
        date = index(data),
        signal = signals,
        momentum = momentum
      ))
    }
  )
)