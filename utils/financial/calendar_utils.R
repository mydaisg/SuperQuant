# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 交易日历工具
#' 
#' 处理交易日历和日期计算

#' 加载交易日历
load_trading_calendar <- function(market = "US", start_year = 2000, end_year = year(Sys.Date())) {
  calendar_file <- paste0("config/calendar_", tolower(market), ".csv")
  
  if (file.exists(calendar_file)) {
    calendar <- read.csv(calendar_file, stringsAsFactors = FALSE)
    calendar$date <- as.Date(calendar$date)
  } else {
    # 生成基础日历（实际使用时应连接交易所日历）
    calendar <- generate_base_calendar(start_year, end_year)
  }
  
  return(calendar)
}

#' 检查是否为交易日
is_trading_day <- function(date, market = "US") {
  calendar <- load_trading_calendar(market)
  date %in% calendar$date[calendar$is_trading_day]
}

#' 获取下一个交易日
next_trading_day <- function(date, n = 1, market = "US") {
  calendar <- load_trading_calendar(market)
  trading_days <- calendar$date[calendar$is_trading_day]
  
  current_idx <- which(trading_days == date)
  if (length(current_idx) == 0) {
    # 如果输入日期不是交易日，找到下一个交易日
    next_days <- trading_days[trading_days > date]
    return(next_days[min(n, length(next_days))])
  }
  
  next_idx <- current_idx + n
  if (next_idx > length(trading_days)) {
    warning("Requested trading day beyond available calendar")
    return(NA)
  }
  
  return(trading_days[next_idx])
}