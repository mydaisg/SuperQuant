# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' SQL查询构建器
#' 
#' 构建复杂的金融数据查询

#' 构建时间序列查询
build_timeseries_query <- function(symbols, start_date, end_date, 
                                   fields = "*", frequency = "daily") {
  base_query <- "
    SELECT {fields}
    FROM {table_name}
    WHERE symbol IN ({symbols})
    AND date BETWEEN '{start_date}' AND '{end_date}'
    AND frequency = '{frequency}'
    ORDER BY symbol, date
  "
  
  table_name <- ifelse(frequency == "daily", "daily_bars", "intraday_bars")
  symbols_str <- paste0("'", paste(symbols, collapse = "','"), "'")
  
  glue::glue(base_query)
}

#' 构建技术指标查询
build_technical_query <- function(symbol, indicator, period, lookback) {
  query <- "
    WITH rolling_stats AS (
      SELECT 
        date,
        symbol,
        close,
        AVG(close) OVER (
          PARTITION BY symbol 
          ORDER BY date 
          ROWS BETWEEN {period-1} PRECEDING AND CURRENT ROW
        ) as sma_{period},
        STDDEV(close) OVER (
          PARTITION BY symbol 
          ORDER BY date 
          ROWS BETWEEN {period-1} PRECEDING AND CURRENT ROW
        ) as volatility_{period}
      FROM daily_bars
      WHERE symbol = '{symbol}'
        AND date >= DATE_SUB(CURRENT_DATE, INTERVAL {lookback} DAY)
    )
    SELECT * FROM rolling_stats
    WHERE date >= DATE_SUB(CURRENT_DATE, INTERVAL {lookback} DAY)
    ORDER BY date
  "
  
  glue::glue(query)
}