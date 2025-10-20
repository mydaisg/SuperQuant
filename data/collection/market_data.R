# data/collection/market_data.R
#' 市场数据采集管理器
MarketDataManager <- R6::R6Class(
  "MarketDataManager",
  public = list(
    initialize = function(config_path = "config/database.yml") {
      private$db_config <- yaml::read_yaml(config_path)
      private$initialize_connections()
    },
    
    # 实时数据流
    start_real_time_feed = function(symbols, handlers) {
      private$setup_websocket_connections(symbols, handlers)
    },
    
    # 批量数据获取
    fetch_historical_data = function(symbol, start_date, end_date, frequency = "1d") {
      cache_key <- paste(symbol, start_date, end_date, frequency, sep = "_")
      
      # 缓存检查
      if (private$is_cached(cache_key)) {
        return(private$get_from_cache(cache_key))
      }
      
      # 多数据源获取
      data_sources <- c("yahoo", "alpha_vantage", "tiingo")
      data <- private$fetch_from_multiple_sources(symbol, start_date, end_date, frequency, data_sources)
      
      # 数据验证和清洗
      clean_data <- private$validate_and_clean(data)
      
      # 缓存结果
      private$save_to_cache(cache_key, clean_data)
      
      return(clean_data)
    }
  ),
  
  private = list(
    db_config = NULL,
    db_conn = NULL,
    cache = list(),
    
    initialize_connections = function() {
      # 初始化数据库连接
      private$db_conn <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = private$db_config$database,
        host = private$db_config$host,
        port = private$db_config$port,
        user = private$db_config$username,
        password = private$db_config$password
      )
    },
    
    fetch_from_multiple_sources = function(symbol, start_date, end_date, frequency, sources) {
      all_data <- list()
      
      for (source in sources) {
        tryCatch({
          data <- switch(source,
                         "yahoo" = quantmod::getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE),
                         "alpha_vantage" = private$fetch_alpha_vantage(symbol, frequency),
                         "tiingo" = private$fetch_tiingo(symbol, start_date, end_date, frequency)
          )
          all_data[[source]] <- data
        }, error = function(e) {
          warning("Failed to fetch from ", source, ": ", e$message)
        })
      }
      
      # 数据融合和质量检查
      merged_data <- private$merge_data_sources(all_data)
      return(merged_data)
    }
  )
)