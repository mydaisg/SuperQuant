# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 数据库连接管理
#' 
#' 管理不同类型的数据库连接，包括缓存、历史数据和实时数据

#' 创建数据库连接
#' @param config 数据库配置
#' @param db_type 数据库类型: "cache", "historical", "realtime"
create_db_connection <- function(config, db_type = "cache") {
  if (db_type == "cache") {
    # Redis或内存数据库连接
    conn <- redis_connection(config$redis)
  } else if (db_type == "historical") {
    # PostgreSQL或类似的历史数据库
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = config$historical$host,
      port = config$historical$port,
      dbname = config$historical$dbname,
      user = config$historical$user,
      password = config$historical$password
    )
  } else if (db_type == "realtime") {
    # 实时数据连接
    conn <- kdb_connection(config$kdb)
  }
  
  return(conn)
}

#' 安全执行数据库查询
safe_db_query <- function(conn, query, params = NULL, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      if (!is.null(params)) {
        result <- DBI::dbGetQuery(conn, query, params)
      } else {
        result <- DBI::dbGetQuery(conn, query)
      }
      return(result)
    }, error = function(e) {
      if (i == max_retries) {
        stop(paste("Database query failed after", max_retries, "attempts:", e$message))
      }
      Sys.sleep(2^i) # 指数退避
    })
  }
}

#' 批量数据插入
batch_insert <- function(conn, table_name, data, chunk_size = 1000) {
  chunks <- split(data, ceiling(seq_along(data[[1]])/chunk_size))
  
  lapply(chunks, function(chunk) {
    DBI::dbWriteTable(conn, table_name, chunk, append = TRUE, row.names = FALSE)
  })
}