# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 错误处理系统
#' 
#' 统一的错误处理和日志记录

#' 安全执行函数
safely_execute <- function(expr, error_value = NULL, warning_action = "log") {
  tryCatch({
    result <- eval(expr)
    return(result)
  }, warning = function(w) {
    handle_warning(w, warning_action)
    return(eval(expr)) # 重试
  }, error = function(e) {
    handle_error(e)
    return(error_value)
  })
}

#' 处理警告
handle_warning <- function(warning_obj, action = "log") {
  warning_msg <- paste("Warning:", warning_obj$message)
  
  if (action == "log") {
    log_message(warning_msg, level = "WARN")
  } else if (action == "alert") {
    send_alert("System Warning", warning_msg, level = "warning")
  }
}

#' 处理错误
handle_error <- function(error_obj) {
  error_msg <- paste("Error:", error_obj$message)
  
  # 记录错误日志
  log_message(error_msg, level = "ERROR")
  
  # 发送警报
  send_alert("System Error", error_msg, level = "error")
  
  # 记录堆栈跟踪
  log_message(paste("Stack trace:", sys.calls()), level = "DEBUG")
}

#' 重试机制
with_retry <- function(expr, max_retries = 3, delay = 1) {
  for (i in 1:max_retries) {
    result <- tryCatch({
      return(eval(expr))
    }, error = function(e) {
      if (i == max_retries) {
        stop(e)
      }
      Sys.sleep(delay * i) # 指数退避
      return(NULL)
    })
    
    if (!is.null(result)) {
      return(result)
    }
  }
}