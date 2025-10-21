# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 工具函数初始化
#' 
#' 初始化所有工具函数并检查依赖

initialize_utils <- function() {
  # 检查必要包
  required_packages <- c(
    "DBI", "RPostgres", "yaml", "ggplot2", "scales",
    "stringr", "moments", "tseries", "nortest", "glue"
  )
  
  # 安装缺失的包
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # 创建工具函数环境
  utils_env <- new.env()
  
  # 加载所有工具函数
  utils_files <- list.files("utils", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  for (file in utils_files) {
    source(file, local = utils_env)
  }
  
  # 将工具函数附加到搜索路径
  attach(utils_env, name = "quant_utils")
  
  message("Utils initialized successfully")
  return(utils_env)
}

# 自动初始化
if (!exists(".UTILS_INITIALIZED")) {
  utils_env <- initialize_utils()
  .UTILS_INITIALIZED <- TRUE
}