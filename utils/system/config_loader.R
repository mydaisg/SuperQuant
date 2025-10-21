# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 配置加载器
#' 
#' 统一加载和管理系统配置

#' 加载YAML配置文件
load_config <- function(config_path, config_type) {
  if (!file.exists(config_path)) {
    stop(paste("Config file not found:", config_path))
  }
  
  config <- yaml::read_yaml(config_path)
  
  # 环境变量替换
  config <- replace_env_variables(config)
  
  # 验证配置
  validate_config(config, config_type)
  
  return(config)
}

#' 替换环境变量
replace_env_variables <- function(config) {
  config_str <- yaml::as.yaml(config)
  
  # 查找并替换 ${VAR_NAME} 格式的环境变量
  replaced_str <- stringr::str_replace_all(
    config_str, 
    "\\$\\{([^}]+)\\}", 
    function(x) {
      var_name <- stringr::str_sub(x, 3, -2)
      var_value <- Sys.getenv(var_name)
      if (var_value == "") {
        warning(paste("Environment variable", var_name, "not set"))
        return("")
      }
      return(var_value)
    }
  )
  
  return(yaml::yaml.load(replaced_str))
}

#' 验证配置
validate_config <- function(config, config_type) {
  if (config_type == "database") {
    required_fields <- c("host", "port", "dbname", "user")
  } else if (config_type == "trading") {
    required_fields <- c("broker", "account", "default_size")
  } else if (config_type == "risk") {
    required_fields <- c("max_position_size", "max_drawdown", "var_limit")
  }
  
  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    stop(paste("Missing required fields in", config_type, "config:", 
               paste(missing_fields, collapse = ", ")))
  }
}