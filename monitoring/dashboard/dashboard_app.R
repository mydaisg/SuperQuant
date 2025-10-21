# @author: my@daisg.com
# @date: 2025-10-21
# @version: 0.1
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
#' 量化交易系统监控面板
#' @description 基于Shiny的实时监控面板
#' @export
QuantDashboard <- R6::R6Class("QuantDashboard",
                              public = list(
                                
                                #' @field ui Shiny UI组件
                                ui = NULL,
                                
                                #' @field server Shiny Server函数
                                server = NULL,
                                
                                #' @field system_monitor 系统监控器实例
                                system_monitor = NULL,
                                
                                #' @field risk_manager 风险管理器实例
                                risk_manager = NULL,
                                
                                #' @description 初始化监控面板
                                #' @param system_monitor 系统监控器
                                #' @param risk_manager 风险管理器
                                initialize = function(system_monitor = NULL, risk_manager = NULL) {
                                  self$system_monitor <- system_monitor
                                  self$risk_manager <- risk_manager
                                  private$build_ui()
                                  private$build_server()
                                },
                                
                                #' @description 运行监控面板
                                #' @param host 主机地址
                                #' @param port 端口号
                                run_dashboard = function(host = "0.0.0.0", port = 3838) {
                                  if (!requireNamespace("shiny", quietly = TRUE)) {
                                    stop("请安装shiny包: install.packages('shiny')")
                                  }
                                  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
                                    stop("请安装shinydashboard包: install.packages('shinydashboard')")
                                  }
                                  
                                  app <- shiny::shinyApp(ui = self$ui, server = self$server)
                                  shiny::runApp(app, host = host, port = port, launch.browser = FALSE)
                                },
                                
                                #' @description 更新系统数据
                                #' @param system_data 系统数据
                                update_system_data = function(system_data) {
                                  private$current_data <- system_data
                                }
                              ),
                              
                              private = list(
                                current_data = list(),
                                
                                #' @description 构建UI界面
                                build_ui = function() {
                                  self$ui <- shinydashboard::dashboardPage(
                                    skin = "black",
                                    
                                    # 标题栏
                                    shinydashboard::dashboardHeader(
                                      title = "量化交易系统监控面板",
                                      shinydashboard::dropdownMenuOutput("alert_menu"),
                                      shinydashboard::dropdownMenuOutput("message_menu")
                                    ),
                                    
                                    # 侧边栏
                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenu(
                                        id = "tabs",
                                        shinydashboard::menuItem("系统概览", tabName = "overview", icon = shiny::icon("dashboard")),
                                        shinydashboard::menuItem("组合监控", tabName = "portfolio", icon = shiny::icon("chart-line")),
                                        shinydashboard::menuItem("风险监控", tabName = "risk", icon = shiny::icon("shield-alt")),
                                        shinydashboard::menuItem("交易执行", tabName = "execution", icon = shiny::icon("exchange-alt")),
                                        shinydashboard::menuItem("系统状态", tabName = "system", icon = shiny::icon("server")),
                                        shinydashboard::menuItem("警报中心", tabName = "alerts", icon = shiny::icon("bell")),
                                        shinydashboard::menuItem("日志查看", tabName = "logs", icon = shiny::icon("file-alt")),
                                        
                                        # 刷新控制
                                        shiny::br(),
                                        shiny::actionButton("refresh", "刷新数据", icon = shiny::icon("sync")),
                                        shiny::textOutput("last_update")
                                      )
                                    ),
                                    
                                    # 主体内容
                                    shinydashboard::dashboardBody(
                                      shiny::tags$head(
                                        shiny::tags$style(HTML("
              .small-box {min-height: 100px;}
              .info-box {min-height: 80px;}
            "))
                                      ),
                                      
                                      shinydashboard::tabItems(
                                        # 系统概览标签页
                                        shinydashboard::tabItem(
                                          tabName = "overview",
                                          shiny::fluidRow(
                                            # 关键指标
                                            shinydashboard::valueBoxOutput("total_pnl", width = 3),
                                            shinydashboard::valueBoxOutput("daily_pnl", width = 3),
                                            shinydashboard::valueBoxOutput("current_drawdown", width = 3),
                                            shinydashboard::valueBoxOutput("active_strategies", width = 3),
                                            
                                            # 性能图表
                                            shiny::column(
                                              width = 8,
                                              shinydashboard::box(
                                                title = "组合净值曲线", width = NULL, status = "primary",
                                                solidHeader = TRUE, plotly::plotlyOutput("equity_curve")
                                              )
                                            ),
                                            
                                            # 风险指标
                                            shiny::column(
                                              width = 4,
                                              shinydashboard::box(
                                                title = "风险指标", width = NULL, status = "warning",
                                                solidHeader = TRUE,
                                                shiny::tableOutput("risk_metrics")
                                              )
                                            ),
                                            
                                            # 实时警报
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "实时警报", width = NULL, status = "danger",
                                                solidHeader = TRUE,
                                                shiny::tableOutput("active_alerts")
                                              )
                                            ),
                                            
                                            # 系统状态
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "系统状态", width = NULL, status = "info",
                                                solidHeader = TRUE,
                                                shiny::tableOutput("system_status")
                                              )
                                            )
                                          )
                                        ),
                                        
                                        # 组合监控标签页
                                        shinydashboard::tabItem(
                                          tabName = "portfolio",
                                          shiny::fluidRow(
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "头寸分布", width = NULL, status = "primary",
                                                plotly::plotlyOutput("position_chart")
                                              )
                                            ),
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "行业暴露", width = NULL, status = "info",
                                                plotly::plotlyOutput("sector_exposure")
                                              )
                                            ),
                                            shiny::column(
                                              width = 12,
                                              shinydashboard::box(
                                                title = "持仓明细", width = NULL, status = "warning",
                                                DT::dataTableOutput("holdings_table")
                                              )
                                            )
                                          )
                                        ),
                                        
                                        # 风险监控标签页
                                        shinydashboard::tabItem(
                                          tabName = "risk",
                                          shiny::fluidRow(
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "回撤分析", width = NULL, status = "primary",
                                                plotly::plotlyOutput("drawdown_chart")
                                              )
                                            ),
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "VaR分析", width = NULL, status = "warning",
                                                plotly::plotlyOutput("var_chart")
                                              )
                                            ),
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "风险限制监控", width = NULL, status = "danger",
                                                shiny::tableOutput("risk_limits")
                                              )
                                            ),
                                            shiny::column(
                                              width = 6,
                                              shinydashboard::box(
                                                title = "止损监控", width = NULL, status = "info",
                                                shiny::tableOutput("stop_loss_monitor")
                                              )
                                            )
                                          )
                                        ),
                                        
                                        # 警报中心标签页
                                        shinydashboard::tabItem(
                                          tabName = "alerts",
                                          shiny::fluidRow(
                                            shiny::column(
                                              width = 12,
                                              shinydashboard::box(
                                                title = "警报配置", width = NULL, status = "primary",
                                                shiny::fluidRow(
                                                  shiny::column(4, shiny::selectInput("alert_type", "警报类型", 
                                                                                      choices = c("风险", "系统", "交易"))),
                                                  shiny::column(4, shiny::selectInput("alert_severity", "严重程度",
                                                                                      choices = c("低", "中", "高"))),
                                                  shiny::column(4, shiny::actionButton("add_alert_rule", "添加规则"))
                                                ),
                                                DT::dataTableOutput("alert_rules")
                                              )
                                            ),
                                            shiny::column(
                                              width = 12,
                                              shinydashboard::box(
                                                title = "历史警报", width = NULL, status = "warning",
                                                DT::dataTableOutput("alert_history")
                                              )
                                            )
                                          )
                                        ),
                                        
                                        # 日志查看标签页
                                        shinydashboard::tabItem(
                                          tabName = "logs",
                                          shiny::fluidRow(
                                            shiny::column(
                                              width = 12,
                                              shinydashboard::box(
                                                title = "系统日志", width = NULL, status = "primary",
                                                shiny::fluidRow(
                                                  shiny::column(3, shiny::selectInput("log_level", "日志级别",
                                                                                      choices = c("DEBUG", "INFO", "WARN", "ERROR"))),
                                                  shiny::column(3, shiny::dateInput("log_date", "日期", value = Sys.Date())),
                                                  shiny::column(3, shiny::textInput("log_search", "搜索关键词")),
                                                  shiny::column(3, shiny::actionButton("refresh_logs", "刷新日志", 
                                                                                       style = "margin-top: 25px;"))
                                                ),
                                                DT::dataTableOutput("system_logs")
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                },
                                
                                #' @description 构建Server逻辑
                                build_server = function() {
                                  self$server <- function(input, output, session) {
                                    
                                    # 反应式数据
                                    dashboard_data <- shiny::reactiveValues(
                                      portfolio = list(),
                                      risk = list(),
                                      system = list(),
                                      alerts = list(),
                                      last_update = Sys.time()
                                    )
                                    
                                    # 自动刷新
                                    auto_refresh <- shiny::reactiveTimer(10000)  # 10秒刷新
                                    
                                    # 观察自动刷新
                                    shiny::observe({
                                      auto_refresh()
                                      private$refresh_data()
                                      dashboard_data$last_update <- Sys.time()
                                    })
                                    
                                    # 手动刷新
                                    shiny::observeEvent(input$refresh, {
                                      private$refresh_data()
                                      dashboard_data$last_update <- Sys.time()
                                    })
                                    
                                    # 概览页 - 数值框
                                    output$total_pnl <- shinydashboard::renderValueBox({
                                      pnl <- ifelse(length(dashboard_data$portfolio$total_pnl) > 0, 
                                                    dashboard_data$portfolio$total_pnl, 0)
                                      shinydashboard::valueBox(
                                        value = sprintf("%.2f", pnl),
                                        subtitle = "总盈亏",
                                        icon = shiny::icon("dollar-sign"),
                                        color = ifelse(pnl >= 0, "green", "red")
                                      )
                                    })
                                    
                                    output$daily_pnl <- shinydashboard::renderValueBox({
                                      daily_pnl <- ifelse(length(dashboard_data$portfolio$daily_pnl) > 0, 
                                                          dashboard_data$portfolio$daily_pnl, 0)
                                      shinydashboard::valueBox(
                                        value = sprintf("%.2f", daily_pnl),
                                        subtitle = "当日盈亏",
                                        icon = shiny::icon("calendar-day"),
                                        color = ifelse(daily_pnl >= 0, "green", "red")
                                      )
                                    })
                                    
                                    output$current_drawdown <- shinydashboard::renderValueBox({
                                      dd <- ifelse(length(dashboard_data$risk$current_drawdown) > 0, 
                                                   dashboard_data$risk$current_drawdown * 100, 0)
                                      shinydashboard::valueBox(
                                        value = sprintf("%.2f%%", dd),
                                        subtitle = "当前回撤",
                                        icon = shiny::icon("chart-line"),
                                        color = ifelse(dd < 5, "green", ifelse(dd < 10, "yellow", "red"))
                                      )
                                    })
                                    
                                    output$active_strategies <- shinydashboard::renderValueBox({
                                      strategies <- ifelse(length(dashboard_data$system$active_strategies) > 0, 
                                                           dashboard_data$system$active_strategies, 0)
                                      shinydashboard::valueBox(
                                        value = strategies,
                                        subtitle = "活跃策略",
                                        icon = shiny::icon("cogs"),
                                        color = "purple"
                                      )
                                    })
                                    
                                    # 净值曲线
                                    output$equity_curve <- plotly::renderPlotly({
                                      if (length(dashboard_data$portfolio$equity_curve) == 0) {
                                        return(plotly::plotly_empty())
                                      }
                                      # 这里需要实际的净值数据
                                      private$plot_equity_curve(dashboard_data$portfolio$equity_curve)
                                    })
                                    
                                    # 风险指标表格
                                    output$risk_metrics <- shiny::renderTable({
                                      if (length(dashboard_data$risk$metrics) == 0) {
                                        return(data.frame(指标 = "无数据"))
                                      }
                                      dashboard_data$risk$metrics
                                    }, bordered = TRUE, striped = TRUE)
                                    
                                    # 活跃警报表格
                                    output$active_alerts <- shiny::renderTable({
                                      if (length(dashboard_data$alerts$active) == 0) {
                                        return(data.frame(警报 = "无活跃警报"))
                                      }
                                      dashboard_data$alerts$active
                                    }, bordered = TRUE, striped = TRUE)
                                    
                                    # 系统状态表格
                                    output$system_status <- shiny::renderTable({
                                      status_data <- data.frame(
                                        组件 = c("数据服务", "交易引擎", "风险监控", "策略引擎"),
                                        状态 = c("正常", "正常", "正常", "正常"),
                                        最后更新 = format(Sys.time(), "%H:%M:%S")
                                      )
                                      status_data
                                    }, bordered = TRUE, striped = TRUE)
                                    
                                    # 最后更新时间
                                    output$last_update <- shiny::renderText({
                                      sprintf("最后更新: %s", format(dashboard_data$last_update, "%H:%M:%S"))
                                    })
                                    
                                    # 警报菜单
                                    output$alert_menu <- shinydashboard::renderDropdownMenu({
                                      alerts <- dashboard_data$alerts$active
                                      if (length(alerts) == 0) {
                                        alerts_count <- 0
                                      } else {
                                        alerts_count <- nrow(alerts)
                                      }
                                      
                                      shinydashboard::dropdownMenu(
                                        type = "notifications",
                                        badgeStatus = ifelse(alerts_count == 0, "success", "danger"),
                                        .list = lapply(1:min(3, alerts_count), function(i) {
                                          shinydashboard::notificationItem(
                                            text = alerts$message[i],
                                            icon = shiny::icon("warning"),
                                            status = "danger"
                                          )
                                        })
                                      )
                                    })
                                  }
                                },
                                
                                #' @description 刷新数据
                                refresh_data = function() {
                                  # 这里实现从系统监控器和风险管理器获取数据的逻辑
                                  # 实际实现需要根据系统架构调整
                                  portfolio_data <- private$get_portfolio_data()
                                  risk_data <- private$get_risk_data()
                                  system_data <- private$get_system_data()
                                  alert_data <- private$get_alert_data()
                                  
                                  # 更新反应式数据
                                  if (!is.null(shiny::getDefaultReactiveDomain())) {
                                    shiny::isolate({
                                      dashboard_data$portfolio <- portfolio_data
                                      dashboard_data$risk <- risk_data
                                      dashboard_data$system <- system_data
                                      dashboard_data$alerts <- alert_data
                                    })
                                  }
                                },
                                
                                #' @description 获取组合数据
                                get_portfolio_data = function() {
                                  # 从组合管理器获取数据
                                  list(
                                    total_pnl = 100000,
                                    daily_pnl = 5000,
                                    equity_curve = data.frame(
                                      date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
                                      equity = cumsum(rnorm(31, 1000, 500)) + 100000
                                    )
                                  )
                                },
                                
                                #' @description 获取风险数据
                                get_risk_data = function() {
                                  # 从风险管理器获取数据
                                  list(
                                    current_drawdown = 0.02,
                                    metrics = data.frame(
                                      指标 = c("波动率", "夏普比率", "VaR(95%)", "最大回撤"),
                                      数值 = c("15.2%", "1.25", "-2.1%", "-8.5%")
                                    )
                                  )
                                },
                                
                                #' @description 获取系统数据
                                get_system_data = function() {
                                  # 从系统监控器获取数据
                                  list(
                                    active_strategies = 5,
                                    system_status = "正常"
                                  )
                                },
                                
                                #' @description 获取警报数据
                                get_alert_data = function() {
                                  # 从警报系统获取数据
                                  list(
                                    active = data.frame(
                                      时间 = format(Sys.time(), "%H:%M:%S"),
                                      类型 = "风险",
                                      严重程度 = "中",
                                      消息 = "回撤超过5%"
                                    )
                                  )
                                },
                                
                                #' @description 绘制净值曲线
                                plot_equity_curve = function(equity_data) {
                                  if (nrow(equity_data) == 0) return(plotly::plotly_empty())
                                  
                                  p <- plotly::plot_ly(equity_data, x = ~date, y = ~equity, type = 'scatter', mode = 'lines') %>%
                                    plotly::layout(
                                      title = "组合净值曲线",
                                      xaxis = list(title = "日期"),
                                      yaxis = list(title = "净值")
                                    )
                                  return(p)
                                }
                              )
)

#' 启动监控面板
#' @param system_monitor 系统监控器
#' @param risk_manager 风险管理器
#' @param host 主机地址
#' @param port 端口号
#' @export
start_dashboard <- function(system_monitor = NULL, risk_manager = NULL, host = "0.0.0.0", port = 3838) {
  dashboard <- QuantDashboard$new(system_monitor, risk_manager)
  dashboard$run_dashboard(host, port)
}