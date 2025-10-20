# monitoring/dashboard/dashboard_app.R
#' Shiny监控仪表板
QuantDashboard <- R6::R6Class(
  "QuantDashboard",
  public = list(
    initialize = function(portfolio, risk_manager, execution_engine) {
      private$portfolio <- portfolio
      private$risk_manager <- risk_manager
      private$execution_engine <- execution_engine
    },
    
    # 启动监控面板
    run_dashboard = function() {
      ui <- private$create_ui()
      server <- private$create_server()
      
      shiny::shinyApp(ui, server)
    }
  ),
  
  private = list(
    portfolio = NULL,
    risk_manager = NULL,
    execution_engine = NULL,
    
    create_ui = function() {
      shiny::navbarPage(
        "量化交易系统",
        theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),
        
        # 概览标签页
        shiny::tabPanel(
          "系统概览",
          shiny::fluidRow(
            shiny::valueBoxOutput("total_return"),
            shiny::valueBoxOutput("sharpe_ratio"),
            shiny::valueBoxOutput("current_var")
          ),
          shiny::fluidRow(
            shiny::column(8, shiny::plotOutput("equity_curve")),
            shiny::column(4, shiny::plotOutput("portfolio_allocation"))
          )
        ),
        
        # 风险监控标签页
        shiny::tabPanel(
          "风险监控",
          shiny::fluidRow(
            shiny::column(6, shiny::plotOutput("risk_metrics")),
            shiny::column(6, shiny::tableOutput("risk_violations"))
          )
        ),
        
        # 策略表现标签页
        shiny::tabPanel(
          "策略表现",
          shiny::fluidRow(
            shiny::column(12, shiny::plotOutput("strategy_performance"))
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::tableOutput("strategy_stats")),
            shiny::column(6, shiny::plotOutput("drawdown_chart"))
          )
        )
      )
    },
    
    create_server = function() {
      function(input, output, session) {
        # 自动刷新数据
        auto_invalidate <- shiny::reactiveTimer(10000)  # 10秒刷新
        
        # 系统概览
        output$total_return <- shiny::renderValueBox({
          auto_invalidate()
          performance <- private$portfolio$get_performance()
          shiny::valueBox(
            paste0(round(performance$total_return, 2), "%"),
            "总收益",
            icon = shiny::icon("chart-line"),
            color = ifelse(performance$total_return >= 0, "success", "danger")
          )
        })
        
        output$equity_curve <- shiny::renderPlot({
          auto_invalidate()
          private$plot_equity_curve()
        })
        
        # 风险监控
        output$risk_metrics <- shiny::renderPlot({
          auto_invalidate()
          risk_metrics <- private$risk_manager$monitor_portfolio_risk(private$portfolio)
          private$plot_risk_metrics(risk_metrics)
        })
      }
    },
    
    plot_equity_curve = function() {
      equity_data <- private$portfolio$get_equity_curve()
      
      ggplot2::ggplot(equity_data, ggplot2::aes(x = date, y = equity)) +
        ggplot2::geom_line(color = "#00AFBB", size = 1) +
        ggplot2::labs(title = "资金曲线", x = "日期", y = "权益") +
        ggplot2::theme_minimal()
    }
  )
)