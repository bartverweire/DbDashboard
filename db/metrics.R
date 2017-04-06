library(shiny)
library(ggplot2)
library(DBI)
library(ROracle)
library(plotly)
library(tidyverse)
source("connMgr.R")

query_sysmetrics <- ' select  inst_id,begin_time,end_time,intsize_csec,group_id,metric_id,metric_name,value,metric_unit
                      from    gv$sysmetric_history'

default_metrics <- c('Average Synchronous Single-Block Read Latency', 
                     'CPU Usage Per Sec')

dbMetricsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(2, selectInput(ns("metric_name"),
                          "Metric",
                          choices = NULL,
                          multiple = TRUE)),
    column(10, plotlyOutput(ns("sysmetrics")))
  )
}

dbMetrics <- function(input, output, session, db, conn_mgr) {
  print("MODULE - metrics")
  drv <- dbDriver("Oracle")
  
  
  rv <- reactiveValues()
  rv$db_con <- dbConnect(drv, "sdbabvw","bcmdp123","db_cmdp.cmc.be")
  
  metric_data <- reactive({
    print("MODULE - metrics - load data")
    data <- dbGetQuery(rv$db_con, query_sysmetrics)
    
    colnames(data) <- tolower(colnames(data))
    print("MODULE - metrics - data loaded")
    data
  })
  
  metric_names <- reactive({
    print("MODULE - metrics - getting metric names")
    print(head(metric_data()))
    
    metric_data() %>% 
      select(metric_name) %>%
      distinct() %>%
      arrange()
  })
  
  observe({
    validate(need(db(), "MODULE - metrics, No database specified"))
    print(paste("MODULE - metrics, db changed to", db()))
    rv$db_con <<- conn_mgr$connect(db())
    print(paste("MODULE - metrics, db_con changed"))
  })
  # metrics_data <- reactive({
  #   print(paste("metrics_data reacting on ", db()))
    # validate(need(db_con(), 'Connection required'))
    # data <- dbGetQuery(db_con(), query_sysmetrics)
    # colnames(data) <- tolower(colnames(data))
    # 
    # head(data)
    # data
  # })
  # 
  # metric_names <- reactive({
  #   print("metric_names reactive")
  #   validate(need(metrics_data, 'metrics data required'))
  #   metric_names <- metrics_data %>% 
  #                       select(metric_name) %>%
  #                       distinct() %>%
  #                       arrange
  #   
  #   print(metric_names)
  #   metric_names
  # })
  # 
  # # Observe a connection to the cmdb
  observeEvent(metric_names, {
    print("MODULE - metrics - ObserveEvent : metric_names received")
    if (!is.null(metric_names())) {
      # update the drop down box containing databases
      print("MODULE - metrics - updating metric_name input")
      print(head(metric_names()))
      updateSelectInput(session, "metric_name",
                        # choices = dbInfo %>% select(core) %>% distinct() %>% arrange(),
                        choices = metric_names(),
                        selected = default_metrics)

    } else {
      print("MODULE - metrics - metric_names list not available")
    }
  })
  
  output$sysmetrics <- renderPlotly(generatePlot())

  generatePlot <- reactive({
    print("MODULE - metrics - Plotting sysmetrics")
    if (is.null(metric_data())) {
      p <- ggplot()

      return (p)
    }

    print("MODULE - metrics - Plotting sysmetrics with real data")
    plot_data <- subset(metric_data(), metric_name %in% default_metrics);

    p <- ggplot(plot_data, aes(x = begin_time, y = value, color = factor(inst_id))) +
      geom_line() +
      geom_point() +
      facet_wrap(~metric_name, ncol = 2)

    p <- ggplotly(p)

    p
  })
}