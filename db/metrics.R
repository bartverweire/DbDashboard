library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(DBI)

query_sysmetrics <- ' select  inst_id,begin_time,end_time,intsize_csec,group_id,metric_id,metric_name,value,metric_unit
                      from    gv$sysmetric_history'

default_metrics <- c('Average Synchronous Single-Block Read Latency', 
                     'CPU Usage Per Sec')

dbMetricsUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("metric_name"),
                  "Metric",
                  choices = NULL,
                  multiple = TRUE)
    ),
    mainPanel(
      uiOutput(ns("plot.region"))
      #plotlyOutput(ns("sysmetrics"))
    )
  )
}

dbMetrics <- function(input, output, session, db_con) {
  print("MODULE - metrics")
  
  rv <- reactiveValues()
  
  metric_data <- reactive({
    print("MODULE - metrics - load data")
    data <- dbGetQuery(db_con(), query_sysmetrics)
    
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
    print("MODULE - metrics - Observe : metric_data changed")
    validate(need(metric_data(), "No data available"))
    
    metric_names <- metric_data() %>% 
      select(metric_name) %>%
      distinct() %>%
      arrange()
    
    # update the drop down box containing databases
    print("MODULE - metrics - updating metric_name input")
    print(head(metric_names))
    updateSelectInput(session, "metric_name",
                      # choices = dbInfo %>% select(core) %>% distinct() %>% arrange(),
                      choices = metric_names,
                      selected = default_metrics)

    
  })
  
  output$plot.region <- renderUI({
    print("MODULE - metrics, define region")
    ns <- session$ns
    
    dyn_height <- 400 * ((length(input$metric_name) + 1) %/% 2)
    print(sprintf("MODULE - metrics, setting plot height to %d", dyn_height))
    plotlyOutput(ns("sysmetrics"), height = dyn_height )
  })
  
  output$sysmetrics <- renderPlotly(generatePlot())

  generatePlot <- reactive({
    print("MODULE - metrics - Plotting sysmetrics")
    if (is.null(metric_data())) {
      p <- ggplot()

      return (p)
    }

    print("MODULE - metrics - Plotting sysmetrics with real data")
    plot_data <- subset(metric_data(), metric_name %in% input$metric_name);

    p <- ggplot(plot_data, aes(x = begin_time, y = value, color = factor(inst_id)), labels = levels(factor(inst_id))) +
      geom_line() +
      geom_point() +
      facet_wrap(~metric_name, ncol = 2, scales = "free") + 
      scale_color_discrete("inst_id")

    p <- ggplotly(p)

    p
  })
}