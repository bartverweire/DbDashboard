
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  fluidPage(
    # Sidebar with a slider input for number of bins
    tabsetPanel(
      tabPanel("DB", 
               verticalLayout(
                 selectInput("db",
                             label = NULL,
                             choices = NULL,
                             selectize = TRUE),
                 tabsetPanel(
                   tabPanel("Metrics", dbMetricsUI("db.metrics"))
                 )
               )
      ),
      tabPanel("Global")
    )
  )
)