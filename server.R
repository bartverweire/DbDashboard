
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("passwdMgr.R")
source("connMgr.R")
library(DBI)
library(ROracle)

shinyServer(function(input, output, session) {
  #pw_mgr <- NULL;
  conn_mgr <- conn_mgr(input, output, session);
  
  rv <- reactiveValues();
  # db_list <- c(cmdp = 'db_cmdp.cmc.be')
  
  # This is the starting point
  observe({
    print("Observing change in cmdb connection")
    if (is.null(rv$cmdp_con)) {
      con <- conn_mgr$connect('cmdp')
      
      if (is.null(con)) {
        print("Connection to cmdp not available")
        showModal(loginModal(db_name = 'cmdp'));
      } else {
        print("Connection to cmdp available")
        rv$cmdp_con <- con
      }
    } 
  })
  
  # Return the UI for a modal dialog with username and password inputs. 
  loginModal <- function(db_name, failed = FALSE) {
    print(paste("Calling loginModal with ", db_name))
    modalDialog(
      title=paste("Enter credentials for database",db_name),
      textInput("username", "Username",
                placeholder = 'sdba'
      ),
      passwordInput("password", "Password"),
      conditionalPanel(condition = "TRUE", 
                       textInput("db_name", "Database", value = db_name)
      ),

      if (failed)
       div(tags$b("Invalid username or password", style = "color: red;")),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("loginOk", "OK")
      )
    )
  }
  
  # Observe login event
  observeEvent(input$loginOk, {
    cat("observeEvent input$loginOk")
    
    username <- input$username
    password <- input$password
    db_name <- input$db_name
    
    con <- conn_mgr$connect(db_name, username, password)
    removeModal()
    
    if (!is.null(con)) {
      print("Connection succeeded")
      
      if (db_name == 'cmdp') {
        print("Connected to cmdp")
        rv$cmdb_con <- con
        rv$cmdb_username <- username
        rv$cmdb_password <- password 
      } else {
        print(paste("Connected to", db_name))
        rv$db_con <- con
      }
    } else {
      print("Connection failed, showing login form again")
      showModal(loginModal(db_name, failed = TRUE))
    }
  })
  
  # Observe a connection to the cmdb
  observeEvent(rv$cmdb_con, {
    print("ObserveEvent : cmdb connection changed")
    if (!is.null(rv$cmdb_con)) {
      conn_mgr$setup(rv$cmdb_con, rv$cmdb_username, rv$cmdb_password, 'db_cmdp.cmc.be')
      
      # update the drop down box containing databases
      updateSelectInput(session, "db",
                        # choices = dbInfo %>% select(core) %>% distinct() %>% arrange(),
                        choices = conn_mgr$get_db_list(),
                        selected = "cmdp")
      
    }
  })
  
  # Observe a change in selected database
  # Action is to get a connection, and to show the login form if no connection is returned
  observeEvent(input$db, {
    print("ObserveEvent : Selected database changed")
    db_name <- input$db
    print(db_name)
    
    if (!is.null(db_name)) {
      con <- conn_mgr$connect(db_name)
      
      if (!is.null(con)) {
        print(paste("Connection successful for ", db_name))
        rv$db_con <- con
      } else {
        print(paste("Connection failed for ", db_name))
        showModal(loginModal(db_name, failed = FALSE))
      }
    }
  },
  ignoreInit = TRUE)   
  
  observeEvent(rv$db_con, {
    print("ObserveEvent : Selected database connection changed")
    
    print("observeEvent rv$db_con")
    rv$data_sysmetric <- dbGetQuery(rv$db_con, qSysmetrics)
    colnames(rv$data_sysmetric) <<- tolower(colnames(rv$data_sysmetric))
  })
  
  db_con <- reactive({
    validate(need(rv$db_con, "Connection must be real"))
    rv$db_con
  })
  callModule(dbMetrics, "db.metrics", db_con)
  
})
