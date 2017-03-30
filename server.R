
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("passwdMgr.R")
source("connMgr.R")

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
      # if (!is.null(pw_mgr)) {
      #   print("Saving password")
      #   pw_mgr$update_password(db_name, username, password)
      # }
      
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
      # get some metadata from the database
      # dbInfo <- dbGetQuery(rv$cmdb_con, qDbInfo);
      # 
      # # lowercase columns. Looks nicer than those loud uppercase columns
      # colnames(dbInfo) <- tolower(colnames(dbInfo))
      # 
      # db_list <<- dbInfo$db_full_name
      # names(db_list) <<- dbInfo$core
      
      conn_mgr$setup(rv$cmdb_con, rv$cmdb_username, rv$cmdb_password, 'db_cmdp.cmc.be')
      # pw_mgr <<- passwd_mgr(list(pwdb_username = rv$cmdb_username,
      #                            pwdb_password = rv$cmdb_password,
      #                            pwdb_name     = 'db_cmdp.cmc.be'))
      
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
  })
  
#   observeEvent(input$db, {
#     print("observeEvent input$db")
#     if (identical(input$db, "")) {
#       cat("input$db empty")
#       return()
#     }
#     
#     rv$db <- input$db
#     
#     tryCatch({
#       if (is.null(pw_mgr)) return;
#       
#       cat("db change event")
#       entry <- pw_mgr$search(input$db)
# 
#       cat("Entry")
#       print (entry)
#       
#       # Test for an empty result. This will be returned as character(0)
#       if (nrow(entry) == 0) {
#         cat("Username/Password Not found");
#         showModal(loginModal(db = input$db));
#       } else  {
#         rv$dbuser <- entry$dbuser
#         rv$dbpsw <- entry$dbpassword
#         
#         rv$dbCon <- tryCatch({
#           db_entry <- rv$dbInfo %>% filter(core == rv$db) %>% slice(1)
#           cat(paste("Connecting to ", db_entry$db_full_name, "using ", rv$dbuser, rv$dbpsw ))
#           
# #          con <- dbConnect(drv, rv$dbuser, rv$dbpsw, db_entry$db_full_name)
#           con <- conn_mgr$connect(rv$dbuser, rv$dbpsw, db_entry$db_full_name)
#           
#           if (!is.null(con)) {
#             # connection ok, updating password
#             pw_mgr$update_password(rv$db, rv$dbuser, rv$dbpsw) 
#           } else {
#             
#           }
#         
#           con
#         },
#         error = function(e) {
#           cat(paste("Error trying to connect to ", rv$db));
#           
#           showModal(loginModal(rv$db, TRUE));
#         },
#         finally = function() {
#           cat("finally")
#           removeModal();
#         })
#       }
#     },
#     error = function(e) {
#       cat("error")
#       cat(e)
#     })
#     
#   })
#   
#   observeEvent(rv$dbCon, {
#     cat("observeEvent rv$dbCon")
#     rv$data_sysmetric <- dbGetQuery(rv$dbCon, qSysmetrics)
#     colnames(rv$data_sysmetric) <<- tolower(colnames(rv$data_sysmetric))
#     
#   })
  
  # output$sysmetrics <- renderPlotly(generatePlot())
  # 
  # generatePlot <- function() {
  #   cat("Plotting sysmetrics")
  #   if (is.null(rv$data_sysmetric)) {
  #     p <- ggplot()
  #     
  #     return (p)
  #   }
  #   
  #   plot_data <- subset(rv$data_sysmetric, metric_name == 'Average Synchronous Single-Block Read Latency');
  #   
  #   p <- ggplot(plot_data, aes(x = begin_time, y = value, color = factor(inst_id))) + 
  #     geom_line() +
  #     geom_point()
  #   p <- ggplotly(p)
  #   
  #   p
  # }

  
})
