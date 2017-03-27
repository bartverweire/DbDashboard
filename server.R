
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("passwdMgr.R")

shinyServer(function(input, output, session) {
  pw_mgr <- NULL;

  rv <- reactiveValues();
  
  
  # Return the UI for a modal dialog with username and password inputs. 
  loginModal <- function(db, failed = FALSE) {
    modalDialog(
      title=paste("Enter credentials for database",db),
      textInput("username", "Username",
                placeholder = 'sdba'
      ),
      passwordInput("password", "Password"),
      
      if (failed)
       div(tags$b("Invalid username or password", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("loginOk", "OK")
      )
    )
  }
  
  observe({
    if (is.null(rv$cmdpUser) | is.null(rv$cmdpPassword)) {
      showModal(loginModal(db = "cmdp"));
    } else {
      cat("not null");
    }
  })
  
  observeEvent(input$loginOk, {
    cat("observeEvent input$loginOk")
    if (is.null(rv$cmdbuser)) {
      rv$cmdpCon <- tryCatch({
        # open connection to cmdp database
        con <- dbConnect(drv, input$username, input$password, "db_cmdp.cmc.be")
        
        # store username and password for later use
        rv$cmdbuser <- input$username
        rv$cmdbpsw <- input$password
        
        # get information about databases
        dbInfo <- dbGetQuery(con, qDbInfo);
        cat("data fetched")
        
        # lowercase columns. Looks nicer than those loud uppercase columns
        colnames(dbInfo) <- tolower(colnames(dbInfo))
        
        pw_mgr <<- passwd_mgr(list(pwdb_username = input$username,
                                   pwdb_password = input$password,
                                   pwdb_name = 'db_cmdp.cmc.be'))

        pw_mgr$update_password('cmdp',rv$cmdbuser,rv$cmdbpsw)       
        
        removeModal();
        
        
        # update the drop down box containing databases
        updateSelectInput(session, "db", 
                          choices = dbInfo %>% select(core) %>% distinct() %>% arrange(),
                          selected = "cmdp")
        
        rv$dbInfo <- dbInfo
        
        con
      },
      error = function(e) {
        cat("Error trying to connect to cmdp");
        cat(e);
        
        showModal(loginModal(db = "cmdp", TRUE));
      },
      finally = function() {
        cat("finally")
        removeModal();
      })
    } else{
      # cmdb user is known, this is about getting the username/password for a normal database connection
      # store username and password for later use
      rv$dbuser <- input$username
      rv$dbpsw <- input$password
      
      rv$dbCon <- tryCatch({
        db_entry <- rv$dbInfo %>% filter(core == rv$db) %>% slice(1)
        cat(paste("Connecting to ", db_entry$db_full_name, "using ", rv$dbuser, rv$dbpsw ))
        con <- dbConnect(drv, rv$dbuser, rv$dbpsw, db_entry$db_full_name)
        
        pw_mgr$update_password(rv$db, rv$dbuser, rv$dbpsw)
        
        removeModal();
        # connection succeeds, password must be correct
        con
      },
      error = function(e) {
        cat(paste("Error trying to connect to ", rv$db));
       
        showModal(loginModal(rv$db, TRUE));
      },
      finally = function() {
        cat("finally")
        removeModal();
      })
    }
  })
  
  observeEvent(input$db, {
    cat("observeEvent input$db")
    if (identical(input$db, "")) {
      cat("input$db empty")
      return()
    }
    
    rv$db <- input$db
    
    tryCatch({
      if (is.null(pw_mgr)) return;
      
      cat("db change event")
      entry <- pw_mgr$search(input$db)

      cat("Entry")
      print (entry)
      
      # Test for an empty result. This will be returned as character(0)
      if (nrow(entry) == 0) {
        cat("Username/Password Not found");
        showModal(loginModal(db = input$db));
      } else  {
        rv$dbuser <- entry$dbuser
        rv$dbpsw <- entry$dbpassword
        
        rv$dbCon <- tryCatch({
          db_entry <- rv$dbInfo %>% filter(core == rv$db) %>% slice(1)
          cat(paste("Connecting to ", db_entry$db_full_name, "using ", rv$dbuser, rv$dbpsw ))
          con <- dbConnect(drv, rv$dbuser, rv$dbpsw, db_entry$db_full_name)
          
          # connection ok, updating password
          pw_mgr$update_password(rv$db, rv$dbuser, rv$dbpsw)
        
          con
        },
        error = function(e) {
          cat(paste("Error trying to connect to ", rv$db));
          
          showModal(loginModal(rv$db, TRUE));
        },
        finally = function() {
          cat("finally")
          removeModal();
        })
      }
    },
    error = function(e) {
      cat("error")
      cat(e)
    })
    
  })
  
  observeEvent(rv$dbCon, {
    cat("observeEvent rv$dbCon")
    rv$data_sysmetric <- dbGetQuery(rv$dbCon, qSysmetrics)
    colnames(rv$data_sysmetric) <<- tolower(colnames(rv$data_sysmetric))
    
  })
  
  output$sysmetrics <- renderPlotly(generatePlot())
  
  generatePlot <- function() {
    cat("Plotting sysmetrics")
    plot_data <- subset(rv$data_sysmetric, metric_name == 'Average Synchronous Single-Block Read Latency');
    
    p <- ggplot(plot_data, aes(x = begin_time, y = value, color = factor(inst_id))) + 
      geom_line() +
      geom_point()
    p <- ggplotly(p)
    
    p
  }

  
})
