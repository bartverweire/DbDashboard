
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  passwords <- data.frame()
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
      print("not null");
    }
  })
  
  observeEvent(input$loginOk, {
    print("observeEvent input$loginOk")
    if (is.null(rv$cmdbuser)) {
      rv$cmdpCon <- tryCatch({
        # open connection to cmdp database
        con <- dbConnect(drv, input$username, input$password, "db_cmdp.cmc.be")
        
        # store username and password for later use
        rv$cmdbuser <- input$username
        rv$cmdbpsw <- input$password
        
        # get information about databases
        dbInfo <- dbGetQuery(con, qDbInfo);
        print("data fetched")
        
        # lowercase columns. Looks nicer than those loud uppercase columns
        colnames(dbInfo) <- tolower(colnames(dbInfo))
        
        # load the passwords table
        passwords <<- dbGetQuery(con, qPasswords, data = data.frame(rv$cmdbpsw, rv$cmdbuser))
        # passwords <<- dbGetQuery(con, qPasswords, data = data.frame("bcmdp123", "sdbabvw"))
        print("passwords received")
        
        colnames(passwords) <<- tolower(colnames(passwords))
        print("colnames")
        print(paste("after",dim(passwords)))
        
        # update password for cmdb in the passwords table
        update_password(con, passwords, rv$cmdbuser, rv$cmdbpsw, 'cmdp', rv$cmdbuser, rv$cmdbpsw)
        
        print("Updating passwords drop down box")
        updateSelectInput(session, "passwords", choices = passwords %>% select(dbpassword) %>% arrange())
        
        removeModal();
        
        
        # update the drop down box containing databases
        updateSelectInput(session, "db", 
                          choices = dbInfo %>% select(core) %>% distinct() %>% arrange(),
                          selected = "cmdp")
        
        rv$dbInfo <- dbInfo
        
        con
      },
      error = function(e) {
        print("Error trying to connect to cmdp");
        print(e);
        
        showModal(loginModal(db = "cmdp", TRUE));
      },
      finally = function() {
        print("finally")
        removeModal();
      })
    } else{
      # cmdb user is known, this is about getting the username/password for a normal database connection
      # store username and password for later use
      rv$dbuser <- input$username
      rv$dbpsw <- input$password
      
      rv$dbCon <- tryCatch({
        db_entry <- rv$dbInfo %>% filter(core == rv$db) %>% slice(1)
        print(paste("Connecting to ", db_entry$db_full_name, "using ", rv$dbuser, rv$dbpsw ))
        con <- dbConnect(drv, rv$dbuser, rv$dbpsw, db_entry$db_full_name)
        
        update_password(rv$cmdpCon, passwords, rv$cmdbuser, rv$cmdbpsw, rv$db, rv$dbuser, rv$dbpsw)
        
        removeModal();
        # connection succeeds, password must be correct
        con
      },
      error = function(e) {
        print(paste("Error trying to connect to ", rv$db));
       
        showModal(loginModal(rv$db, TRUE));
      },
      finally = function() {
        print("finally")
        removeModal();
      })
    }
  })
  
  observeEvent(input$db, {
    print("observeEvent input$db")
    if (identical(input$db, "")) {
      print("input$db empty")
      return()
    }
    
    rv$db <- input$db
    
    tryCatch({
      if (is.null(passwords)) return;
      
      print("db change event")
      print(passwords)
      entry <- passwords %>% filter(db == input$db) %>% select(dbuser, dbpassword)
      print("Entry")
      print (entry)
      
      # Test for an empty result. This will be returned as character(0)
      if (nrow(entry) == 0) {
        print("Username/Password Not found");
        showModal(loginModal(db = input$db));
      } else  {
        rv$dbuser <- entry$dbuser
        rv$dbpsw <- entry$dbpassword
        
        rv$dbCon <- tryCatch({
          db_entry <- rv$dbInfo %>% filter(core == rv$db) %>% slice(1)
          print(paste("Connecting to ", db_entry$db_full_name, "using ", rv$dbuser, rv$dbpsw ))
          con <- dbConnect(drv, rv$dbuser, rv$dbpsw, db_entry$db_full_name)
          
          # connection ok, updating password
          update_password(rv$cmdpCon, passwords, rv$cmdbuser, rv$cmdbpsw, rv$db, rv$dbuser, rv$dbpsw)
        
          con
        },
        error = function(e) {
          print(paste("Error trying to connect to ", rv$db));
          
          showModal(loginModal(rv$db, TRUE));
        },
        finally = function() {
          print("finally")
          removeModal();
        })
      }
    },
    error = function(e) {
      print("error")
      print(e)
    })
    
  })
  
  observeEvent(rv$dbCon, {
    print("observeEvent rv$dbCon")
    rv$data_sysmetric <- dbGetQuery(rv$dbCon, qSysmetrics)
    colnames(rv$data_sysmetric) <<- tolower(colnames(rv$data_sysmetric))
    
  })
  
  output$sysmetrics <- renderPlotly({
    print("Plotting sysmetrics")
    plot_data <- subset(rv$data_sysmetric, metric_name == 'Average Synchronous Single-Block Read Latency');
    
    p <- ggplot(plot_data, aes(x = begin_time, y = value)) + geom_point(color="purple")
    p <- ggplotly(p)
    
    p
  })
  
  update_password <- function(con, passwords, cmdbuser, cmdbpassword, db, dbuser, dbpassword) {
    operation <- "none"
    pw <- passwords
    print("update_password - current passwords")
    print(pw)
    
    if (is.null(passwords)) {
      print("passwords data frame is empty")
      operation <- "insert"
      pw <- rbind(passwords, c(tolower(cmdbuser), db, dbuser, password) )
    } else {
      print("ok, passwords data frame not empty")
      
      currpw <- pw[pw$dbuser == dbuser & pw$db == db, ]$dbpassword
      if (is.null(currpw) || identical(currpw, character(0))) {
        print("new password, inserting")
        operation <- "insert"
        pw <- rbind(passwords, c(tolower(cmdbuser), db, dbuser, dbpassword) )
      } else if (currpw != dbpassword) {
        print("existing password,")
        operation <- "update"
        pw[pw$dbuser == dbuser & pw$db == db, ]$dbpassword <- dbpassword
      }
    }
    
    if (operation == "insert") {
      tryCatch({
        print("inserting password")
        rs <- dbSendQuery(con, iPassword, data = data.frame(cmdbuser, db, dbuser, dbpassword, cmdbpassword))
        # execute(rs)
        # dbClearResult(rs)
        dbCommit(con)
        print("password inserted")
      },
      error = function(e) {
        print("error inserting new password")
        print(e)
      })
    } else if (operation == "update"){
      print("updating password")
      rs <- dbSendQuery(con, uPassword, data = data.frame(dbpassword, cmdbpassword, cmdbuser, db, dbuser))
      # execute(rs)
      # dbClearResult(rs)
      dbCommit(con)
      print("password inserted")
    }
    
    print("passwords after operation")
    print(pw)
    pw
  }
  
  get_password <- function(passwords, db, dbuser) {
    passwords[passwords[dbuser] == dbuser & passwords[db] == db]$dbpassword
  }
  
})
