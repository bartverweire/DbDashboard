library(DBI)
library(ROracle)
library(shiny)
source("passwdMgr.R")

conn_mgr <- function(input, output, session) {
  query_test <- "select instance_name from v$instance"
  query_db_list <- " select   core, env, host_name, host_name_1, host_name_2, rac, db_name, lower(db_name||'.'||domain) db_full_name
                     from     config_global_mv
                     order by core"
  drv <- dbDriver("Oracle")
  
  # Initialize the password manager
  pw_mgr <- NULL
  
  # list containing all databases
  db_list <- c(cmdp = 'db_cmdp.cmc.be')
  connections <- list()
  
  connect <- function(db_name, username = NULL, password = NULL) {
    print(paste("Calling connect with", db_name, username, password))
    db_full_name <- db_list[db_name]
    print(paste("Connecting to ", db_full_name))
    
    connection <- tryCatch({
      con <- connections[[db_name]]
      
      if (!is.null(con) && test_connection(con, username, db_name)) {
        print(paste("Connection for", db_name, "returned from cache"))
        
        return (con);
      }
      
      if ((is.null(username) || is.null(password)) && !is.null(pw_mgr)) {
        # Get username and password from the password manager
        entry <- pw_mgr$search(db_name)
        print(entry)
        print(entry$username)
        print(entry$password)
        
        username <- entry$username
        password <- entry$password
      }
      
      if (!is.null(username) && !is.null(password)) {
        # create a new connection
        print("Create new connection")
        con <- dbConnect(drv, username, password, db_full_name)
        
        # test the connection
        if (test_connection(con, username, db_name)) {
          print("connections before")
          print(length(connections))
          connections[[db_name]] <<- con
          print("connections after")
          print(length(connections))
          # if the connection test succeeds, update the password manager
          if (!is.null(pw_mgr)) {
            pw_mgr$update_password(db_name, username, password)
          }
          
          return (con)
        }
      } 
    
      return (NULL)
    },
    error = function(e) {
      cat(paste("Error trying to connect to ", db_name));
      print(e);
      
      return (NULL)
    },
    finally = function() {
      cat("finally")
    })
  }
  
  setup <- function(con, db_name, username, password) {
    print(paste("Calling setup with ", db_name, username, password))
    # setup password manager
    pw_mgr <<- passwd_mgr(list(pwdb_username = db_name,
                               pwdb_password = username,
                               pwdb_name     = password))
    
    
    # get the list of databases
    dbInfo <- dbGetQuery(con, query_db_list);
    
    # lowercase columns. Looks nicer than those loud uppercase columns
    colnames(dbInfo) <- tolower(colnames(dbInfo))
    
    db_list <<- dbInfo$db_full_name
    names(db_list) <<- dbInfo$core
  }
  
  get_db_list <- function() {
    return (sort(names(db_list)))
  }
  # show_login <- function(db_name, failed = FALSE) {
  #   # Return the UI for a modal dialog with username and password inputs. 
  #   showModal(
  #     modalDialog(
  #       title=paste("Enter credentials for database",db_name),
  #       textInput("username", "Username",
  #                 placeholder = 'sdba'
  #       ),
  #       passwordInput("password", "Password"),
  #       conditionalPanel(condition = "FALSE", 
  #         textInput("db_name", "Database", value = db_name)
  #       ),
  #       if (failed)
  #         div(tags$b("Invalid username or password", style = "color: red;")),
  #       
  #       footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("loginOk", "OK")
  #       )
  #     )
  #   )
  # }
  
  test_connection <- function(connection, username, db_name) {
    sprintf("testing connection %s@%s", username, db_name)
    test <- dbGetQuery(connection, query_test)
   
    print(test) 
    nrow(test) == 1
  }
  
  # return list of callable functions
  list(connect = connect,
       setup = setup,
       get_db_list = get_db_list)
}