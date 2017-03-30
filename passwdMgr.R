library(DBI)
library(ROracle)


passwd_mgr <- function(connection_params) {
  # SQL Statements to get, insert and update passwords for the current user
  query_passwords <- ' select  lower(cmdbuser) cmdbuser, db, lower(dbuser) dbuser
                             , DBMS_OBFUSCATION_TOOLKIT.DESDecrypt(input_string => dbpwd, key_string => :2) dbpassword
                       from    db_credentials
                       where   cmdbuser = upper(:1) '
  
  insert_password <- 'insert 
                      into    db_credentials (cmdbuser, db, dbuser, dbpwd)
                      values  (upper(:1), lower(:2), upper(:3), DBMS_OBFUSCATION_TOOLKIT.DESEncrypt(input_string => :4, key_string => :5))'
  
  update_password <- 'update  db_credentials
                      set     dbpwd = DBMS_OBFUSCATION_TOOLKIT.DESEncrypt(input_string => :1, key_string => :2)
                      where   cmdbuser = upper(:3)
                      and     db = lower(:4)
                      and     dbuser = upper(:5)'
  
  drv <- dbDriver("Oracle")

  # Defining local variables from the function parameters
  pwdb_username <- connection_params$pwdb_username
  pwdb_password <- connection_params$pwdb_password
  pwdb_name     <- connection_params$pwdb_name
  con <- dbConnect(drv, pwdb_username, pwdb_password, pwdb_name) 
  
  # Get the passwords from the database
  passwords <- dbGetQuery(con, query_passwords, data = data.frame(pwdb_password, pwdb_username))
  print("passwords loaded")
  
  colnames(passwords) <- tolower(colnames(passwords))
  print("colnames")
  
  # Get the current password list
  get_passwords <- function() {
    passwords
  }
  
  # Search the username and password for a particular database
  search <- function(a_db) {
    result <- passwords %>% 
      filter(db == a_db) %>% 
      select(username = dbuser, password = dbpassword)
    
    result
  }
  
  # add or update a password in the data frame and in the database
  update_password <- function(db, dbuser, dbpassword) {
    operation <- "none"
    print("update_password")
    
    if (is.null(passwords)) {
      print("passwords data frame is empty")
      operation <- "insert"
      passwords <<- rbind(passwords, c(tolower(pwdb_username), db, dbuser, dbpassword) )
    } else {
      print("passwords data frame not empty")
      
      currpw <- passwords[passwords$dbuser == dbuser & passwords$db == db, ]$dbpassword
      if (is.null(currpw) || identical(currpw, character(0))) {
        print("new password, inserting")
        operation <- "insert"
        passwords <<- rbind(passwords, c(tolower(pwdb_username), db, dbuser, dbpassword) )
      } else if (currpw != dbpassword) {
        print("existing password, updating")
        operation <- "update"
        passwords[passwords$dbuser == dbuser & passwords$db == db, ]$dbpassword <- dbpassword
      }
    }
    
    if (operation == "insert") {
      tryCatch({
        print("inserting password")
        rs <- dbSendQuery(con, insert_password, 
                          data = data.frame(pwdb_username, db, dbuser, dbpassword, pwdb_password))
        
        dbCommit(con)
        print("password inserted")
      },
      error = function(e) {
        print("error inserting new password")
        print(e)
      })
    } else if (operation == "update"){
      print("updating password")
      rs <- dbSendQuery(con, update_password, 
                        data = data.frame(dbpassword, pwdb_password, pwdb_username, db, dbuser))
      # execute(rs)
      # dbClearResult(rs)
      dbCommit(con)
      print("password updated")
    }
    
    print("passwords after operation")
  }
  
  return(list(update_password = update_password,
              get_passwords = get_passwords,
              search = search))
}