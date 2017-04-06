library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(lazyeval)
library(DBI)
library(ROracle)
library(htmlwidgets)
library(formattable)
library(plotly)


source("db/metrics.R")

drv <- dbDriver("Oracle")
# qDbInfo <- " select   core, env, host_name, host_name_1, host_name_2, rac, db_name, lower(db_name||'.'||domain) db_full_name
#              from     config_global_mv
#              order by core"

# qPasswords <- ' select  lower(cmdbuser) cmdbuser, db, lower(dbuser) dbuser
#                       , DBMS_OBFUSCATION_TOOLKIT.DESDecrypt(input_string => dbpwd, key_string => :2) dbpassword
#                 from    db_credentials
#                 where   cmdbuser = upper(:1) '

# iPassword <- 'insert 
#               into    db_credentials (cmdbuser, db, dbuser, dbpwd)
#               values  (upper(:1), lower(:2), upper(:3), DBMS_OBFUSCATION_TOOLKIT.DESEncrypt(input_string => :4, key_string => :5))'
# 
# uPassword <- 'update  db_credentials
#               set     dbpwd = DBMS_OBFUSCATION_TOOLKIT.DESEncrypt(input_string => :1, key_string => :2)
#               where   cmdbuser = upper(:3)
#               and     db = lower(:4)
#               and     dbuser = upper(:5)'

qSysmetrics <- 'select  inst_id,begin_time,end_time,intsize_csec,group_id,metric_id,metric_name,value,metric_unit
                 from    gv$sysmetric_history'

