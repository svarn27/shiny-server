
qry <- function(string){
  
  library(RMySQL)
  
  hst <-"104.131.115.73"
  usr <- "shane"
  pwd <- "0117"
  dbTab <- "holdings"
  prt <- 3306
  
  con <- dbConnect(dbDriver("MySQL"), host=hst, user=usr, password=pwd ,db= dbTab, port=prt)
  
  result<- dbGetQuery(con, paste0(string))
  
  dbDisconnect(con)
  
  all_cons <- dbListConnections(MySQL())
  for(cons in all_cons){dbDisconnect(cons)}
  
  return(result)
  
}

app <- function(object, table){
  
  library(RMySQL)
  
  hst <-"104.131.115.73"
  usr <- "shane"
  pwd <- "0117"
  dbTab <- "holdings"
  prt <- 3306
  
  con <- dbConnect(dbDriver("MySQL"), host=hst, user=usr, password=pwd ,db= dbTab, port=prt)
  
  dbWriteTable(con, name=table, value=object, append=T, row.names=F)
  
  dbDisconnect(con)
  
  all_cons <- dbListConnections(MySQL())
  for(cons in all_cons){dbDisconnect(cons)}
  
}

sqlClean <- function(y){
  if(!is.null(y)){
    if(length(y) > 1){
      y <- lapply(y, function(x){paste0("'",x,"'")})
      y <- toString(y)
    } else { y <- paste0("'",y,"'")}
  }
  
  return(y)
}