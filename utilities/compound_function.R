#compound function for trailing returns
compound_trail <- function(returnsTab,months, rolling, risk){
  
  returnsTab$MANAGER <- sapply(returnsTab$MANAGER, trimws)
  months <- as.numeric(months)
  riskIR <- risk
  risk <- (as.numeric(substr(risk,1,1)) * 12) - 1
  
  riskAnnualizer <- function(x){ switch(x, '1YR' = as.numeric(1/1),
                                        '3YR' = as.numeric(12/36),
                                        '5YR' = as.numeric(12/60)) }
  
  rtnAnnualizer <- function(x){ switch(as.character(x),
                                       "1" = as.numeric(1/1),
                                       "3" = as.numeric(1/1),
                                       "6" = as.numeric(1/1),
                                       "12" = as.numeric(1/1),
                                       "36" = as.numeric(12/36),
                                       "60" = as.numeric(12/60)) }
  
  if(!(months == 0)){
    
    returnsTot <- matrix(,nrow=0,ncol=9)  
    managers <- unique(returnsTab[,"MANAGER"])
    
    for(manager in managers){
      
      returns <- returnsTab[returnsTab$MANAGER == manager,]
      
      if(nrow(returns) < months) next
      
      x <- 1
      z <- 1
      
      if(!(rolling)){
        while(as.numeric(substr(returns[x,"MONTH_END"],6,7)) %% months != 0){
          x = x + 1
        }
      }
      
      results <- matrix(,nrow=floor((nrow(returns)-x)/months),ncol=9)
      if(rolling){results <- matrix(,nrow=floor((nrow(returns)) - months),ncol=9)}
      
      while(x + months <= length(returns[,1])){
        y <- x + months - 1
        
        results[z,1] <- returns[x,"MANAGER"]
        results[z,2] <- returns[x,"MONTH_END"]
        results[z,3] <- format(round((prod(1+returns[x:y,"MGD_RETURN"]/100)^rtnAnnualizer(months)-1)*100,2), nsmall = 2)
        results[z,4] <- format(round((prod(1+returns[x:y,"BMK_RETURN"]/100)^rtnAnnualizer(months)-1)*100,2), nsmall = 2)
        results[z,5] <- format(round(as.numeric(results[z,3]) - as.numeric(results[z,4]),2), nsmall = 2)
        results[z,6] <- returns[x,"MKT_VAL"]
        results[z,7] <- format(round(sd(returns[x:(x+risk),"MGD_RETURN"] - returns[x:(x+risk),"BMK_RETURN"])*(12 ^ 0.5),2), nsmall = 2)
        results[z,8] <- round((((prod(1+returns[x:(x+risk),"MGD_RETURN"]/100)^riskAnnualizer(riskIR)-1)*100) - 
                                 ((prod(1+returns[x:(x+risk),"BMK_RETURN"]/100)^riskAnnualizer(riskIR)-1)*100))/as.numeric(results[z,7]),2)
        results[z,9] <- returns[x,"BMK_NAME"]
        
        ifelse(rolling, x <- x + 1,  x <- x + months)
        z <- z + 1
      }
      
      returnsTot <- rbind(returnsTot, results)
      
    }
    
    returnsTab <- as.data.frame(returnsTot)
    names(returnsTab) <- c("MANAGER","MONTH_END", "MGD_RETURN", "BMK_RETURN", "ACTIVE","MKT_VAL","TE", "IR", "BMK_NAME") 
    
  } else {
    
    newCol <- c()
    for(x in 1:nrow(returnsTab)){
      y <- round(sd(returnsTab[x:(x+risk),"MGD_RETURN"] - returnsTab[x:(x+risk),"BMK_RETURN"])*(12 ^ 0.5),2)
      newCol <- c(newCol,y)
    }
    
    returnsTab$TE <- newCol
    
  }
  
  return(returnsTab)
  
}


#compound function for trailing returns
compound_multi <- function(returnsTab){
  
  returnsTab$MANAGER <- sapply(returnsTab$MANAGER, trimws)
  managers <- unique(returnsTab[,"MANAGER"])
  
  results <- matrix(,nrow=length(managers),ncol=15)
  
  currMonth <- as.numeric(substr(returnsTab[1,"MONTH_END"],6,7))
  
  cytd <- currMonth
  fytd <- ifelse(currMonth <= 6, currMonth + 6, currMonth - 6)
  qtd <- ifelse((currMonth %% 3) > 0, (currMonth %% 3), 3)
  
  z <- 1
  
  for(manager in managers){
    returns <- returnsTab[returnsTab$MANAGER == manager,] 
    monthCount <- nrow(returns)
    if(monthCount < 12){monthCount == 12}
    
    results[z,1] <- returns[1,"MANAGER"]
    results[z,2] <- returns[1,"MKT_VAL"] #MKT_VAL
    results[z,3] <- format(round(returns[1,"MGD_RETURN"] - returns[1,"BMK_RETURN"],2), nsmall = 2) #MTD
    results[z,4] <- format(round((prod(1+returns[1:qtd,"MGD_RETURN"]/100)-1)*100 - (prod(1+returns[1:qtd,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #QTD
    results[z,5] <- format(round((prod(1+returns[1:fytd,"MGD_RETURN"]/100)-1)*100 - (prod(1+returns[1:fytd,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #FYTD
    results[z,6] <- format(round((prod(1+returns[1:cytd,"MGD_RETURN"]/100)-1)*100 - (prod(1+returns[1:cytd,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #CYTD
    results[z,7] <- format(round((prod(1+returns[1:12,"MGD_RETURN"]/100)-1)*100 - (prod(1+returns[1:12,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #1YR
    results[z,8] <- format(round((prod(1+returns[1:24,"MGD_RETURN"]/100)^(1/2)-1)*100 - (prod(1+returns[1:24,"BMK_RETURN"]/100)^(1/2)-1)*100,2), nsmall = 2) #3YR
    results[z,9] <- format(round((prod(1+returns[1:36,"MGD_RETURN"]/100)^(1/3)-1)*100 - (prod(1+returns[1:36,"BMK_RETURN"]/100)^(1/3)-1)*100,2), nsmall = 2) #3YR
    results[z,10] <- format(round((prod(1+returns[1:48,"MGD_RETURN"]/100)^(1/4)-1)*100 - (prod(1+returns[1:48,"BMK_RETURN"]/100)^(1/4)-1)*100,2), nsmall = 2) #4YR
    results[z,11] <- format(round((prod(1+returns[1:60,"MGD_RETURN"]/100)^(1/5)-1)*100 - (prod(1+returns[1:60,"BMK_RETURN"]/100)^(1/5)-1)*100,2), nsmall = 2) #5YR
    results[z,12] <- format(round((prod(1+returns[1:72,"MGD_RETURN"]/100)^(1/6)-1)*100 - (prod(1+returns[1:72,"BMK_RETURN"]/100)^(1/6)-1)*100,2), nsmall = 2) #6YR
    results[z,13] <- format(round((prod(1+returns[1:84,"MGD_RETURN"]/100)^(1/7)-1)*100 - (prod(1+returns[1:84,"BMK_RETURN"]/100)^(1/7)-1)*100,2), nsmall = 2) #7YR
    results[z,14] <- format(round((prod(1+returns[1:96,"MGD_RETURN"]/100)^(1/8)-1)*100 - (prod(1+returns[1:96,"BMK_RETURN"]/100)^(1/8)-1)*100,2), nsmall = 2) #8YR
    results[z,15] <- format(round((prod(1+returns[,"MGD_RETURN"]/100)^(12/monthCount)-1)*100 - (prod(1+returns[,"BMK_RETURN"]/100)^(12/monthCount)-1)*100,2), nsmall = 2) #INCEPTION
    
    z <- z + 1
  }
  
  if(length(managers) == 1){
    results2 <- matrix(,nrow=2,ncol=15)
    
    results2[1,1] <- returns[1,"MANAGER"]
    results2[1,2] <- returns[1,"MKT_VAL"] #MKT_VAL
    results2[1,3] <- format(round(returns[1,"MGD_RETURN"],2), nsmall = 2) #MTD
    results2[1,4] <- format(round((prod(1+returns[1:qtd,"MGD_RETURN"]/100)-1)*100,2), nsmall = 2) #QTD
    results2[1,5] <- format(round((prod(1+returns[1:fytd,"MGD_RETURN"]/100)-1)*100,2), nsmall = 2) #FYTD
    results2[1,6] <- format(round((prod(1+returns[1:cytd,"MGD_RETURN"]/100)-1)*100,2), nsmall = 2) #CYTD
    results2[1,7] <- format(round((prod(1+returns[1:12,"MGD_RETURN"]/100)-1)*100,2), nsmall = 2) #1YR
    results2[1,8] <- format(round((prod(1+returns[1:24,"MGD_RETURN"]/100)^(1/2)-1)*100,2), nsmall = 2) #2YR
    results2[1,9] <- format(round((prod(1+returns[1:36,"MGD_RETURN"]/100)^(1/3)-1)*100,2), nsmall = 2) #3YR
    results2[1,10] <- format(round((prod(1+returns[1:48,"MGD_RETURN"]/100)^(1/4)-1)*100,2), nsmall = 2) #4YR
    results2[1,11] <- format(round((prod(1+returns[1:60,"MGD_RETURN"]/100)^(1/5)-1)*100,2), nsmall = 2) #5YR
    results2[1,12] <- format(round((prod(1+returns[1:72,"MGD_RETURN"]/100)^(1/6)-1)*100,2), nsmall = 2) #6YR
    results2[1,13] <- format(round((prod(1+returns[1:84,"MGD_RETURN"]/100)^(1/7)-1)*100,2), nsmall = 2) #7YR
    results2[1,14] <- format(round((prod(1+returns[1:96,"MGD_RETURN"]/100)^(1/8)-1)*100,2), nsmall = 2) #8YR
    results2[1,15] <- format(round((prod(1+returns[,"MGD_RETURN"]/100)^(12/monthCount)-1)*100,2), nsmall = 2) #INCEPTION
    
    results2[2,1] <- "Benchmark"
    results2[2,2] <- "--"
    results2[2,3] <- format(round(returns[1,"BMK_RETURN"],2), nsmall = 2) #MTD
    results2[2,4] <- format(round((prod(1+returns[1:qtd,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #QTD
    results2[2,5] <- format(round((prod(1+returns[1:fytd,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #FYTD
    results2[2,6] <- format(round((prod(1+returns[1:cytd,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #CYTD
    results2[2,7] <- format(round((prod(1+returns[1:12,"BMK_RETURN"]/100)-1)*100,2), nsmall = 2) #1YR
    results2[2,8] <- format(round((prod(1+returns[1:24,"BMK_RETURN"]/100)^(1/2)-1)*100,2), nsmall = 2) #2YR
    results2[2,9] <- format(round((prod(1+returns[1:36,"BMK_RETURN"]/100)^(1/3)-1)*100,2), nsmall = 2) #3YR
    results2[2,10] <- format(round((prod(1+returns[1:48,"BMK_RETURN"]/100)^(1/4)-1)*100,2), nsmall = 2) #4YR
    results2[2,11] <- format(round((prod(1+returns[1:60,"BMK_RETURN"]/100)^(1/5)-1)*100,2), nsmall = 2) #5YR
    results2[2,12] <- format(round((prod(1+returns[1:72,"BMK_RETURN"]/100)^(1/6)-1)*100,2), nsmall = 2) #6YR
    results2[2,13] <- format(round((prod(1+returns[1:84,"BMK_RETURN"]/100)^(1/7)-1)*100,2), nsmall = 2) #7YR
    results2[2,14] <- format(round((prod(1+returns[1:96,"BMK_RETURN"]/100)^(1/8)-1)*100,2), nsmall = 2) #8YR
    results2[2,15] <- format(round((prod(1+returns[,"BMK_RETURN"]/100)^(12/monthCount)-1)*100,2), nsmall = 2) #INCEPTION
    
    results <- rbind(results2, results)
    
    results[3,1] <- "Active"
    results[3,2] <- "--"
  }
  
  returns <- as.data.frame(results)
  returns <- as.data.frame(returns)
  names(returns) <- c("MANAGER","MKT_VAL","MONTH", "QTD", "FYTD", "CYTD","1YR", "2YR", 
                      "3YR", "4YR", "5YR", "6YR" ,"7YR","8YR","INCEPT") 
  
  return(returns)
}

#Stat Table function
statTable <- function(returnsTab){
  if(nrow(returnsTab) > 0 & !(names(returnsTab)[2] == "MKT_VAL")){
    returnsTab[,c("ACTIVE","BMK_RETURN")] <- apply(returnsTab[,c("ACTIVE","BMK_RETURN")],2,as.numeric)
    
    perc <- quantile(returnsTab$BMK_RETURN, c(1/3,2/3))
    retSeries <- list(returnsTab$ACTIVE, 
                      returnsTab[returnsTab$BMK_RETURN <= perc[1],"ACTIVE"], 
                      returnsTab[returnsTab$BMK_RETURN > perc[1] & returnsTab$BMK_RETURN < perc[2], "ACTIVE"], 
                      returnsTab[returnsTab$BMK_RETURN >= perc[2],"ACTIVE"])
    
    multi.fun <- function(x) {
      c(Average = sprintf("%.2f",mean(x)*100), 
        HitRt = sprintf("%.2f",length(x[x > 0])/length(x)),
        AvgWin = sprintf("%.2f",mean(x[x > 0])*100),
        AvgLoss = sprintf("%.2f",mean(x[x <= 0])*100),
        Max = sprintf("%.2f",max(x)*100),
        Min = sprintf("%.2f",min(x)*100))
    }
    
    #averages
    zz <- lapply(retSeries, multi.fun)
    zz <- do.call(cbind.data.frame, zz)
    names(zz) <- c("ALL","LEFT TAIL","NORMAL","RIGHT TAIL")
    
    return(zz)
  }
}

corrTable <- function(returnsTab){
  if(unique(length(returnsTab$MANAGER)) > 1){
    
    returnsTab[,c("ACTIVE","BMK_RETURN")] <- apply(returnsTab[,c("ACTIVE","BMK_RETURN")],2,as.numeric)
    
    w <- reshape(returnsTab[,c("MANAGER","MONTH_END","ACTIVE")],
                 timevar = "MANAGER", 
                 idvar= "MONTH_END", 
                 direction="wide")
    
    z <- as.data.frame(cor(w[,2:ncol(w)], use="pairwise.complete.obs"))
    
    rownames(z) <- gsub("ACTIVE.","", rownames(z))
    colnames(z) <- gsub("ACTIVE.","", colnames(z))
    
    return(z)
  }
}