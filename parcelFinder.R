parcelFinder <- function(X,Type, Beg.Year,End.Year){
  
  Ys <- Beg.Year:End.Year  
  PX <- as.data.frame(X$PINX)
  colnames(PX) <- "PINX"
  yl <- dim(PX)[1]
  
  for(j in 1:length(Ys)){
    
    odbc <- odbcConnectAccess2007(paste0(
      "C://Dropbox//Data//WA//King//Assessor//Annual//King", Ys[j], ".accdb"))
    
    temp <- sqlQuery(odbc, paste0("SELECT Major, Minor FROM Parcel", Ys[j]))
    temp <- pinCreate(temp)
    xx <- as.data.frame(table(temp$PINX))    
    colnames(xx) <- c("PINX","Match")
    PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
    colnames(PX)[dim(PX)[2]] <- paste0("Y",Ys[j])
    odbcClose(odbc)
  } # Closes J loop
  
  PX[is.na(PX)] <- 0
  X$PChng.Year <- 0
  
  if(Type=="B"){
   for(Q in 1:yl){
    if(PX[Q,2]==0){X$PChng.Year[Q] <- -1}
    if(PX[Q,2]!=0){
      X$PChng.Year[Q] <- Ys[which(as.numeric(PX[Q,3:(length(Ys)+1)]) != 
                                      as.numeric(PX[Q,2]))[1]]    
    }
   }
  }  
  
  if(Type=="E"){
    for(Q in 1:yl){
      if(PX[Q,2]==1){X$PChng.Year[Q] <- -1}
      if(PX[Q,2]!=1){
        X$PChng.Year[Q] <- Ys[which(as.numeric(PX[Q,3:(length(Ys)+1)]) != 
                                      as.numeric(PX[Q,2]))[1]]    
      }
    }
  }
return(X)
}
