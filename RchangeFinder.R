
RchangeFinder <- function(X,Field,Beg.Year,End.Year, rev=F){
 
Ys <- Beg.Year:End.Year  
Results <- rep(0,length(X[,1]))
PX <- as.data.frame(X$PINX)
colnames(PX) <- "PINX"
yl <- dim(PX)[1]

a.Field <- "Field"
if(Field == "RecType"){
  a.Field <- "RecType"
  Field <- "PresentUse"
}

for(j in 1:length(Ys)){

 odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Ys[j], ".accdb"))
    
 if(Field=="NbrLivingUnits"){
   temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                 " FROM ResBldg", Ys[j]))
   temp <- pinCreate(temp)
   xx <- as.data.frame(tapply(temp$NbrLivingUnits, temp$PINX, sum))
   xx$PINX <- rownames(xx)
   colnames(xx) <- c("NbrUnits","PINX")
   PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
   colnames(PX)[dim(PX)[2]] <- paste0("Y",Ys[j])
 }     
 
 if(Field=="NbrBldgs"){
   temp <- sqlQuery(odbc, paste0("SELECT Major, Minor FROM ResBldg",
          Ys[j]))
      temp <- pinCreate(temp)
      xx <- as.data.frame(table(temp$PINX))    
      colnames(xx) <- c("PINX","NbrBldgs")
      PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
      colnames(PX)[dim(PX)[2]] <- paste0("Y",Ys[j])
 }     
 
 
 if(Field=="SqFtTotLiving"){
   temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                 " FROM ResBldg", Ys[j]))
   xx <- as.data.frame(tapply(temp$NbrLivingUnits, temp$PINX, sum))
   xx$PINX <- rownames(xx)
   colnames(xx) <- c("SF","PINX")
   PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
   colnames(PX)[dim(PX)[2]] <- paste0("Y",Ys[j])
 }     
 
 
 
 
odbcClose(odbc) 
} 
 
# Correct Vals

PX[is.na(PX)]<- -.1
 for(Q in 1:yl){
   zzz <- c(as.numeric(PX[Q,2]),as.numeric(PX[Q,2:(length(Ys))]))
   zzzz <- zzz-as.numeric(PX[Q,2:(length(Ys)+1)])
  if(Field != "RecType"){
     Results[Q] <- Ys[which(abs(zzzz) == max(abs(zzzz)))[1]]
  }

  if(Field == "RecType"){
    Results[Q] <- Ys[which(as.numeric(PX[Q,2:(length(Ys)+1)]) != 
                            as.numeric(PX[Q,2]))[1]]
  }
 }
return(Results)
}
