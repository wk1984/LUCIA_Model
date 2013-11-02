
AchangeFinder <- function(X,Field,Beg.Year,End.Year, rev=F){
 
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
 
    temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                  " FROM AptComp", Ys[j]))
    temp <- pinCreate(temp)
    PX <- merge(PX, temp[ ,c("PINX", Field)], by.x="PINX", by.y="PINX", all.x=T)
    colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
    
 odbcClose(odbc)
 } # Closes J loop
    
# Correct Vals
  PX[is.na(PX)]<- -.1
  for(Q in 1:yl){
   zzz <- c(as.numeric(PX[Q, 2]), as.numeric(PX[Q, 2:(length(Ys))]))
   zzzz <- zzz - as.numeric(PX[Q, 2:(length(Ys) + 1)])
   Results[Q] <- Ys[which(abs(zzzz) == max(abs(zzzz)))[1]]
  }
return(Results)
}
