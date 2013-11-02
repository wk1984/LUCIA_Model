
CchangeFinder <- function(X,Field,Beg.Year,End.Year, rev=F){
 
 Ys <- Beg.Year:End.Year  
 Results <- rep(0,length(X[,1]))
 PX <- as.data.frame(X$PINX)
 colnames(PX) <- "PINX"
 yl <- dim(PX)[1]

for(j in 1:length(Ys)){

 odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Ys[j], ".accdb"))
 
 
    if(Field=="BldgNbr"){
      temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                    " FROM CommBldg", Ys[j]))
      temp <- pinCreate(temp)
      xx<-as.data.frame(table(temp$PINX))
      colnames(xx) <- c("PINX","NbrBldgs")
      PX <- merge(PX, xx, by.x="PINX", by.y="PINX", all.x=T)
      colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
    }
 
   if(Field=="BldgGrossSqFt"){
     temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                   " FROM CommBldg", Ys[j]))
     temp <- pinCreate(temp)
     xx<-as.data.frame(tapply(temp$BldgGrossSqFt,temp$PINX,sum))
     xx$PINX <- rownames(xx)
     PX <- merge(PX, xx, by.x="PINX", by.y="PINX", all.x=T)
     colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
   }
 
   if(Field=="Units"){
     
     if(j<3 | j > 8){
      temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, GrossSqFt as SF, ",
                                   " BldgNbr, SectionUse FROM CommSect", Ys[j]))
      temp <- pinCreate(temp)
      temp <- temp[temp$SectionUse == 300 | temp$SectionUse == 301 |
                    temp$SectionUse == 352 | temp$SectionUse == 984, ]
      xx<-as.data.frame(tapply(temp$SF,temp$PINX,sum))
      xx$PINX <- rownames(xx)
      xx$Units <- round(xx[,1]/800,0)
      xx <- xx[,c("PINX","Units")]
      PX <- merge(PX, xx, by.x="PINX", by.y="PINX", all.x=T)
      colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
     }
    if(j>=3 & j<=8){
      PX[,j+1] <- PX[,j]
    } 
   }
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
