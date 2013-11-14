################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: CchangeFinder                                                #
#                                                                              #
#        Locates Changes to CommercialRecord Type Data                         #                                                                                    ###          by Andy Krause                                                  ###
#                                                                              #
#        Most Recent Update: 11/13/2013                                        #
#                                                                              # 
###############################################################################'

CchangeFinder <- function(X, Field, Beg.Year, End.Year, rev=F){
 
# 1.0 Prep Data ----------------------------------------------------------------
  
  # 1.1 Set sequence of years  
  Ys <- Beg.Year:End.Year  

  # 1.2 Set up blank results
  Results <- rep(0, length(X[ ,1]))
 
  # 1.3 Isolate PIN numbers
  PX <- as.data.frame(X$PINX)
  colnames(PX) <- "PINX"

  # 1.4 Set up Length
  yl <- dim(PX)[1]

# 2.0 Read in Data  ------------------------------------------------------------

# 2.1 Start Loop
for(j in 1:length(Ys)){

  # 2.2 Read Data and Merge to PX
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Ys[j], ".accdb"))
 
  # 2.3 Read BldgNbr Data and Merge to PX
    if(Field=="BldgNbr"){
      temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                    " FROM CommBldg", Ys[j]))
      temp <- pinCreate(temp)
      xx<-as.data.frame(table(temp$PINX))
      colnames(xx) <- c("PINX","NbrBldgs")
      PX <- merge(PX, xx, by.x="PINX", by.y="PINX", all.x=T)
      colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
    }
 
  # 2.4 Read BldgGrossSqFt Data and Merge to PX
   if(Field=="BldgGrossSqFt"){
     temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                   " FROM CommBldg", Ys[j]))
     temp <- pinCreate(temp)
     xx<-as.data.frame(tapply(temp$BldgGrossSqFt,temp$PINX,sum))
     xx$PINX <- rownames(xx)
     PX <- merge(PX, xx, by.x="PINX", by.y="PINX", all.x=T)
     colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
   }
 
  # 2.5 Read BldgNbr Data and Merge to PX
   if(Field=="Units"){
     
     if(j<3 | j > 8){
      temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, GrossSqFt as SF, ",
                                   " BldgNbr, SectionUse FROM CommSect", Ys[j]))
      temp <- pinCreate(temp)
      temp <- temp[temp$SectionUse == 300 | temp$SectionUse == 301 |
                    temp$SectionUse == 352 | temp$SectionUse == 984, ]
      xx<-as.data.frame(tapply(temp$SF, temp$PINX, sum))
      xx$PINX <- rownames(xx)
      xx$Units <- round(xx[ ,1] / 800, 0)
      xx <- xx[,c("PINX","Units")]
      PX <- merge(PX, xx, by.x="PINX", by.y="PINX", all.x=T)
      colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
     }
    if(j >= 3 & j <= 8){
      PX[ ,j+1] <- PX[ ,j]
    } 
   }
 odbcClose(odbc)
 } # Closes J loop

# 3.0 Correct Time Values ------------------------------------------------------

# 3.1 Correct Vals
  PX[is.na(PX)]<- -.1
  for(Q in 1:yl){
   zzz <- c(as.numeric(PX[Q, 2]), as.numeric(PX[Q, 2:(length(Ys))]))
   zzzz <- zzz - as.numeric(PX[Q, 2:(length(Ys) + 1)])
   Results[Q] <- Ys[which(abs(zzzz) == max(abs(zzzz)))[1]]
  }

# 4.0 Return Results -----------------------------------------------------------

return(Results)
}
