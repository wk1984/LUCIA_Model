################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: PchangeFinder                                                #
#                                                                              #
#        Locates Changes to Parcel Record Type Data                            #                                                                                    ###          by Andy Krause                                                  ###
#                                                                              #
#        Most Recent Update: 11/13/2013                                        #
#                                                                              # 
###############################################################################'

PchangeFinder <- function(X, Field, Beg.Year, End.Year, rev=F){

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
 
    temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                  " FROM Parcel", Ys[j]))
    temp <- pinCreate(temp)
    PX <- merge(PX, temp[ ,c("PINX", Field)], by.x="PINX", by.y="PINX", all.x=T)
    colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j]) 
    
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
