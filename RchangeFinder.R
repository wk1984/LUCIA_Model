################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: AchangeFinder                                                #
#                                                                              #
#        Locates Changes to Residential Record Type Data                       #                                                                                    ###          by Andy Krause                                                  ###
#                                                                              #
#        Most Recent Update: 11/2/2013                                         #
#                                                                              # 
###############################################################################'

RchangeFinder <- function(X, Field, Beg.Year, End.Year, rev=F){

# 1.0 Prep Data ----------------------------------------------------------------
  
  # 1.1 Set sequence of years
  Ys <- Beg.Year:End.Year 
  
  # 1.2 Set up blank results
  Results <- rep(0,length(X[,1]))

  # 1.3 Isolate PIN numbers
  PX <- as.data.frame(X$PINX)
  colnames(PX) <- "PINX"

  # 1.4 Set up Length
  yl <- dim(PX)[1]

  # 1.5 Set Field
  a.Field <- "Field"
  if(Field == "RecType"){
    a.Field <- "RecType"
    Field <- "PresentUse"
  }

# 2.0 Read in Data  ------------------------------------------------------------

for(j in 1:length(Ys)){

  # 2.1 Start Loop
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Ys[j], ".accdb"))

  # 2.2 Read Lving Units Data and Merge to PX
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
 
  # 2.3 Read NbrBldgs Units Data and Merge to PX
  if(Field=="NbrBldgs"){
   temp <- sqlQuery(odbc, paste0("SELECT Major, Minor FROM ResBldg",
          Ys[j]))
      temp <- pinCreate(temp)
      xx <- as.data.frame(table(temp$PINX))    
      colnames(xx) <- c("PINX","NbrBldgs")
      PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
      colnames(PX)[dim(PX)[2]] <- paste0("Y",Ys[j])
 }     
 
  # 2.4 Read SFTotLiving Units Data and Merge to PX
  if(Field=="SqFtTotLiving"){
   temp <- sqlQuery(odbc, paste0("SELECT Major, Minor, ", Field,
                                 " FROM ResBldg", Ys[j]))
   temp <- pinCreate(temp)
   xx <- as.data.frame(tapply(temp$SqFtTotLiving, temp$PINX, sum))
   xx$PINX <- rownames(xx)
   colnames(xx) <- c("SF","PINX")
   PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
   colnames(PX)[dim(PX)[2]] <- paste0("Y",Ys[j])
 }     

  odbcClose(odbc) 
} 

# 3.0 Correct Time Values ------------------------------------------------------

# 3.1 Correct Vals

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


# 4.0 Return Results -----------------------------------------------------------

return(Results)
}
