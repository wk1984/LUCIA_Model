################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: parcelFinder                                                 #
#                                                                              #
#        Locates the year that a parcel joined or droppe                       #                                                                                    ###          by Andy Krause                                                  ###
#        From county database                                                  #
#                                                                              #
#        Most Recent Update: 11/2/2013                                         #
#                                                                              # 
################################################################################

parcelFinder <- function(X, Type, Beg.Year, End.Year){

# 0.1 Load Source Files --------------------------------------------------------
  
  source(paste0("D://Code//R//General//Geographic//WA//KingCounty//",
                "Coding_Functions.R"))  
  
# 0.2 Set up necessary variables -----------------------------------------------

 # 0.2.1 Range of Years
  Ys <- Beg.Year:End.Year

 # 0.2.2 List of PINs
  PX <- as.data.frame(X$PINX)
  colnames(PX) <- "PINX"
  
 # 0.2.3 Count Dimensions
  yl <- dim(PX)[1]
  
# 0.3 Loop through all PINs ----------------------------------------------------  
  
  for(j in 1:length(Ys)){
  
  # 0.3.1 Open up ODBC
    odbc <- odbcConnectAccess2007(paste0(
      "D://Data//WA//King//Assessor//King", Ys[j], ".accdb"))
    
  # 0.3.2 Read in and prep data
    temp <- sqlQuery(odbc, paste0("SELECT Major, Minor FROM Parcel", Ys[j]))
    temp <- pinCreate(temp)
    xx <- as.data.frame(table(temp$PINX))    
    colnames(xx) <- c("PINX", "Match")
  
  # 0.3.3 Merge to PINs  
    PX <- merge(PX, xx, by.x="PINX",by.y="PINX", all.x=T)
    colnames(PX)[dim(PX)[2]] <- paste0("Y", Ys[j])
  
  # 0.3.4 Close odbc  
    odbcClose(odbc)
  } # Closes J loop

# 0.4 Prep PIN data ------------------------------------------------------------

  PX[is.na(PX)] <- 0
  X$PChng.Year <- 0

# 0.5 Fix PIN change date if "B" parcel ----------------------------------------
  
  if(Type == "B"){
   for(Q in 1:yl){
    if(PX[Q,2] == 0){X$PChng.Year[Q] <- -1}
    if(PX[Q,2] != 0){
      X$PChng.Year[Q] <- Ys[which(as.numeric(PX[Q, 3:(length(Ys) + 1)]) != 
                                      as.numeric(PX[Q, 2]))[1]]    
    }
   } # ends Q for
  } # ends if


# 0.6 Fix PIN change date if "B" parcel ----------------------------------------

  if(Type == "E"){
    for(Q in 1:yl){
      if(PX[Q,2] == 1){X$PChng.Year[Q] <- -1}
      if(PX[Q,2] != 1){
        X$PChng.Year[Q] <- Ys[which(as.numeric(PX[Q, 3:(length(Ys) + 1)]) != 
                                      as.numeric(PX[Q, 2]))[1]]    
      }
    } # ends Q loop
  } # ends If

# 0.7 Returns Value ------------------------------------------------------------

X$PChng.Year[is.na(X$PChng.Year)] <- -1
return(X)
}
