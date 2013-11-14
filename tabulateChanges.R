################################################################################
#                                                                              #           
#         LUCIA MODEL: tabulateChanges() Function                              #                    
#         by Andy Krause                                                       #
#                                                                              #
#         Calculates land use changes between two sets of parcels              #
#         Over used-defined dimension(s)                                       #
#                                                                              #
#         Last Update: 10/29/2013                                              #
#                                                                              #
################################################################################

tabulateChanges <- function(clip.shp, beg.parcels, end.parcels, 
                            PH.object, beg.year, end.year, CType, par=0){

################################################################################  
# 0.0 Set parameters and values  -----------------------------------------------
  
# 0.1 Set packages
  require(rgeos)

# 0.2 Set Column names

  cNames <- c("# w/UL", "Units Lost", "# w/UG", "Units Gained", "Net Units",
              "# w/SL", "SqFt Lost", "# w/SG", "SqFt Gained", "Net SqFt")  

################################################################################  
# 1.0 Clip parcels to chosen extent    -----------------------------------------

# Future add code to: 1) check for X, Y in parcels; 2) if not, calculate
# They are being assumed now.

# 1.1 Clip beginning year parcels ----------------------------------------------

  # Clip to nearby 
  beg.near <- beg.parcels[beg.parcels@data$X > clip.shp@bbox[1, 1] &
                           beg.parcels@data$X < clip.shp@bbox[1, 2] &
                           beg.parcels@data$Y > clip.shp@bbox[2, 1] & 
                           beg.parcels@data$Y < clip.shp@bbox[2, 2], ]
  # Conver to SPDF  
  beg.near <- SpatialPointsDataFrame(cbind(beg.near@data$X, beg.near@data$Y),
                                      beg.near@data)
  # Calculate Intersects and clip
  clip.int <- gIntersects(clip.shp, beg.near, byid=T)
  beg.clip <- beg.near[which(clip.int), ]
 
# 1.2 Clip end year parcels ----------------------------------------------------

  # Clip to nearby
  end.near <- end.parcels[end.parcels@data$X > clip.shp@bbox[1, 1] &
                          end.parcels@data$X < clip.shp@bbox[1, 2] &
                          end.parcels@data$Y > clip.shp@bbox[2, 1] & 
                          end.parcels@data$Y < clip.shp@bbox[2, 2], ]
  # Conver to SPDF  
  end.near <- SpatialPointsDataFrame(cbind(end.near@data$X, end.near@data$Y),
                                   end.near@data)
  # Calculate Intersects and clip
  clip.int <- gIntersects(clip.shp, end.near, byid=T)
  end.clip <- end.near[which(clip.int), ]

################################################################################
# 2.0 Locate Records with Changes ----------------------------------------------
   
# 2.1 Identify all changes to use ----------------------------------------------
  
 # 2.1.1 From Beginning Only Parcels
   PH.b <- merge(PH.object[PH.object$Count.Type == "B",],
                 beg.clip@data[,c("PINX", "X")],
                 by.x="PINX", by.y="PINX")
   PH.b$X <- NULL
 
 # 2.1.2 From End and All Parcels 
   PH.e <- merge(PH.object[PH.object$Count.Type != "B", ],
                 end.clip@data[ ,c("PINX", "X")],
                 by.x="PINX", by.y="PINX")
   PH.e$X <- NULL
 
 # 2.1.3 If some Beg Parcels exist
   if(dim(PH.b)[1] >= 1){
     PwC <- rbind(PH.b, PH.e)
     PwC <- PwC[PwC$Chng.Time > beg.year & PwC$Chng.Time < end.year, ]
   }
 
 # 2.1.4 If only end parcels
   if(dim(PH.b)[1] == 0){
     PwC <- PH.e[PH.e$Chng.Time > beg.year & PH.e$Chng.Time < end.year, ]
   }
 
# 2.2 If Raw, Return Data ------------------------------------------------------
 
  if(CType == "Raw") return(PwC)
 
################################################################################
# 3.0 Tally Overall Changes ----------------------------------------------------  
    
if(CType == "All"){
   
 
# 3.1 Calculate Changes --------------------------------------------------------
     
   All.Change <- NULL
   All.Change[1] <- length(which(PwC$Loss.Units != 0))
   All.Change[2] <- -sum(PwC$Loss.Units)
   All.Change[3] <- length(which(PwC$Gain.Units != 0))
   All.Change[4] <- sum(PwC$Gain.Units)
   All.Change[5] <- All.Change[2] + All.Change[4]
   
   All.Change[6] <- length(which(PwC$Loss.SF[PwC$B.Class == "C"] != 0))
   All.Change[7] <- -sum(PwC$Loss.SF[PwC$B.Class == "C"])
   All.Change[8] <- length(which(PwC$Gain.SF[PwC$E.Class == "C"] != 0)) 
   All.Change[9] <- sum(PwC$Gain.SF[PwC$E.Class == "C"])
   All.Change[10] <- All.Change[7] + All.Change[9]

# 3.2 Return Value -------------------------------------------------------------

   return(All.Change)   
 }
     

###############################################################################
#4.0 Tally Changes over TIme --------------------------------------------------
  
 if(CType == "Time"){

# 4.1 Set up Variables ---------------------------------------------------------
   
   tmin <- 2000
   tmax <- max(PH$Chng.Time)
   Time.Change <- matrix(ncol=10, nrow=(length(tmin:tmax) + 1))
   
# 4.2 Calculate Changes --------------------------------------------------------
   
   for(Y in tmin:tmax){
     YY <- PwC[PwC$Chng.Time == Y, ]
     Time.Change[Y - (tmin - 1), 1] <- length(which(YY$Loss.Units != 0))
     Time.Change[Y - (tmin - 1), 2] <- -sum(YY$Loss.Units)
     Time.Change[Y - (tmin - 1), 3] <- length(which(YY$Gain.Units != 0))
     Time.Change[Y - (tmin - 1), 4] <- sum(YY$Gain.Units)
     Time.Change[Y - (tmin - 1), 5] <- sum(Time.Change[Y - (tmin - 1), c(2, 4)])
     Time.Change[Y - (tmin - 1), 6] <- length(which(
                                          YY$Loss.SF[YY$B.Class == "C"] != 0))
     Time.Change[Y - (tmin - 1), 7] <- -sum(YY$Loss.SF[YY$B.Class == "C"])
     Time.Change[Y - (tmin - 1), 8] <- length(which(
                                          YY$Gain.SF[YY$E.Class == "C"] != 0)) 
     Time.Change[Y - (tmin - 1), 9] <- sum(YY$Gain.SF[YY$E.Class == "C"])
     Time.Change[Y - (tmin - 1), 10] <- sum(Time.Change[Y - (tmin - 1),
                                                              c(7, 9)])
   }
  
# 4.3 Summarize and Names Rows/Columns -----------------------------------------
   
   Time.Change[length(tmin:tmax) + 1, ] <- colSums(
                                    Time.Change[1:length(tmin:tmax), ])
   colnames(Time.Change) <- cNames
   rownames(Time.Change) <- c(tmin:tmax, "Totals")

# 4.4 Return Values ------------------------------------------------------------
 
   return(Time.Change)
 }
 
################################################################################
# 5.0 Tally Changes by Process   -----------------------------------------------

 if(CType == "Process"){
  
# 5.1 Set up Variables ---------------------------------------------------------
   
  chngs <- rownames(table(as.character(PwC$Chng.Type)))
  cut <- which(chngs == "A.C." | chngs == "None")
  if(length(cut) > 0){chngs <- chngs[-cut, ]}
  Process.Change <- matrix(ncol=10, nrow=length(chngs) + 1)
  
# 5.2 Calculate Changes  -------------------------------------------------------

   for(Y in 1:length(chngs)){
    YY <- PwC[PwC$Chng.Type == chngs[Y], ]
    Process.Change[Y, 1] <- length(which(YY$Loss.Units != 0))
    Process.Change[Y, 2] <- -sum(YY$Loss.Units)
    Process.Change[Y, 3] <- length(which(YY$Gain.Units != 0))
    Process.Change[Y, 4] <- sum(YY$Gain.Units)
    Process.Change[Y, 5] <- sum(Process.Change[Y, c(2, 4)])
    Process.Change[Y, 6] <- length(which(YY$Loss.SF[YY$B.Class == "C"] != 0))
    Process.Change[Y, 7] <- -sum(YY$Loss.SF[YY$B.Class == "C"])
    Process.Change[Y, 8] <- length(which(YY$Gain.SF[YY$E.Class == "C"] != 0)) 
    Process.Change[Y, 9] <- sum(YY$Gain.SF[YY$E.Class == "C"])
    Process.Change[Y, 10] <- sum(Process.Change[Y, c(7, 9)])
   }
  
# 5.3 Summarize and Name Rows/Columns  -----------------------------------------

  Process.Change[length(chngs) + 1, ] <- colSums(
                                           Process.Change[1:length(chngs), ])
  colnames(Process.Change) <- cNames
  rownames(Process.Change) <- c(chngs, "Totals")
  
# 5.4 Return Values ------------------------------------------------------------

  return(Process.Change)
 }

################################################################################
# 6.0 Tally changes by Land Use ------------------------------------------------

if(CType == "Use"){

# 6.1 Add Land Use Categories to Data  -----------------------------------------
  
  ludbc <- odbcConnectAccess2007(paste0(
    "D://Data//WA//King//Assessor//Codes.accdb"))
  lu.codes <- sqlQuery(ludbc, "SELECT * FROM LandUseCodes")
   PwC <- merge(PwC, lu.codes[ ,c("Code","Type")],
                by.x="B.Use", by.y = "Code", all.x=T)
   colnames(PwC)[dim(PwC)[2]] <- "B.LUType"
   PwC <- merge(PwC, lu.codes[ ,c("Code","Type")],
                by.x="E.Use", by.y = "Code", all.x=T)
   colnames(PwC)[dim(PwC)[2]] <- "E.LUType"
  
# 6.2 Set up Variables   -------------------------------------------------------
   
   LT <- table(c(as.character(PwC$B.LUType),
                 as.character(PwC$E.LUType)))
   Use.Change <- matrix(ncol=10, nrow=length(LT) + 1)
   
# 6.3 Calculate Changes --------------------------------------------------------
   
   for(Y in 1:length(LT)){
     YB <- PwC[PwC$B.LUType == rownames(LT)[Y], ]
     YE <- PwC[PwC$E.LUType == rownames(LT)[Y], ]
     Use.Change[Y, 1] <- length(which(YB$Loss.Units != 0))
     Use.Change[Y, 2] <- -sum(YB$Loss.Units)
     Use.Change[Y, 3] <- length(which(YE$Gain.Units != 0))   
     Use.Change[Y, 4] <- sum(YE$Gain.Units)
     Use.Change[Y, 5] <- sum(Use.Change[Y ,c(2, 4)])
     Use.Change[Y, 6] <- length(which(YB$Loss.SF[YB$B.Class == "C"] != 0))
     Use.Change[Y, 7] <- -sum(YB$Loss.SF[YB$B.Class == "C"])
     Use.Change[Y, 8] <- length(which(YE$Gain.SF[YE$E.Class == "C"] != 0))   
     Use.Change[Y, 9] <- sum(YE$Gain.SF[YE$E.Class == "C"])
     Use.Change[Y, 10] <- sum(Use.Change[Y, c(7, 9)])
   } 

# 6.4 Summarize and Name Rows/Columns ------------------------------------------ 

   Use.Change[(length(LT) + 1), ] <- colSums(Use.Change[1:length(LT), ])
   colnames(Use.Change) <- cNames
   rownames(Use.Change) <- c(rownames(LT), "Totals")

# 6.5 Cut Missing Land Uses ----------------------------------------------------

   rs <- rowSums(Use.Change)
   Use.Change <- Use.Change[-which(rs == 0), ]
   
# 6.6 Return Values ------------------------------------------------------------
   
   return(Use.Change)
}

################################################################################
# 7.0 Tally changes by Size of Development -------------------------------------
   
if(CType == "Size"){

# 7.1 Changes in Units
  
  # 7.1.1 Set up capture variables
  schngs <- c(1, par[[1]], max(c(PwC$Gain.Units, PwC$Loss.Units)) + 1)
  Size.Change.U <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  # 7.1.2 Calculate Changes
  for(Y in 2:length(schngs)){
    yl <- length(which(PwC$Loss.Units >= schngs[Y - 1] &
                         PwC$Loss.Units < schngs[Y]))
    YL <- PwC[PwC$Loss.Units >= schngs[Y - 1]  & PwC$Loss.Units < schngs[Y], ]
    
    yg <- length(which(PwC$Gain.Units >= schngs[Y - 1] &
                         PwC$Gain.Units < schngs[Y]))
    YG <- PwC[PwC$Gain.Units >= schngs[Y - 1]  & PwC$Gain.Units < schngs[Y], ]
    
    Size.Change.U[Y - 1, 1] <- yl
    Size.Change.U[Y - 1, 2] <- -sum(YL$Loss.Units)
    Size.Change.U[Y - 1, 3] <- yg
    Size.Change.U[Y - 1, 4] <- sum(YG$Gain.Units)
    Size.Change.U[Y - 1, 5] <- Size.Change.U[Y - 1, 2] + Size.Change.U[Y - 1, 4]
  }
  
  # 7.1.3 Summarize 
  Size.Change.U[length(schngs), ] <- colSums(
                                     Size.Change.U[1:(length(schngs) - 1), ])
  
  # 7.1.4 Add Row and Column Names
  colnames(Size.Change.U) <- cNames[1:5]
  
  for(i in 2:length(schngs)){
    rownames(Size.Change.U)[i - 1] <- paste0(schngs[i - 1],
                                             " to ", (schngs[i] - 1))  
  }
  rownames(Size.Change.U)[length(schngs)] <- "Totals"  
  
# 7.2 Changes in SF ------------------------------------------------------------ 
  
  # 7.2.1 Set up capture variables
  schngs <- c(1, par[[2]], max(c(PwC$Gain.SF, PwC$Loss.SF)) + 1)
  Size.Change.S <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  # 7.2.2 Calculate Changes
  for(Y in 2:length(schngs)){
    yl <- length(which(PwC$Loss.SF >= schngs[Y - 1] &
                         PwC$Loss.SF < schngs[Y]))
    YL <- PwC[PwC$Loss.SF >= schngs[Y - 1]  & PwC$Loss.SF < schngs[Y], ]
    yg <- length(which(PwC$Gain.SF >= schngs[Y - 1] &
                         PwC$Gain.SF < schngs[Y]))
    YG <- PwC[PwC$Gain.SF >= schngs[Y - 1]  & PwC$Gain.SF < schngs[Y], ]
    Size.Change.S[Y - 1, 1] <- yl
    Size.Change.S[Y - 1, 2] <- -sum(YL$Loss.SF)
    Size.Change.S[Y - 1, 3] <- yg
    Size.Change.S[Y - 1, 4] <- sum(YG$Gain.SF)
    Size.Change.S[Y - 1, 5] <- Size.Change.S[Y - 1, 2] + Size.Change.S[Y - 1, 4]
  }
  
  # 7.2.3 Summarize
  Size.Change.S[length(schngs), ] <- colSums(
                                       Size.Change.S[1:(length(schngs) - 1), ])
  
  # 7.2.4 Add Row and Column Names
  colnames(Size.Change.S) <- cNames[6:10]
  for(i in 2:length(schngs)){
    rownames(Size.Change.S)[i-1] <- paste0(schngs[i - 1],
                                           " to ", (schngs[i] - 1))  
  }
  rownames(Size.Change.S)[length(schngs)] <- "Totals"

# 7.3 Return -------------------------------------------------------------------
  
  return(list(Size.Change.U,Size.Change.S))
}

################################################################################
# 8.0 Use by Year --------------------------------------------------------------

if(CType == "Use.Time"){
 
# 8.1 Prepare the Yearly Variables  --------------------------------------------
  
  tmin <- min(PwC$Chng.Time[PwC$Chng.Time != 0])
  tmax <- max(PwC$Chng.Time)
  
# 8.2 Prepare the Use Based Variables ------------------------------------------  

  ludbc <- odbcConnectAccess2007(paste0(
  "D://Data//WA//King//Assessor//Codes.accdb"))
  lu.codes <- sqlQuery(ludbc, "SELECT * FROM LandUseCodes")
  
  PwC <- merge(PwC, lu.codes[,c("Code", "Type")],
               by.x="B.Use", by.y = "Code", all.x=T)
  colnames(PwC)[dim(PwC)[2]] <- "B.LUType"
  PwC <- merge(PwC, lu.codes[ ,c("Code", "Type")],
               by.x="E.Use", by.y = "Code", all.x=T)
  colnames(PwC)[dim(PwC)[2]] <- "E.LUType"
  
  LT <- table(c(as.character(PwC$B.LUType),
                as.character(PwC$E.LUType)))
  Use.Change <- matrix(ncol=10, nrow=length(LT) + 1)
  
# 8.3 Set up capture Variable   ------------------------------------------------
 
  UT.Change <- NULL
    
# 8.4 Loop through years  ------------------------------------------------------

  for(Z in tmin:tmax){
    
    # 8.4.1 Set to Current Year Data
    YY <- PwC[PwC$Chng.Time == Z, ]

    # 8.4.2 Set up Capture Variable
    Use.Change <- matrix(ncol=10, nrow=length(LT) + 1)
    
    # 8.4.3 Calculate Changes
    for(Y in 1:length(LT)){
      YB <- YY[YY$B.LUType == rownames(LT)[Y], ]
      YE <- YY[YY$E.LUType == rownames(LT)[Y], ]
      Use.Change[Y, 1] <- length(which(YB$Loss.Units != 0))
      Use.Change[Y, 2] <- -sum(YB$Loss.Units)
      Use.Change[Y, 3] <- length(which(YE$Gain.Units != 0))   
      Use.Change[Y, 4] <- sum(YE$Gain.Units)
      Use.Change[Y, 5] <- sum(Use.Change[Y, c(2, 4)])
      Use.Change[Y, 6] <- length(which(YB$Loss.SF[YB$B.Class == "C"] != 0))
      Use.Change[Y, 7] <- -sum(YB$Loss.SF[YB$B.Class == "C"])
      Use.Change[Y, 8] <- length(which(YE$Gain.SF[YE$E.Class == "C"] != 0))   
      Use.Change[Y, 9] <- sum(YE$Gain.SF[YE$E.Class == "C"])
      Use.Change[Y, 10] <- sum(Use.Change[Y, c(7, 9)])
    } 
    
    # 8.4.5 Summarize and Name Rows/Columns  
    Use.Change[(length(LT) + 1),] <- colSums(Use.Change[1:length(LT), ])
    colnames(Use.Change) <- cNames
    rownames(Use.Change) <- c(rownames(LT),"Totals")

# 8.5 Add to the list  ---------------------------------------------------------

    UT.Change[[Z - 1999]] <- Use.Change 
  }  
    
# 8.6 Return -------------------------------------------------------------------
  return(UT.Change)
}

################################################################################
# 9.0 ChangeType by Year -------------------------------------------------------

if(CType == "Process.Time"){
  
# 9.1 Prepare the Yearly Variables ---------------------------------------------  
  
  tmin <- min(PwC$Chng.Time[PwC$Chng.Time != 0])
  tmax <- max(PwC$Chng.Time)
  
# 9.2 Prepare Change Type Variables --------------------------------------------
  
  chngs <- rownames(table(as.character(PwC$Chng.Type)))
  cut <- which(chngs == "A.C." | chngs == "None")
  if(length(cut) > 0){chngs <- chngs[-cut, ]}
  
# 9.3 Set up capture Variable  -------------------------------------------------
  
  PT.Change <- NULL
  
# 9.4 Loop through years  ------------------------------------------------------

  for(Z in tmin:tmax){
    
    # 9.4.1 Set to Current Year Data
    ZZ <- PwC[PwC$Chng.Time == Z, ]
  
    # 9.4.2 Set up Capture Variable
    Process.Change <- matrix(ncol=10, nrow=length(chngs) + 1)
  
    # 9.4.3 calc changes
    for(Y in 1:length(chngs)){
      YY <- ZZ[ZZ$Chng.Type == chngs[Y], ]
      Process.Change[Y, 1] <- length(which(YY$Loss.Units != 0))
      Process.Change[Y, 2] <- -sum(YY$Loss.Units)
      Process.Change[Y, 3] <- length(which(YY$Gain.Units != 0))
      Process.Change[Y, 4] <- sum(YY$Gain.Units)
      Process.Change[Y, 5] <- sum(Process.Change[Y, c(2, 4)])
      Process.Change[Y, 6] <- length(which(YY$Loss.SF[YY$B.Class == "C"] != 0))
      Process.Change[Y, 7] <- -sum(YY$Loss.SF[YY$B.Class == "C"])
      Process.Change[Y, 8] <- length(which(YY$Gain.SF[YY$E.Class == "C"] != 0)) 
      Process.Change[Y, 9] <- sum(YY$Gain.SF[YY$E.Class == "C"])
      Process.Change[Y, 10] <- sum(Process.Change[Y, c(7, 9)])
    }
    
    # 9.4.4 Summarize and Add Row Names    
    Process.Change[length(chngs) + 1, ] <- colSums(
                                            Process.Change[1:length(chngs), ])
    colnames(Process.Change) <- cNames
    rownames(Process.Change) <- c(chngs, "Totals")
    
# 9.5 Add to List  -------------------------------------------------------------

    PT.Change[[Z - 1999]] <- Process.Change 
  }

# 9.6 Return -------------------------------------------------------------------

  return(PT.Change)
}  

################################################################################
# 10.0 Size by Year ------------------------------------------------------------

if(CType == "Size.Time"){
  
# 10.1 Prepare the Yearly Variables  -------------------------------------------
  
  tmin <- min(PwC$Chng.Time[PwC$Chng.Time != 0])
  tmax <- max(PwC$Chng.Time)
  
# 10.2 Work on Change in Units -------------------------------------------------
  
  # 10.2.1 Prepare Size Type Variables
  schngs <- c(1, par[[1]], max(c(PwC$Gain.Units, PwC$Loss.Units)) + 1)
  Size.Change.U <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  # 10.2.2 Prepare Capture Variable
  ST.Change.U <- NULL
  
  # 10.2.3 Loop through years  
  for(Z in tmin:tmax){
    
    # 10.2.4 Set to Current Year Data
    YY <- PwC[PwC$Chng.Time == Z, ]
    
    # 10.2.5 Calculate Changes
    for(Y in 2:length(schngs)){
      yl <- length(which(YY$Loss.Units >= schngs[Y - 1] &
                         YY$Loss.Units < schngs[Y]))
      YL <- YY[YY$Loss.Units >= schngs[Y - 1]  & YY$Loss.Units < schngs[Y], ]
    
      yg <- length(which(YY$Gain.Units >= schngs[Y - 1] &
                         YY$Gain.Units < schngs[Y]))
      YG <- YY[YY$Gain.Units >= schngs[Y - 1]  & YY$Gain.Units < schngs[Y], ]
    
      Size.Change.U[Y - 1, 1] <- yl
      Size.Change.U[Y - 1, 2] <- -sum(YL$Loss.Units)
      Size.Change.U[Y - 1, 3] <- yg
      Size.Change.U[Y - 1, 4] <- sum(YG$Gain.Units)
      Size.Change.U[Y - 1, 5] <- Size.Change.U[Y - 1, 2] + Size.Change.U[
                                                                    Y - 1, 4]
    }
    
  # 10.2.6 Summarize 
    Size.Change.U[length(schngs), ] <- colSums(Size.Change.U[1:(
                                                         length(schngs) - 1), ])
  
  # 10.2.7 Add Row and Column Names
 
  colnames(Size.Change.U) <- cNames[1:5]  
  for(i in 2:length(schngs)){
    rownames(Size.Change.U)[i - 1] <- paste0(schngs[i - 1],
                                            " to ", (schngs[i] - 1))  
  }
  rownames(Size.Change.U)[length(schngs)] <- "Totals"  

  # 10.2.8 Add to List  
    ST.Change.U[[Z - 1999]] <- Size.Change.U 
  }

# 10.3 Work on Change in Units -------------------------------------------------
  
 # 10.3.1 Prepare Size Type Variables
  schngs <- c(1,par[[2]], max(c(PwC$Gain.SF, PwC$Loss.SF)) + 1)
  Size.Change.S <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  # 10.3.2 Prepare Capture Variable
  ST.Change.S <- NULL
  
  # 10.3.3 Loop through years  
  for(Z in tmin:tmax){
    
    # 10.3.4 Set to Current Year Data
    YY <- PwC[PwC$Chng.Time == Z,]
    
    for(Y in 2:length(schngs)){
      yl <- length(which(YY$Loss.SF >= schngs[Y - 1] &
                           YY$Loss.SF < schngs[Y]))
      YL <- YY[YY$Loss.SF >= schngs[Y - 1]  & YY$Loss.SF < schngs[Y], ]
      
      yg <- length(which(YY$Gain.SF >= schngs[Y - 1] &
                           YY$Gain.SF < schngs[Y]))
      YG <- YY[YY$Gain.SF >= schngs[Y - 1]  & YY$Gain.SF < schngs[Y], ]
      
      Size.Change.S[Y - 1, 1] <- yl
      Size.Change.S[Y - 1, 2] <- -sum(YL$Loss.SF)
      Size.Change.S[Y - 1, 3] <- yg
      Size.Change.S[Y - 1, 4] <- sum(YG$Gain.SF)
      Size.Change.S[Y - 1, 5] <- Size.Change.S[Y - 1, 2] + Size.Change.S[
                                                                       Y - 1, 4]
    }
    
    # 10.3.5 Summarize 
    Size.Change.S[length(schngs), ] <- colSums(Size.Change.S[1:(
                                                        length(schngs) - 1), ])
    
    # 10.3.6 Add Row and Column Names
    colnames(Size.Change.S) <- cNames[6:10]
    
    for(i in 2:length(schngs)){
      rownames(Size.Change.S)[i - 1] <- paste0(schngs[i - 1],
                                               " to ", (schngs[i] - 1))  
    }
    rownames(Size.Change.S)[length(schngs)] <- "Totals"  
    
    # 10.3.7 Add to List  
    ST.Change.S[[Z - 1999]] <- Size.Change.S 
  }
  
# 10.4 Return Values -----------------------------------------------------------  

 return(list(ST.Change.U, ST.Change.S, par))
}  

################################################################################
# 11.0 Size by Process Type ----------------------------------------------------

if(CType == "Size.Process"){
  
# 11.1 set up change types -----------------------------------------------------
  
  chngs <- rownames(table(as.character(PwC$Chng.Type)))
  cut <- which(chngs == "A.C." | chngs == "None")
  if(length(cut) > 0){chngs <- chngs[-cut, ]}
  
# 11.2 Work on Change in Units -------------------------------------------------
  
  # 11.2.1 Prepare Size Type Variables
  schngs <- c(1, par[[1]], max(c(PwC$Gain.Units, PwC$Loss.Units)) + 1)
  Size.Change.U <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  # 11.2.2 Prepare Capture Variable
  SP.Change.U <- NULL
    
  # 11.2.3 Calculate Changes
  for(Y in 1:length(chngs)){
    YY <- PwC[PwC$Chng.Type == chngs[Y], ]
  
    for(Z in 2:length(schngs)){
      yl <- length(which(YY$Loss.Units >= schngs[Z - 1] &
                           YY$Loss.Units < schngs[Z]))
      YL <- YY[YY$Loss.Units >= schngs[Z - 1]  & YY$Loss.Units < schngs[Z], ]
      
      yg <- length(which(YY$Gain.Units >= schngs[Z - 1] &
                           YY$Gain.Units < schngs[Z]))
      YG <- YY[YY$Gain.Units >= schngs[Z - 1]  & YY$Gain.Units < schngs[Z], ]
      
      Size.Change.U[Z - 1, 1] <- yl
      Size.Change.U[Z - 1, 2] <- -sum(YL$Loss.Units)
      Size.Change.U[Z - 1, 3] <- yg
      Size.Change.U[Z - 1, 4] <- sum(YG$Gain.Units)
      Size.Change.U[Z - 1, 5] <- Size.Change.U[Z - 1, 2] + Size.Change.U[
                                                                       Z - 1, 4]
    }
  
  # 11.2.4 Summarize 
    Size.Change.U[length(schngs), ] <- colSums(Size.Change.U[1:(
                                                        length(schngs) - 1), ])
  
  # 11.2.5 Add Row and Column Names
  colnames(Size.Change.U) <- cNames[1:5]
    
  for(i in 2:length(schngs)){
    rownames(Size.Change.U)[i - 1] <- paste0(schngs[i - 1],
                                             " to ", (schngs[i]-1))  
  }
  rownames(Size.Change.U)[length(schngs)] <- "Totals"  
  
  # 11.2.6 Add to List  
  SP.Change.U[[Y]] <- Size.Change.U 
  } 
    
# 11.3 Work on Change in SF ----------------------------------------------------
  
  # 11.3.1 Prepare Size Type Variables
  schngs <- c(1, par[[2]], max(c(PwC$Gain.SF, PwC$Loss.SF)) + 1)
  Size.Change.S <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  # 11.3.2 Prepare Capture Variable
  SP.Change.S <- NULL
  
  # 11.3.3 Calculate Changes
  for(Y in 1:length(chngs)){
    YY <- PwC[PwC$Chng.Type == chngs[Y], ]
    
    for(Z in 2:length(schngs)){
      yl <- length(which(YY$Loss.SF >= schngs[Z - 1] &
                           YY$Loss.SF < schngs[Z]))
      YL <- YY[YY$Loss.SF >= schngs[Z - 1]  & YY$Loss.SF < schngs[Z], ]
      
      yg <- length(which(YY$Gain.SF >= schngs[Z - 1] &
                           YY$Gain.SF < schngs[Z]))
      YG <- YY[YY$Gain.SF >= schngs[Z - 1]  & YY$Gain.SF < schngs[Z], ]
      
      Size.Change.S[Z - 1, 1] <- yl
      Size.Change.S[Z - 1, 2] <- -sum(YL$Loss.SF)
      Size.Change.S[Z - 1, 3] <- yg
      Size.Change.S[Z - 1, 4] <- sum(YG$Gain.SF)
      Size.Change.S[Z - 1, 5] <- Size.Change.S[Z - 1, 2] + Size.Change.S[
                                                                     Z - 1, 4]
    }
    
    # 11.3.4 Summarize 
    Size.Change.S[length(schngs), ] <- colSums(Size.Change.S[1:(
                                    length(schngs) - 1), ])
    
    # 11.3.5 Add Row and Column Names
    colnames(Size.Change.S) <- cNames[6:10]
    
    for(i in 2:length(schngs)){
      rownames(Size.Change.S)[i - 1] <- paste0(schngs[i - 1],
                                               " to ", (schngs[i] - 1))  
    }
    rownames(Size.Change.S)[length(schngs)] <- "Totals"  
    
    # 11.3.6 Add to List  
    SP.Change.S[[Y]] <- Size.Change.S 
  }    
    
# 11.4 Return Values -----------------------------------------------------------

return(list(SP.Change.U, SP.Change.S, chngs))
}

################################################################################
# 12.0 Use by Process Type -----------------------------------------------------

if(CType == "Use.Process"){
  
# 12.1 set up change types -----------------------------------------------------
  
  chngs <- rownames(table(as.character(PwC$Chng.Type)))
  cut <- which(chngs == "A.C." | chngs == "None")
  if(length(cut) > 0){chngs <- chngs[-cut, ]}
  
# 12.2 Prepare the Use Based Variables -----------------------------------------  

  ludbc <- odbcConnectAccess2007(paste0(
    "D://Data//WA//King//Assessor//Codes.accdb"))
  lu.codes <- sqlQuery(ludbc, "SELECT * FROM LandUseCodes")
  
  PwC <- merge(PwC, lu.codes[,c("Code", "Type")],
               by.x="B.Use", by.y = "Code", all.x=T)
  colnames(PwC)[dim(PwC)[2]] <- "B.LUType"
  PwC <- merge(PwC, lu.codes[,c("Code", "Type")],
               by.x="E.Use", by.y = "Code", all.x=T)
  colnames(PwC)[dim(PwC)[2]] <- "E.LUType"
  
# 12.3 Set up Variables --------------------------------------------------------

  LT <- table(c(as.character(PwC$B.LUType),
                as.character(PwC$E.LUType)))
  UP.Change <- NULL
  
  # 12.4 Calculate Changes
  for(Z in 1:length(chngs)){
    
    # 12.4.1 Set to Current Year Data
    YY <- PwC[PwC$Chng.Type == chngs[Z], ]

    Use.Change <- matrix(ncol=10, nrow=length(LT) + 1)

    # 12.4.2 Calculate Changes
    for(Y in 1:length(LT)){
      YB <- PwC[YY$B.LUType == rownames(LT)[Y], ]
      YE <- PwC[YY$E.LUType == rownames(LT)[Y], ]
      Use.Change[Y, 1] <- length(which(YB$Loss.Units != 0))
      Use.Change[Y, 2] <- -sum(YB$Loss.Units)
      Use.Change[Y, 3] <- length(which(YE$Gain.Units != 0))   
      Use.Change[Y, 4] <- sum(YE$Gain.Units)
      Use.Change[Y, 5] <- sum(Use.Change[Y, c(2, 4)])
      Use.Change[Y, 6] <- length(which(YB$Loss.SF[YB$B.Class == "C"] != 0))
      Use.Change[Y, 7] <- -sum(YB$Loss.SF[YB$B.Class == "C"])
      Use.Change[Y, 8] <- length(which(YE$Gain.SF[YE$E.Class == "C"] != 0))   
      Use.Change[Y, 9] <- sum(YE$Gain.SF[YE$E.Class == "C"])
      Use.Change[Y, 10] <- sum(Use.Change[Y, c(7, 9)])
    } 
    
    # 12.4.3 Summarize and Name Rows/Columns  
    Use.Change[(length(LT) + 1), ] <- colSums(Use.Change[1:length(LT), ])
    colnames(Use.Change) <- cNames
    rownames(Use.Change) <- c(rownames(LT), "Totals")
    
# 12.5 Add to the list  --------------------------------------------------------
    
    UP.Change[[Z]] <- Use.Change 
  }
  
# 12.6 Return Values -----------------------------------------------------------

return(list(UP.Change, chngs))
}

################################################################################
# 13.0 Size by Use -------------------------------------------------------------

if(CType == "Size.Use"){

# 13.1 Prepare the Use Based Variables -----------------------------------------  
  
  ludbc <- odbcConnectAccess2007(paste0(
    "D://Data//WA//King//Assessor//Codes.accdb"))
  lu.codes <- sqlQuery(ludbc, "SELECT * FROM LandUseCodes")
  
  PwC <- merge(PwC, lu.codes[ ,c("Code","Type")],
               by.x="B.Use", by.y = "Code", all.x=T)
  colnames(PwC)[dim(PwC)[2]] <- "B.LUType"
  PwC <- merge(PwC, lu.codes[ ,c("Code","Type")],
               by.x="E.Use", by.y = "Code", all.x=T)
  colnames(PwC)[dim(PwC)[2]] <- "E.LUType"
  
  LT <- table(c(as.character(PwC$B.LUType),
                as.character(PwC$E.LUType)))
  SU.Change.U <- NULL
  SU.Change.S <- NULL
    
# 13.2.Do Changes in Units -----------------------------------------------------
  
  # 13.2.1 Set up capture variables
  schngs <- c(1, par[[1]], max(c(PwC$Gain.Units, PwC$Loss.Units)) + 1)
  Size.Change.U <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  for(Y in 1:length(LT)){
    YB <- PwC[PwC$B.LUType == rownames(LT)[Y], ]
    YE <- PwC[PwC$E.LUType == rownames(LT)[Y], ]

    # 13.2.2 Calculate Changes
    for(Z in 2:length(schngs)){
      yl <- length(which(YB$Loss.Units >= schngs[Z - 1] &
                         YB$Loss.Units < schngs[Z]))
      YL <- YB[YB$Loss.Units >= schngs[Z - 1]  & YB$Loss.Units < schngs[Z], ]
    
      yg <- length(which(YE$Gain.Units >= schngs[Z - 1] &
                         YE$Gain.Units < schngs[Z]))
      YG <- YE[YE$Gain.Units >= schngs[Z - 1]  & YE$Gain.Units < schngs[Z], ]
    
      Size.Change.U[Z - 1, 1] <- yl
      Size.Change.U[Z - 1, 2] <- -sum(YL$Loss.Units)
      Size.Change.U[Z - 1, 3] <- yg
      Size.Change.U[Z - 1, 4] <- sum(YG$Gain.Units)
      Size.Change.U[Z - 1, 5] <- Size.Change.U[Z - 1, 2] + Size.Change.U[
                                                                       Z - 1, 4]
    }
    
   # 13.2.3 Summarize 
    Size.Change.U[length(schngs), ] <- colSums(Size.Change.U[1:(
                                                        length(schngs) - 1), ])
  
   # 13.2.4 Add Row and Column Names
   colnames(Size.Change.U) <- cNames[1:5]
   for(i in 2:length(schngs)){
     rownames(Size.Change.U)[i - 1] <- paste0(schngs[i - 1],
                                              " to ", (schngs[i] - 1))  
   }
   rownames(Size.Change.U)[length(schngs)] <- "Totals"  
   
  #13.2.5 Add to List  
  SU.Change.U[[Y]] <- Size.Change.U
  }
  
# 13.3.Do Changes in Units -----------------------------------------------------
  
  # 13.2.1 Set up capture variables
  schngs <- c(1, par[[2]], max(c(PwC$Gain.SF,PwC$Loss.SF)) + 1)
  Size.Change.S <- as.data.frame(matrix(ncol=5, nrow=length(schngs)))
  
  for(Y in 1:length(LT)){
    YY <- PwC[PwC$B.LUType == rownames(LT)[Y] | 
                PwC$E.LUType == rownames(LT)[Y], ]
    
    # 13.2.2 Calculate Changes
    for(Z in 2:length(schngs)){
      yl <- length(which(YY$Loss.SF >= schngs[Z - 1] &
                           YY$Loss.SF < schngs[Z]))
      YL <- YY[YY$SF.Units >= schngs[Z - 1]  & YY$Loss.SF < schngs[Z], ]
      
      yg <- length(which(YY$Gain.SF >= schngs[Z - 1] &
                           YY$Gain.SF < schngs[Z]))
      YG <- YY[YY$Gain.SF >= schngs[Z - 1]  & YY$Gain.SF < schngs[Z], ]
      
      Size.Change.S[Z - 1, 1] <- yl
      Size.Change.S[Z - 1, 2] <- -sum(YL$Loss.SF)
      Size.Change.S[Z - 1, 3] <- yg
      Size.Change.S[Z - 1, 4] <- sum(YG$Gain.SF)
      Size.Change.S[Z - 1, 5] <- Size.Change.S[Z - 1, 2] + Size.Change.S[
                                                                    Z - 1, 4]
    }
    
    # 13.2.3 Summarize 
    Size.Change.S[length(schngs), ] <- colSums(Size.Change.S[1:(
                                                       length(schngs) - 1), ])
    
    # 13.2.4 Add Row and Column Names
    colnames(Size.Change.S) <- cNames[6:10]
    
    for(i in 2:length(schngs)){
      rownames(Size.Change.S)[i - 1] <- paste0(schngs[i - 1],
                                               " to ", (schngs[i] - 1))  
    }
    rownames(Size.Change.S)[length(schngs)] <- "Totals"  
    
    # 13.3.5 Add to List    
   SU.Change.S[[Y]] <- Size.Change.S
  }

# 13.4 Return ------------------------------------------------------------------

 return(list(SU.Change.U, SU.Change.S,rownames(LT)))
 }

} # Ends Function




