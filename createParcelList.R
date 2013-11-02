'###############################################################################
#                                                                              #
#  createParcelList.R                                                          #
#  - Most Recent Update:  4/6/2013                                            #
#                                                                              #
#  This file determine the parcel history of each parcel from both the         #
#  beginning and the end year files as indicated by the user                   #
#  It is part of the data cleaning process                                     #
#                                                                              #
###############################################################################'

createParcelList <- function(Beg.Year, End.Year){

  # 0.0 Set Global Parameters, Load Libraries and Files --------------------------

# 0.1 Load Libraries

library(RODBC)
options(warn=-1)

# 0.2 Load Helper Files

source("c://Dropbox//Code//WA//KingCounty//K_CodingFunctions.R")

################################################################################
# 1.0 Prepare Residential Data   -----------------------------------------------

# 1.1 Load Beginning Year Residential Data  
  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Beg.Year, ".accdb"))
  B.resbldg <- sqlQuery(odbc, paste0("SELECT Major, Minor, SqFtTotLiving as SF,",
                                     "YrBuilt as YearBuilt, Condition,",
                                     "BldgNbr, NbrLivingUnits, Stories",
                                     " FROM ResBldg", Beg.Year))
  B.resbldg <- pinCreate(B.resbldg)
  odbcClose(odbc)
  
# 1.2 Load End Year Residential Data  
  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", End.Year, ".accdb"))
  E.resbldg <- sqlQuery(odbc, paste0("SELECT Major, Minor, SqFtTotLiving as SF,",
                                     " YrBuilt as YearBuilt, Condition,",
                                     " BldgNbr, NbrLivingUnits, Stories",
                                     " FROM ResBldg", End.Year))
  E.resbldg <- pinCreate(E.resbldg)
  odbcClose(odbc)
  
# 1.3 Sum up Number of Buildings
  
  # 1.3.1 Beginning Year
  x <- as.data.frame(table(B.resbldg$PINX))
  colnames(x) <- c("PINX", "NbrBldgs")
  brc <- merge(B.resbldg, x, by.x="PINX", by.y="PINX", all.x=T)
  
  # 1.3.2 End Year
  x <- as.data.frame(table(E.resbldg$PINX))
  colnames(x) <- c("PINX", "NbrBldgs")
  erc <- merge(E.resbldg, x, by.x="PINX", by.y="PINX", all.x=T)
  
# 1.4 Sum up Additional Dwelling Unit Living Units
  
  # 1.4.1 Beginning Year
  x <- B.resbldg[B.resbldg$BldgNbr!=1,]
  xu <- as.data.frame(tapply(x$NbrLivingUnits, x$PINX, sum))
  colnames(xu) <- "A.NbrUnits"
  xu$PINX <- rownames(xu)
  brc <- merge(brc, xu, by.x="PINX", by.y="PINX", all.x=T)
  brc$A.NbrUnits[is.na(brc$A.NbrUnits)] <- 0
  
  # 1.4.2 End Year
  x <- E.resbldg[E.resbldg$BldgNbr!=1,]
  xu <- as.data.frame(tapply(x$NbrLivingUnits, x$PINX, sum))
  colnames(xu) <- "A.NbrUnits"
  xu$PINX <- rownames(xu)
  erc <- merge(erc, xu, by.x="PINX", by.y="PINX", all.x=T)
  erc$A.NbrUnits[is.na(erc$A.NbrUnits)] <- 0
  
# 1.5 Sum up Additional Dwelling Unit Square Footage
  
  # 1.5.1 Beginning Year
  x <- B.resbldg[B.resbldg$BldgNbr!=1,]
  xs <- as.data.frame(tapply(x$SF, x$PINX, sum))
  colnames(xs) <- "A.SF"
  xs$PINX <- rownames(xs)
  brc <- merge(brc, xs, by.x="PINX", by.y="PINX", all.x=T)
  brc$A.SF[is.na(brc$A.SF)] <- 0
  
  # 1.5.2 End Year
  x <- E.resbldg[E.resbldg$BldgNbr!=1,]
  xs <- as.data.frame(tapply(x$SF, x$PINX, sum))
  colnames(xs) <- "A.SF"
  xs$PINX <- rownames(xs)
  erc <- merge(erc, xs, by.x="PINX", by.y="PINX", all.x=T)
  erc$A.SF[is.na(erc$A.SF)] <- 0
  
# 1.6 Find most recent Additional Dwelling Unit
  
  # 1.6.1 Beginning Year
  x <- B.resbldg[B.resbldg$BldgNbr!=1,]
  xy <- as.data.frame(tapply(x$YearBuilt, x$PINX, max))
  colnames(xy) <- "A.YB"
  xy$PINX <- rownames(xy)
  brc <- merge(brc, xy, by.x="PINX", by.y="PINX", all.x=T)
  brc$A.YB[is.na(brc$A.YB)] <- 0
  
  # 1.6.2 End Year
  x <- E.resbldg[E.resbldg$BldgNbr!=1,]
  xy <- as.data.frame(tapply(x$YearBuilt, x$PINX, max))
  colnames(xy) <- "A.YB"
  xy$PINX <- rownames(xy)
  erc <- merge(erc, xy, by.x="PINX", by.y="PINX", all.x=T)
  erc$A.YB[is.na(erc$A.YB)] <- 0
  
# 1.7 Trim to one record, rename fields
  
  # 1.7.1 Beginning Year
  Beg.R <- brc[brc$BldgNbr==1,]
  Beg.R$PIN <- NULL
  Beg.R$BldgNbr <- NULL
  colnames(Beg.R)[which(colnames(Beg.R)=="NbrLivingUnits")] <- "NbrUnits"
  Beg.R$Class <- "R"
  Beg.R <- rmDup(Beg.R, "PINX")
  
  # 1.7.2 End Year
  End.R <- erc[erc$BldgNbr==1,]
  End.R$PIN <- NULL
  End.R$BldgNbr <- NULL
  colnames(End.R)[which(colnames(End.R)=="NbrLivingUnits")] <- "NbrUnits"
  End.R$Class <- "R"
  End.R <- rmDup(End.R, "PINX")
  
# 1.99 Clean up
  rm(brc); rm(erc); rm(x); rm(xu); rm(xs)
  
################################################################################  
# 2.0 Prepare Apartment Data  -------------------------------------------------- 
  
# 2.1 Load Beginning Year Data
  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Beg.Year, ".accdb"))
  B.aptcomp <- sqlQuery(odbc, paste0("SELECT Major, Minor, NbrUnits, AvgUnitSize,",
                                     "Condition, YrBuilt as YearBuilt,",
                                     "NbrBldgs, NbrStories FROM AptComp", Beg.Year))
  B.aptcomp <- pinCreate(B.aptcomp)
  odbcClose(odbc)
  
# 2.2 Load End Year Data
  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", End.Year, ".accdb"))
  E.aptcomp <- sqlQuery(odbc, paste0("SELECT Major, Minor, NbrUnits, AvgUnitSize,",
                                     " Condition, YrBuilt as YearBuilt,",
                                     " NbrBldgs, NbrStories FROM AptComp", End.Year))
  E.aptcomp <- pinCreate(E.aptcomp)
  odbcClose(odbc)
  
# 2.3 Calculate Square Footage
  
  # 2.3.1 Beginning Year
  Beg.A <- B.aptcomp
  Beg.A$SF <- Beg.A$NbrUnits * Beg.A$AvgUnitSize
  Beg.A$AvgUnitSize <- NULL

  # 2.3.2 End Year
  End.A <- E.aptcomp
  End.A$SF <- End.A$NbrUnits * End.A$AvgUnitSize
  End.A$AvgUnitSize <- NULL
  
# 2.4 Rename Columns and add blank variables

 # 2.4.1 Beginning Year
  colnames(Beg.A)[which(colnames(Beg.A)=="NbrStories")] <- "Stories"
  colnames(Beg.A)[which(colnames(Beg.A)=="NbrLivingUnits")] <- "NbrUnits"
  Beg.A$A.NbrUnits <- 0
  Beg.A$A.SF <- 0
  Beg.A$A.YB <- 0
  Beg.A$Class <- "A"
  Beg.A$PIN <- NULL
  Beg.A <- rmDup(Beg.A, "PINX")
  
 # 2.4.2 End Year
  colnames(End.A)[which(colnames(End.A)=="NbrStories")] <- "Stories"
  colnames(End.A)[which(colnames(End.A)=="NbrLivingUnits")] <- "NbrUnits"
  End.A$A.NbrUnits <- 0
  End.A$A.SF <- 0
  End.A$A.YB <- 0
  End.A$Class <- "A"
  End.A$PIN <- NULL
  End.A <- rmDup(End.A, "PINX")
  
################################################################################
# 3.0 Prepare Condominium Data -------------------------------------------------  

# 3.1 Load Beginning Year Data  

  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Beg.Year, ".accdb"))
  B.condocomp <- sqlQuery(odbc, paste0("SELECT Major, YrBuilt as YearBuilt,",
                             " Condition, NbrUnits, AvgUnitSize",
                             ", NbrBldgs, NbrStories FROM CondoComp", Beg.Year))
  B.condocomp <- pinCreateCC(B.condocomp)
  odbcClose(odbc)
  
# 3.2 Load End Year Data
  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", End.Year, ".accdb"))

  E.condocomp <- sqlQuery(odbc, paste0("SELECT Major, YrBuilt as YearBuilt,",
                           " Condition, NbrUnits, AvgUnitSize",
                           ", NbrBldgs, NbrStories FROM CondoComp", End.Year))
  E.condocomp <- pinCreateCC(E.condocomp)
  odbcClose(odbc)
  
# 3.3 Calculate Square Footage
  
  # 3.3.1 Beginning Year
  Beg.K <- B.condocomp
  Beg.K$SF <- Beg.K$NbrUnits * Beg.K$AvgUnitSize
  Beg.K$AvgUnitSize <- NULL
  
  # 3.3.2 End Year
  End.K <- E.condocomp
  End.K$SF <- End.K$NbrUnits * End.K$AvgUnitSize
  End.K$AvgUnitSize <- NULL
  
# 3.4 Rename Columns and add blank variables
  
  # 3.4.1 Beginning Year
  colnames(Beg.K)[which(colnames(Beg.K)=="NbrStories")] <- "Stories"
  colnames(Beg.K)[which(colnames(Beg.K)=="NbrLivingUnits")] <- "NbrUnits"
  Beg.K$A.NbrUnits <- 0
  Beg.K$A.SF <- 0
  Beg.K$A.YB <- 0
  Beg.K$Class <- "K"
  Beg.K$PIN <- NULL
  Beg.K <- rmDup(Beg.K, "PINX")
  
  # 3.4.2 End Year
  colnames(End.K)[which(colnames(End.K)=="NbrStories")] <- "Stories"
  colnames(End.K)[which(colnames(End.K)=="NbrLivingUnits")] <- "NbrUnits"
  End.K$A.NbrUnits <- 0
  End.K$A.SF <- 0
  End.K$A.YB <- 0
  End.K$Class <- "K"
  End.K$PIN <- NULL
  End.K <- rmDup(End.K, "PINX")  
  
################################################################################
# 4.0 Prepare Commerical Data  -------------------------------------------------
  
# 4.1 Load Beginning Year Data  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", Beg.Year, ".accdb"))
    
  B.parcel <- sqlQuery(odbc, paste0("SELECT Major, Minor, PresentUse,",
                                "CurrentZoning, SqFtLot FROM Parcel", Beg.Year))
  B.parcel <- pinCreate(B.parcel)
    
  B.commbldg <- sqlQuery(odbc, paste0("SELECT Major, Minor, BldgGrossSqFt as SF,",
                              "BldgQuality, YrBuilt as YearBuilt, BldgNbr",
                              ", NbrStories FROM CommBldg", Beg.Year))
  B.commbldg <- pinCreate(B.commbldg)
  
  B.commsect <- sqlQuery(odbc, paste0("SELECT Major, Minor, GrossSqFt as SF,",
                              " BldgNbr, SectionUse FROM CommSect", Beg.Year))
  B.commsect <- pinCreate(B.commsect)
  odbcClose(odbc)
   
# 4.2 Load End Year Data  
  odbc <- odbcConnectAccess2007(paste0(
    "C://Dropbox//Data//WA//King//Assessor//Annual//King", End.Year, ".accdb"))
  E.parcel <- sqlQuery(odbc, paste0("SELECT Major, Minor, PresentUse,",
                              " CurrentZoning, SqFtLot FROM Parcel", End.Year))
  E.parcel <- pinCreate(E.parcel)
    
  E.commbldg <- sqlQuery(odbc, paste0("SELECT Major, Minor, BldgGrossSqFt as SF,",
                                  " BldgQuality, YrBuilt as YearBuilt, BldgNbr",
                                 ", NbrStories FROM CommBldg", End.Year))
  E.commbldg <- pinCreate(E.commbldg)
  E.commsect <- sqlQuery(odbc, paste0("SELECT Major, Minor, GrossSqFt as SF,",
                             " BldgNbr, SectionUse FROM CommSect", End.Year))
  E.commsect <- pinCreate(E.commsect)
  odbcClose(odbc)
  
# 4.3 Sum up Number of Buildings
  
  # 4.3.1 Beginning Year
  x <- as.data.frame(table(B.commbldg$PINX))
  colnames(x) <- c("PINX", "NbrBldgs")
  bcc <- merge(B.commbldg, x, by.x="PINX", by.y="PINX", all.x=T)
  bcc$SF[is.na(bcc$SF)] <- 0
  
  # 4.3.2 End Year
  x <- as.data.frame(table(E.commbldg$PINX))
  colnames(x) <- c("PINX", "NbrBldgs")
  ecc <- merge(E.commbldg, x, by.x="PINX", by.y="PINX", all.x=T)
  ecc$SF[is.na(ecc$SF)] <- 0
  
# 4.4 Sum up Additional Building Square Footage
  
  # 4.4.1 Beginning Year
  x <- B.commbldg[B.commbldg$BldgNbr!=1,]
  xs <- as.data.frame(tapply(x$SF, x$PINX, sum))
  colnames(xs) <- "A.SF"
  xs$PINX <- rownames(xs)
  bcc <- merge(bcc, xs, by.x="PINX", by.y="PINX", all.x=T)
  bcc$A.SF[is.na(bcc$A.SF)] <- 0
  
  # 4.4.2 End Year
  x <- E.commbldg[E.commbldg$BldgNbr!=1,]
  xs <- as.data.frame(tapply(x$SF, x$PINX, sum))
  colnames(xs) <- "A.SF"
  xs$PINX <- rownames(xs)
  ecc <- merge(ecc, xs, by.x="PINX", by.y="PINX", all.x=T)
  ecc$A.SF[is.na(ecc$A.SF)] <- 0  
  
# 4.5 Find most recent Additional Building
  
  # 4.5.1 Beginning Year
  x <- B.commbldg[B.commbldg$BldgNbr!=1,]
  xy <- as.data.frame(tapply(x$YearBuilt, x$PINX, max))
  colnames(xy) <- "A.YB"
  xy$PINX <- rownames(xy)
  bcc <- merge(bcc, xy, by.x="PINX", by.y="PINX", all.x=T)
  bcc$A.YB[is.na(bcc$A.YB)] <- 0
  
  # 4.5.2 End Year
  x <- E.commbldg[E.commbldg$BldgNbr!=1,]
  xy <- as.data.frame(tapply(x$YearBuilt, x$PINX, max))
  colnames(xy) <- "A.YB"
  xy$PINX <- rownames(xy)
  ecc <- merge(ecc, xy, by.x="PINX", by.y="PINX", all.x=T)
  ecc$A.YB[is.na(ecc$A.YB)] <- 0  
  
# 4.6 Split up by mixed use
  
  # 4.6.1 Beginning Year
    
   # 4.6.1.1 Attach Present Use
    bcc <- merge(bcc, B.parcel[, c("PINX","PresentUse")],
                 by.x="PINX", by.y="PINX", all.x=T)
    bcc$PresentUse[is.na(bcc$PresentUse)] <- 0

   # 4.6.1.2 Separate out Mixed Use Properties
    bmu <- bcc[bcc$PresentUse == 16 | bcc$PresentUse == 25, ]
    
   # 4.6.1.3 Add information on Building Sections
    bmu$PINC <- paste0(bmu$PINX,".",bmu$BldgNbr)
    B.commsect$PINC <- paste0(B.commsect$PINX,".",B.commsect$BldgNbr)
    bmu <- merge(bmu, B.commsect[,c("PINC","BldgNbr","SectionUse","SF")]
                 , by.x="PINC", by.y="PINC", all.x=T)
    bmu$BldgNbr.y[is.na(bmu$BldgNbr.y)] <- 0
    bmu$SectionUse[is.na(bmu$SectionUse)] <- 0
    bmu$SF.y[is.na(bmu$SF.y)] <- 0
  
   # 4.6.1.4 Cull to residential sections 
    bmu$Res <- 0
    bmu$Res[bmu$SectionUse == 300 | bmu$SectionUse == 351 |
              bmu$SectionUse == 352 |bmu$SectionUse == 984] <- 1
    bmu <- bmu[bmu$Res == 1, ]
  
   # 4.6.1.5 Estimate $ of Units
    bmu$Units <- round(bmu$SF.y/800,0)
    bmu$Units[bmu$Units==0] <- 1
  
   # 4.6.1.6 Sum up Res SF in Buildings 1
    bx1 <- as.data.frame(tapply(bmu$SF.y[bmu$BldgNbr.x==1]
                                ,bmu$PINX[bmu$BldgNbr.x==1],sum))
    colnames(bx1) <- "R1.SF"
    bx1$PINX <- rownames(bx1)
  
   # 4.6.1.7 Remove this SF from overall Building 1 SF
    bcx <- merge(bcc, bx1, by.x="PINX", by.y="PINX", all.x=T)
    bcx$R1.SF[is.na(bcx$R1.SF)] <- 0  
    bcx$xSF <- 0
    bcx$xSF[bcx$BldgNbr==1] <- bcx$SF[bcx$BldgNbr==1] - bcx$R1.SF[bcx$BldgNbr==1]
    bcx$SF[bcx$xSF>=0] <- bcx$xSF[bcx$xSF>=0]
    bcx$R1.SF <- NULL
    bcx$xSF <- NULL
  
   # 4.6.1.8 Sum up Res SF in Buildings 2+
    bx2 <- as.data.frame(tapply(bmu$SF.y[bmu$BldgNbr.x!=1]
                              ,bmu$PINX[bmu$BldgNbr.x!=1],sum))
    colnames(bx2) <- "R2.SF"
    bx2$PINX <- rownames(bx2)
  
  # 4.6.1.9 Remove this SF from overall Building 2+ SF
    bcx <- merge(bcx, bx2, by.x="PINX", by.y="PINX", all.x=T)
    bcx$R2.SF[is.na(bcx$R2.SF)] <- 0  
    bcx$xSF <- 0
    bcx$xSF[bcx$BldgNbr==1] <- bcx$A.SF[bcx$BldgNbr==1] - bcx$R2.SF[bcx$BldgNbr==1]
    bcx$A.SF[bcx$xSF>=0] <- bcx$xSF[bcx$xSF>=0]
    bcx$R2.SF <- NULL
    bcx$xSF <- NULL
  
  # 4.6.1.10 Sum up Units in Buildings 1
    bu1 <- as.data.frame(tapply(bmu$Units[bmu$BldgNbr.x==1]
                                ,bmu$PINX[bmu$BldgNbr.x==1],sum))
    colnames(bu1) <- "NbrUnits"
    bu1$PINX <- rownames(bu1)
  
  # 4.6.1.11 Add to NbrUnits
    bcx <- merge(bcx, bu1, by.x="PINX", by.y="PINX", all.x=T)
    bcx$NbrUnits[is.na(bcx$NbrUnits)] <- 0  
    
  # 4.6.1.12 Sum up units in Buildings 2+
    bu2 <- as.data.frame(tapply(bmu$Units[bmu$BldgNbr.x!=1]
                              ,bmu$PINX[bmu$BldgNbr.x!=1],sum))
    colnames(bu2) <- "A.NbrUnits"
    bu2$PINX <- rownames(bu2)
  
  # 4.6.1.13 Add to NbrUnits
    bcx <- merge(bcx, bu2, by.x="PINX", by.y="PINX", all.x=T)
    bcx$A.NbrUnits[is.na(bcx$A.NbrUnits)] <- 0  
  
  # 4.6.1.14 Remove secondary buildings and rename columns
    Beg.C <- bcx[bcx$BldgNbr==1,]
    colnames(Beg.C)[which(colnames(Beg.C)=="BldgQuality")] <- "Condition"
    colnames(Beg.C)[which(colnames(Beg.C)=="NbrStories")] <- "Stories"
    Beg.C$Class <- "C"
    Beg.C$PIN <- NULL
    Beg.C$BldgNbr <- NULL
  
  # 4.6.2 End Year
  
  # 4.6.2.1 Attach Present Use
  ecc <- merge(ecc, E.parcel[, c("PINX","PresentUse")],
               by.x="PINX", by.y="PINX", all.x=T)
  ecc$PresentUse[is.na(ecc$PresentUse)] <- 0
  
  # 4.6.1.2 Separate out Mixed Use Properties
  emu <- ecc[ecc$PresentUse == 16 | ecc$PresentUse == 25, ]
  
  # 4.6.2.3 Add information on Building Sections
  emu$PINC <- paste0(emu$PINX,".",emu$BldgNbr)
  E.commsect$PINC <- paste0(E.commsect$PINX,".",E.commsect$BldgNbr)
  emu <- merge(emu, E.commsect[,c("PINC","BldgNbr","SectionUse","SF")]
               , by.x="PINC", by.y="PINC", all.x=T)
  emu$BldgNbr.y[is.na(emu$BldgNbr.y)] <- 0
  emu$SectionUse[is.na(emu$SectionUse)] <- 0
  emu$SF.y[is.na(emu$SF.y)] <- 0
  
  # 4.6.2.4 Cull to residential sections 
  emu$Res <- 0
  emu$Res[emu$SectionUse == 300 | emu$SectionUse == 351 |
            emu$SectionUse == 352 |emu$SectionUse == 984] <- 1
  emu <- emu[emu$Res == 1, ]
  
  # 4.6.2.5 Estimate $ of Units
  emu$Units <- round(emu$SF.y/800,0)
  emu$Units[emu$Units==0] <- 1
  
  # 4.6.2.6 Sum up Res SF in Buildings 1
  ex1 <- as.data.frame(tapply(emu$SF.y[emu$BldgNbr.x==1]
                              ,emu$PINX[emu$BldgNbr.x==1],sum))
  colnames(ex1) <- "R1.SF"
  ex1$PINX <- rownames(ex1)
  
  # 4.6.2.7 Remove this SF from overall Building 1 SF
  ecx <- merge(ecc, ex1, by.x="PINX", by.y="PINX", all.x=T)
  ecx$R1.SF[is.na(ecx$R1.SF)] <- 0  
  ecx$xSF <- 0
  ecx$xSF[ecx$BldgNbr==1] <- ecx$SF[ecx$BldgNbr==1] - ecx$R1.SF[ecx$BldgNbr==1]
  ecx$SF[ecx$xSF>=0] <- ecx$xSF[ecx$xSF>=0]
  ecx$R1.SF <- NULL
  ecx$xSF <- NULL
  
  # 4.6.2.8 Sum up Res SF in Buildings 2+
  ex2 <- as.data.frame(tapply(emu$SF.y[emu$BldgNbr.x!=1]
                              ,emu$PINX[emu$BldgNbr.x!=1],sum))
  colnames(ex2) <- "R2.SF"
  ex2$PINX <- rownames(ex2)
  
  # 4.6.2.9 Remove this SF from overall Building 2+ SF
  ecx <- merge(ecx, ex2, by.x="PINX", by.y="PINX", all.x=T)
  ecx$R2.SF[is.na(ecx$R2.SF)] <- 0  
  ecx$xSF <- 0
  ecx$xSF[ecx$BldgNbr==1] <- ecx$A.SF[ecx$BldgNbr==1] - ecx$R2.SF[ecx$BldgNbr==1]
  ecx$A.SF[ecx$xSF>=0] <- ecx$xSF[ecx$xSF>=0]
  ecx$R2.SF <- NULL
  ecx$xSF <- NULL
  
  # 4.6.2.10 Sum up Units in Buildings 1
  eu1 <- as.data.frame(tapply(emu$Units[emu$BldgNbr.x==1]
                              ,emu$PINX[emu$BldgNbr.x==1],sum))
  colnames(eu1) <- "NbrUnits"
  eu1$PINX <- rownames(eu1)
  
  # 4.6.2.11 Add to NbrUnits
  ecx <- merge(ecx, eu1, by.x="PINX", by.y="PINX", all.x=T)
  ecx$NbrUnits[is.na(ecx$NbrUnits)] <- 0  
  
  # 4.6.2.12 Sum up units in Buildings 2+
  eu2 <- as.data.frame(tapply(emu$Units[emu$BldgNbr.x!=1]
                              ,emu$PINX[emu$BldgNbr.x!=1],sum))
  colnames(eu2) <- "A.NbrUnits"
  eu2$PINX <- rownames(eu2)
  
  # 4.6.2.13 Add to NbrUnits
  ecx <- merge(ecx, eu2, by.x="PINX", by.y="PINX", all.x=T)
  ecx$A.NbrUnits[is.na(ecx$A.NbrUnits)] <- 0  
  
  # 4.6.2.14 Remove secondary buildings and rename columns
  End.C <- ecx[ecx$BldgNbr==1,]
  colnames(End.C)[which(colnames(End.C)=="BldgQuality")] <- "Condition"
  colnames(End.C)[which(colnames(End.C)=="NbrStories")] <- "Stories"
  End.C$Class <- "C"
  End.C$PIN <- NULL
  End.C$BldgNbr <- NULL
  
# 4.7 Eliminate Apartments ,Condos and ResBdlg from Commercial Structures
  
 # 4.7.1 Beginning Year
  
  # 4.7.1.1 Apartments
  ba <- Beg.A[,c("PINX","Class")]
  ba$X <- 1
  ba$Class <- NULL
  Beg.C <- merge(Beg.C, ba,by.x="PINX", by.y="PINX",all.x=T)
  Beg.C$X[is.na(Beg.C$X)] <- 0  
  Beg.C <- Beg.C[Beg.C$X == 0,]
  Beg.C$X <- NULL
  
  # 4.7.1.2 Condominiums
  #   bk <- Beg.K[,c("PINX","Class")]
  #   bk$X <- 1
  #   bk$Class <- NULL
  #   Beg.C <- merge(Beg.C, bk,by.x="PINX", by.y="PINX",all.x=T)
  #   Beg.C$X[is.na(Beg.C$X)] <- 0  
  #   Beg.C <- Beg.C[Beg.C$X == 0]
  #   Beg.C$X <- NULL
  
  # 4.7.1.3 ResBldg
  #   br <- Beg.R[,c("PINX","Class")]
  #   br$X <- 1
  #   br$Class <- NULL
  #   Beg.C <- merge(Beg.C, br,by.x="PINX", by.y="PINX",all.x=T)
  #   Beg.C$X[is.na(Beg.C$X)] <- 0  
  #   Beg.C <- Beg.C[Beg.C$X == 0,]
  #   Beg.C$X <- NULL
  
 # 4.7.2 End Year
  
  # 4.7.2.1 Apartments
  ea <- End.A[,c("PINX","Class")]
  ea$X <- 1
  ea$Class <- NULL
  End.C <- merge(End.C, ea, by.x="PINX", by.y="PINX",all.x=T)
  End.C$X[is.na(End.C$X)] <- 0  
  End.C <- End.C[End.C$X == 0,]
  End.C$X <- NULL
  
  # 4.7.2.2 Condominiums
  ek <- End.K[,c("PINX","Class")]
  ek$X <- 1
  ek$Class <- NULL
  End.C <- merge(End.C, ek, by.x="PINX", by.y="PINX",all.x=T)
  End.C$X[is.na(End.C$X)] <- 0
  cutX <- which(End.C$X==1 & End.C$PresentUse >= 2 & End.C$PresentUse <= 29)
  End.C <- End.C[-cutX, ]
  
  # 4.7.2.2.1 Remove Comm Condos from Condos
  ck <- End.C[End.C$X==1 & (End.C$PresentUse == 0 | End.C$PresentUse > 29),
              c("PINX","Class")]
  ck$X <- 1
  ck$Class <- NULL
  End.C$X <- NULL
  End.K <- merge(End.K, ck, by.x="PINX", by.y="PINX",all.x=T)
  End.K$X[is.na(End.K$X)] <- 0
  End.K <- End.K[End.K$X == 0, ]
  End.K$X <- NULL

  # 4.7.2.3 Residential Buildings
  er <- End.R[,c("PINX","Class")]
  er$X <- 1
  er$Class <- NULL
  End.C <- merge(End.C, er, by.x="PINX", by.y="PINX",all.x=T)
  End.C$X[is.na(End.C$X)] <- 0  
  cutX <- which(End.C$X==1 & End.C$PresentUse >= 2 & End.C$PresentUse <= 29)
  End.C <- End.C[-cutX, ]
  
  # 4.7.2.3.1 Remove Comm Conv from Res
  ck <- End.C[End.C$X==1 & (End.C$PresentUse == 0 | End.C$PresentUse > 29),
              c("PINX","Class")]
  ck$X <- 1
  ck$Class <- NULL
  End.C$X <- NULL
  End.R <- merge(End.R, ck, by.x="PINX", by.y="PINX",all.x=T)
  End.R$X[is.na(End.R$X)] <- 0
  End.R <- End.R[End.R$X == 0, ]
  End.R$X <- NULL
  
  # 4.8.1 Clean up Present Use Field
  Beg.C$PresentUse <- NULL
  End.C$PresentUse <- NULL
  
################################################################################
# 5.0 Merge R, A, K and C Data  ------------------------------------------------
  
# 5.1 Merge Beginning Year  
  Beg.All <- rbind(Beg.R, Beg.A, Beg.C, Beg.K)
  Beg.All <- rmDup(Beg.All, "PINX")

# 5.2 Merge End Year  
  End.All <- rbind(End.R, End.A, End.C, End.K)
  End.All <- rmDup(End.All, "PINX")
  
# 5.3 Remove End Year duplicates  
  
  # 5.3.1 Identify and Split
  EX <- as.data.frame(table(End.All$PINX))
  colnames(EX) <- c("PINX", "Freq")
  eax <- merge(End.All, EX, by.x="PINX", by.y="PINX", all.x=T)
  eok <- eax[eax$Freq==1,]
  eno <- eax[eax$Freq>1,]
  
 # 5.3.2 Go through Duplicate R/A Parcels and Remove/Combine  
  eno.c <- rownames(table(eno$PINX))
  for(k in 1:length(eno.c)){
    a <- which(eno$PINX == eno.c[k])
    old <- which(eno$YearBuilt[a] <= Beg.Year)
    new <- which(eno$YearBuilt[a] > Beg.Year)
    if(length(old)==1 & length(new)==1){
      eno$A.NbrUnits[a[old]] <- eno$A.NbrUnits[a[old]] + eno$NbrUnits[a[new]] 
      eno$A.SF[a[old]] <- eno$A.SF[a[old]] + eno$SF[a[new]] 
      eno$NbrBldgs[a[old]] <- eno$NbrBldgs[a[old]] + eno$NbrBldgs[a[new]]
      eno <- eno[-a[new], ]
    }
    if(length(new)==2){
     if(abs(eno$SF[a[new[1]]]/(eno$SF[a[new[1]]]+1)) < .1){
       eno <- eno[-a[new][2],]
     }
     if(abs(eno$SF[a[new[1]]]/(eno$SF[a[new[1]]]+1)) > .1){
       eno$A.NbrUnits[a[1]] <- eno$A.NbrUnits[a[1]] + eno$NbrUnits[a[2]] 
       eno$A.SF[a[1]] <- eno$A.SF[a[1]] + eno$SF[a[2]] 
       eno$NbrBldgs[a[1]] <- eno$NbrBldgs[a[1]] + eno$NbrBldgs[a[2]]
       eno <- eno[-a[2], ]
     }
    }
    if(length(old)==2){
      eno <- eno[-a[1],]
    }
  }  

# 5.4 Bring back together  
  eno$Freq <- NULL
  eok$Freq <- NULL
  End.All <- rbind(eok, eno)
  
################################################################################
# 6.0 Add to Parcel Data
  
  Beg.P <- B.parcel
  Beg.P <- merge(Beg.P, Beg.All, by.x="PINX", by.y="PINX", all.x=T)
  Beg.P$PIN <- NULL
  Beg.P$SF[is.na(Beg.P$SF)] <- 0
  Beg.P$YearBuilt[is.na(Beg.P$YearBuilt)] <- 0
  Beg.P$Condition[is.na(Beg.P$Condition)] <- 0
  Beg.P$NbrUnits[is.na(Beg.P$NbrUnits)] <- 0
  Beg.P$Stories[is.na(Beg.P$Stories)] <- 0
  Beg.P$NbrBldgs[is.na(Beg.P$NbrBldgs)] <- 0
  Beg.P$A.NbrUnits[is.na(Beg.P$A.NbrUnits)] <- 0
  Beg.P$A.SF[is.na(Beg.P$A.SF)] <- 0
  Beg.P$A.YB[is.na(Beg.P$A.YB)] <- 0
  Beg.P$Class[is.na(Beg.P$Class)] <- "V"
  
  End.P <- E.parcel
  End.P <- merge(End.P, End.All, by.x="PINX", by.y="PINX", all.x=T)
  End.P$PIN <- NULL
  End.P$SF[is.na(End.P$SF)] <- 0
  End.P$YearBuilt[is.na(End.P$YearBuilt)] <- 0
  End.P$Condition[is.na(End.P$Condition)] <- 0
  End.P$NbrUnits[is.na(End.P$NbrUnits)] <- 0
  End.P$Stories[is.na(End.P$Stories)] <- 0
  End.P$NbrBldgs[is.na(End.P$NbrBldgs)] <- 0
  End.P$A.NbrUnits[is.na(End.P$A.NbrUnits)] <- 0
  End.P$A.SF[is.na(End.P$A.SF)] <- 0
  End.P$A.YB[is.na(End.P$A.YB)] <- 0
  End.P$Class[is.na(End.P$Class)] <- "V"
  
################################################################################
# 7.0 Merge into Parcel List  --------------------------------------------------

# 7.1 Match End Parcels to Beginning parcels
  BegEnd <- merge(Beg.P, End.P, "PINX", "PINX", all.x=T)
  colnames(BegEnd) <- c("PINX","B.Use","B.Zone","B.LotSF","B.SF","B.YearBuilt",
                      "B.Cond","B.Units","B.Stories","B.NbrBldgs","B.aUnits",
                      "B.aSF","B.aYB","B.Class","E.Use","E.Zone","E.LotSF",
                      "E.SF","E.YearBuilt","E.Cond","E.Units","E.Stories",
                      "E.NbrBldgs","E.aUnits","E.aSF","E.aYB","E.Class")

# 7.2 Label as B (beginning only) or A (all years)  
  BegEnd$RType <- ifelse(is.na(BegEnd$E.Class),"B","A")

# 7.3 Fill in Empties
  BegEnd$E.Use[is.na(BegEnd$E.Use)] <- 0
  BegEnd$E.Zone[is.na(BegEnd$E.Zone)] <- "None"
  BegEnd$E.LotSF[is.na(BegEnd$E.LotSF)] <- 0
  BegEnd$E.SF[is.na(BegEnd$E.SF)] <- 0
  BegEnd$E.YearBuilt[is.na(BegEnd$E.YearBuilt)] <- 0
  BegEnd$E.Cond[is.na(BegEnd$E.Cond)] <- 0
  BegEnd$E.Units[is.na(BegEnd$E.Units)] <- 0
  BegEnd$E.Stories[is.na(BegEnd$E.Stories)] <- 0
  BegEnd$E.NbrBldgs[is.na(BegEnd$E.NbrBldgs)] <- 0
  BegEnd$E.aUnits[is.na(BegEnd$E.aUnits)] <- 0
  BegEnd$E.aSF[is.na(BegEnd$E.aSF)] <- 0
  BegEnd$E.aYB[is.na(BegEnd$E.aYB)] <- 0
  
# 7.4 Match Beginning Parcels to End Parcels
  EndAdd <- merge(End.P, Beg.P,"PINX", "PINX", all.x=T)
  colnames(EndAdd) <- c("PINX","E.Use","E.Zone","E.LotSF","E.SF","E.YearBuilt",
                      "E.Cond","E.Units","E.Stories","E.NbrBldgs","E.aUnits",
                      "E.aSF","E.aYB","E.Class","B.Use","B.Zone","B.LotSF",
                      "B.SF","B.YearBuilt","B.Cond","B.Units","B.Stories",
                      "B.NbrBldgs","B.aUnits","B.aSF","B.aYB","B.Class")

# 7.5 Label as E (End only) or A (all years)  
  EndAdd$RType <- ifelse(is.na(EndAdd$B.Class), "E", "A")
  EndAdd <- EndAdd[EndAdd$RType == "E", ]

# 7.6 Fill in Empties
  EndAdd$B.Use[is.na(EndAdd$B.Use)] <- 0
  EndAdd$B.Zone[is.na(EndAdd$B.Zone)] <- "None"
  EndAdd$B.LotSF[is.na(EndAdd$B.LotSF)] <- 0
  EndAdd$B.SF[is.na(EndAdd$B.SF)] <- 0
  EndAdd$B.YearBuilt[is.na(EndAdd$B.YearBuilt)] <- 0
  EndAdd$B.Cond[is.na(EndAdd$B.Cond)] <- 0
  EndAdd$B.Units[is.na(EndAdd$B.Units)] <- 0
  EndAdd$B.Stories[is.na(EndAdd$B.Stories)] <- 0
  EndAdd$B.NbrBldgs[is.na(EndAdd$B.NbrBldgs)] <- 0
  EndAdd$B.aUnits[is.na(EndAdd$B.aUnits)] <- 0
  EndAdd$B.aSF[is.na(EndAdd$B.aSF)] <- 0
  EndAdd$B.aYB[is.na(EndAdd$B.aYB)] <- 0
   
# 7.7 Comine A and B from 3.1 with only E from 3.2
  Parcel.List <- rbind(BegEnd, EndAdd)
  Parcel.List$B.Class <- as.character(Parcel.List$B.Class)
  Parcel.List$B.Class[is.na(Parcel.List$B.Class)] <- "X"
  Parcel.List$E.Class <- as.character(Parcel.List$E.Class)
  Parcel.List$E.Class[is.na(Parcel.List$E.Class)] <- "X"
    
# 7.8 Clean up Temp Data
  rm(EndAdd);rm(BegEnd)

return(Parcel.List)
}

