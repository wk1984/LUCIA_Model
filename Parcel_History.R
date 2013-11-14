################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: Parcel_History                                               #
#                                                                              #
#        Compares the parcel geometry data from two years                      #                                                                                    ###          by Andy Krause                                                  ###
#        To determine the changes to the parcel over that time                 #
#                                                                              #
#        Most Recent Update: 11/12/2013                                        #
#                                                                              # 
###############################################################################'

createParcelHistory <- function(Parcel.List, Par.Geom, Beg.Year, End.Year){

# 0.0 Set Global Parameters, Load Libraries and Files --------------------------

# 0.1 Load Libraries

  require(RODBC)
  options(warn=-1)

# 0.2 Load Files

  source("D://Code//R//General//R_Helpers//Spatial_Helpers.R")
  source("D://Code//R//General//R_Helpers//Basic_Helpers.R")
  source("D://Code//R//Research//LUCIA_Model//parcelFinder.R")  
  source("D://Code//R//Research//LUCIA_Model//yearFix.R")
  source("D://Code//R//Research//LUCIA_Model//createParcelList.R")
  source("D://Code//R//Research//LUCIA_Model//AchangeFinder.R")
  source("D://Code//R//Research//LUCIA_Model//RchangeFinder.R")
  source("D://Code//R//Research//LUCIA_Model//PchangeFinder.R")
  source("D://Code//R//Research//LUCIA_Model//CchangeFinder.R")


################################################################################
# 1.0 Merge Parcel List and Parcel Geometry Data  ------------------------------

# 1.1 Merge to the Parcel List
  
  AllPT <- merge(Par.Geom, Parcel.List, by.x="PINX", by.y="PINX")

################################################################################
# 2.0 Assign Original Consistencies --------------------------------------------

# 2.1 Record Type Consistency --------------------------------------------------
  
  AllPT$RT.Cons <- "Yes"
  AllPT$RT.Cons[AllPT$E.Class != AllPT$B.Class] <- "No"
  AllPT$RT.Cons[AllPT$E.Class == "X"] <- "No"
  AllPT$RT.Cons[AllPT$B.Class == "X"] <- "No"
  AllPT$RT.Cons[AllPT$B.Class == "K" & AllPT$E.Class == "C.K"] <- "Yes"
  AllPT$RT.Cons[AllPT$B.Class == "A" & AllPT$E.Class == "C.A"] <- "Yes"
  AllPT$RT.Cons[AllPT$B.Class == "C.A" & AllPT$E.Class == "A"] <- "Yes"

# 2.2 Parcel Type Consistency --------------------------------------------------

  AllPT$PType <- AllPT$Type
  AllPT$Type <- NULL
  AllPT$PT.Cons <- "No"
  AllPT$PT.Cons[AllPT$PType == "A" & 
                (AllPT$Topo.Type == "Consistent" | 
                AllPT$Topo.Type == "Lot Adjustment" |
                AllPT$Topo.Type == "Consistent -  MP" |
                AllPT$Topo.Type == "Consistent - Rel" |
                AllPT$Topo.Type == "Lot Adj - Exp") |
                (AllPT$Topo.Type == "New - Renamed" & AllPT$B.Use != 0)] <- "Yes"

# 2.3 Determine Joint Consistency ----------------------------------------------

  AllPT$RPT.Cons <- "No"
  AllPT$RPT.Cons[AllPT$RT.Cons == "Yes" & AllPT$PT.Cons == "Yes"] <- "Yes"

# 2.4 Split into RPT.Cons and Not ----------------------------------------------

  C.rpt <- AllPT[AllPT$RPT.Cons == "Yes", ]
  NC.rpt <- AllPT[AllPT$RPT.Cons == "No", ]

################################################################################
# 3.0 Judge Structural Consistency of RPT.yes Parcels    -----------------------

# 3.1 Split up Consistent ------------------------------------------------------               

  pc.R <- C.rpt[C.rpt$B.Class == "R", ]
  pc.K <- C.rpt[C.rpt$B.Class == "K", ]
  pc.C <- C.rpt[C.rpt$B.Class == "C", ]
  pc.A <- C.rpt[C.rpt$B.Class == "A" | C.rpt$B.Class == "C.A", ]
  pc.V <- C.rpt[C.rpt$B.Class == "V", ]

# 3.2 Evaluate Residential Consistency -----------------------------------------

  pc.R$Str.Cons <- "No"
  pc.R$Str.Cons[pc.R$B.SF == pc.R$E.SF &
                pc.R$B.YearBuilt == pc.R$E.YearBuilt &
                pc.R$B.Units == pc.R$E.Units &
                pc.R$B.NbrBldgs == pc.R$E.NbrBldgs &
                pc.R$B.aSF == pc.R$E.aSF] <- "Yes"

# 3.3 Evaluate Condominium Consistency -----------------------------------------

  if(dim(pc.K)[1] > 0){  
    pc.K$Str.Cons <- "No"
    pc.K$Str.Cons[(abs((pc.K$B.SF-pc.K$E.SF)/pc.K$B.SF) < .1)  &
                pc.K$B.YearBuilt == pc.K$E.YearBuilt &
                pc.K$B.Units == pc.K$E.Units] <- "Yes"
  }

# 3.4 Evaluate Apartment Consistency -------------------------------------------

  if(dim(pc.A)[1] > 0){
    pc.A$Str.Cons <- "No"
    pc.A$Str.Cons[(abs((pc.A$B.SF-pc.A$E.SF)/pc.A$B.SF) < .1)  &
                pc.A$B.YearBuilt == pc.A$E.YearBuilt &
                pc.A$B.Units == pc.A$E.Units] <- "Yes"
  }
    
# 3.5 Evaluate Commercial Consistency ------------------------------------------

  pc.C$Str.Cons <- "No"
  pc.C$Str.Cons[(abs((pc.C$B.SF-pc.C$E.SF)/pc.C$B.SF) < .1)  &
                pc.C$B.YearBuilt == pc.C$E.YearBuilt] <- "Yes"

# 3.6 Label Vacant as Consistent -----------------------------------------------

  pc.V$Str.Cons <- "Yes"

# 3.7 Merge Back Together ------------------------------------------------------

  all.pc <- rbind(pc.R, pc.K, pc.A, pc.C, pc.V)

# 3.8 Merge with NC.rpt --------------------------------------------------------

  NC.rpt$Str.Cons <- "No"
  All.RPS <- rbind(all.pc,NC.rpt)

# 3.9 Clean Up -----------------------------------------------------------------

  rm(R.Crpt);rm(A.Crpt);rm(K.Crpt);rm(C.Crpt);rm(V.Crpt)
  rm(pc.R);rm(pc.A);rm(pc.K);rm(pc.C)

################################################################################
# 4.0 Divide Parcels up for further analysis  ----------------------------------

# 4.1 Split by RPS.Cons --------------------------------------------------------

  All.Cons <- All.RPS[All.RPS$RPT.Cons == "Yes" & All.RPS$Str.Cons == "Yes", ]
  All.NC <- All.RPS[All.RPS$RPT.Cons != "Yes" | All.RPS$Str.Cons != "Yes", ]

# 4.2 Split NCs by RPT.Cons ----------------------------------------------------

  NC.RPTyes <- All.NC[All.NC$RPT.Cons=="Yes", ]
  NC.RPTno <- All.NC[All.NC$RPT.Cons=="No", ]

# 4.3 Divide RP.Yes into record types ------------------------------------------

  pc.R <- NC.RPTyes[NC.RPTyes$B.Class=="R",]
  pc.K <- NC.RPTyes[NC.RPTyes$B.Class=="K",]
  pc.A <- NC.RPTyes[NC.RPTyes$B.Class=="C.A" | NC.RPTyes$B.Class=="A",]
  pc.C <- NC.RPTyes[NC.RPTyes$B.Class=="C",]

# 4.4 Add Fields to All.Cons ---------------------------------------------------

  All.Cons$Chng.Type <- "None"
  All.Cons$Loss.Units <- 0
  All.Cons$Loss.SF <- 0
  All.Cons$Gain.Units <- 0
  All.Cons$Gain.SF <- 0
  All.Cons$Chng.Time <- 0
  All.Cons$Use.Chng <- "None"

################################################################################
# 5.0 Residential Structural Changes -------------------------------------------

# 5.1 Label the individual changes to the structure(s) -------------------------

  pc.R$ChangeSum <- 0
  pc.R$MinusBldg <- 0
  pc.R$MinusBldg[pc.R$B.Bldgs > pc.R$E.Bldgs] <- 1
  pc.R$Rebuild <- 0
  pc.R$Rebuild[pc.R$E.YearBuilt > Beg.Year] <- 1
  pc.R$PlusUnits <- 0
  pc.R$PlusUnits[(pc.R$E.Units + pc.R$E.aUnits) > 
                   (pc.R$B.Units + pc.R$B.aUnits)] <- 1
  pc.R$MinusUnits <- 0
  pc.R$MinusUnits[(pc.R$E.Units + pc.R$E.aUnits) < 
                 (pc.R$B.Units + pc.R$B.aUnits)] <- 1
  pc.R$NewADU <- 0
  pc.R$NewADU[pc.R$E.aYB > Beg.Year] <- 1

# 5.2 Sum up Changes -----------------------------------------------------------

  pc.R$ChangeSum <- rowSums(pc.R[ ,(which(colnames(pc.R) == 
                                    "Str.Cons") + 1):dim(pc.R)[2]])

# 5.3 Divide Off Those with only Assessor's Updates ----------------------------

  pc.R.cons <- pc.R[pc.R$ChangeSum == 0, ]
  pc.R.cons$Use.Chng <- "None"
  pc.R.cons$Chng.Time <- 0
  pc.R.cons$Chng.Type <- "A.C."
  pc.R.cons$Loss.Units <- 0
  pc.R.cons$Loss.SF <- 0
  pc.R.cons$Gain.Units <- 0
  pc.R.cons$Gain.SF <- 0

# 5.4. Split off Those with Real Changes ---------------------------------------

  nc.R <- pc.R[pc.R$ChangeSum != 0, ]

 # 5.4.1 Set up Variables
  nc.R$Use.Chng <- "None"
  nc.R$Chng.Time <- 0
  nc.R$Chng.Type <- "X"
  nc.R$Loss.Units <- 0
  nc.R$Loss.SF <- 0
  nc.R$Gain.Units <- 0
  nc.R$Gain.SF <- 0

 # 5.4.2 Label the Rebuilds
  reb <- which(nc.R$Rebuild == 1)
  nc.R$Chng.Type[reb] <- "Redev"
  nc.R$Chng.Time[reb] <- nc.R$E.YearBuilt[reb]
  nc.R$Loss.Units[reb] <- nc.R$B.Units[reb] + nc.R$B.aUnits[reb]
  nc.R$Loss.SF[reb] <- nc.R$B.SF[reb] + nc.R$B.aSF[reb]
  nc.R$Gain.Units[reb] <- nc.R$E.Units[reb] + nc.R$E.aUnits[reb]
  nc.R$Gain.SF[reb] <- nc.R$E.SF[reb] + nc.R$E.aSF[reb]

 # 5.4.3 Label the New Accessory Dwelling Units
  nadu <- which(nc.R$NewADU == 1 & nc.R$Rebuild != 1)
  nc.R$Chng.Type[nadu] <- "New"
  nc.R$Chng.Time[nadu] <- nc.R$E.aYB[nadu]
  nc.R$Gain.Units[nadu] <- nc.R$E.aUnits[nadu]
  nc.R$Gain.SF[nadu] <- nc.R$E.aSF[nadu]

 # 5.4.4 Label the Unit Losses 
  unl <- which(nc.R$ChangeSum==1 & nc.R$MinusUnits==1)
  nc.R$Chng.Type[unl] <- "Dens"
  nc.R$Chng.Time[unl] <- yearFix(
    RchangeFinder(nc.R[unl, ],"NbrLivingUnits", Beg.Year, End.Year))
  nc.R$Loss.Units[unl] <- (nc.R$B.Units[unl] + nc.R$B.aUnits[unl]) - 
                            (nc.R$E.Units[unl] + nc.R$E.aUnits[unl])
  nc.R$Loss.SF[unl] <- (nc.R$B.SF[unl] + nc.R$B.aSF[unl]) - 
                             (nc.R$E.SF[unl] + nc.R$E.aSF[unl])

 # 5.4.5 Label the Unit Gains 
  ung <- which(nc.R$ChangeSum==1 & nc.R$PlusUnits==1)
  nc.R$Chng.Type[ung] <- "Dens"
  nc.R$Chng.Time[ung] <- yearFix(
    RchangeFinder(nc.R[ung,], "NbrLivingUnits", Beg.Year, End.Year))
  nc.R$Gain.Units[ung] <- (nc.R$E.Units[ung] + nc.R$E.aUnits[ung]) - 
                            (nc.R$B.Units[ung] + nc.R$B.aUnits[ung])
  nc.R$Gain.SF[ung] <- (nc.R$E.SF[ung] + nc.R$E.aSF[ung]) - 
                            (nc.R$E.SF[ung] + nc.R$E.aSF[ung])

 # 5.4.6 Label the Building Losses
  bgl <- which(nc.R$MinusBldg == 1)
  nc.R$Chng.Type[bgl] <- "Demo"
  nc.R$Chng.Time[bgl] <- yearFix(
     RchangeFinder(nc.R[bgl,],"NbrBldgs", Beg.Year, End.Year))
  nc.R$Loss.Units[bgl] <- (nc.R$B.Units[bgl] + nc.R$B.aUnits[bgl]) - 
                           (nc.R$E.Units[bgl] + nc.R$E.aUnits[bgl])
  nc.R$Loss.SF[bgl] <- (nc.R$B.SF[bgl] + nc.R$B.aSF[bgl]) - 
                          (nc.R$E.SF[bgl] + nc.R$E.aSF[bgl])

 # 5.4.7 Label the Remodels
  rem <- which(pc.R.cons$Chng.Type == "A.C." & 
                 abs((pc.R.cons$B.SF - pc.R.cons$E.SF)/pc.R.cons$B.SF) > .1)
  if(length(rem) > 0){
    pc.R.cons$Chng.Type[rem] <- "Exp"
    pc.R.cons$Gain.SF[rem] <- (pc.R.cons$E.SF[rem] + pc.R.cons$E.aSF[rem]) - 
                         (pc.R.cons$E.SF[rem] + pc.R.cons$E.aSF[rem])
    pc.R.cons$Chng.Time[rem] <- yearFix(
       RchangeFinder(pc.R.cons[rem,],"SqFtTotLiving", Beg.Year, End.Year)) 
  }

# 5.5 Re-combine pc.R ----------------------------------------------------------

  par.R <- rbind(pc.R.cons, nc.R)

# 5.6 Check for negatives ------------------------------------------------------

  par.R$Gain.Units[par.R$Loss.Units < 0] <- -par.R$Loss.Units[
                                                par.R$Loss.Units < 0]
  par.R$Loss.Units[par.R$Loss.Units < 0] <- 0

  par.R$Gain.SF[par.R$Loss.SF < 0] <- -par.R$Loss.SF[
                                                par.R$Loss.SF < 0]
  par.R$Loss.SF[par.R$Loss.SF < 0] <- 0

  par.R$Loss.Units[par.R$Gain.Units < 0] <- -par.R$Gain.Units[
                                                par.R$Gain.Units < 0]
  par.R$Gain.Units[par.R$Gain.Units < 0] <- 0

  par.R$Loss.SF[par.R$Gain.SF < 0] <- -par.R$Gain.SF[
                                                par.R$Gain.SF < 0]
  par.R$Gain.SF[par.R$Gain.SF < 0] <- 0

# 5.7 Clean up Fields ----------------------------------------------------------

  par.R$ChangeSum <- NULL
  par.R$MinusBldg <- NULL
  par.R$Rebuild <- NULL
  par.R$PlusUnits <- NULL
  par.R$MinusUnits <- NULL
  par.R$NewADU <- NULL

################################################################################
# 6.0 Condiminum Structural Changes --------------------------------------------

if(dim(pc.K)[1] > 0){
  
# 6.1 Label the individual changes to the structure(s) -----------------------
  
  pc.K$ChangeSum <- 0
  pc.K$PlusUnits <- 0
  pc.K$PlusUnits[pc.K$E.Units > pc.K$B.Units] <- 1
  pc.K$MinusUnits <- 0
  pc.K$MinusUnits[pc.K$E.Units < pc.K$B.Units] <- 1
  pc.K$Rebuild <- 0
  pc.K$Rebuild[pc.K$E.YearBuilt > Beg.Year] <- 1
  pc.K$PlusBldg <- 0
  pc.K$PlusBldg[pc.K$E.NbrBldgs > pc.K$B.NbrBldg] <- 1
  pc.K$MinusBldg <- 0
  pc.K$MinusBldg[pc.K$B.NbrBldgs > pc.K$E.NbrBldg] <- 1
  
# 6.2 Sum up Changes -----------------------------------------------------------
  
  pc.K$ChangeSum <- rowSums(pc.K[,(which(
    colnames(pc.K) =="ChangeSum")+1):dim(pc.K)[2]])
  
# 6.3 Divide Off Those with only Assessor's Updates ----------------------------
  
  pc.K.cons <- pc.K[pc.K$ChangeSum==0,]
  pc.K.cons$Use.Chng <- "None"
  pc.K.cons$Chng.Time <- 0
  pc.K.cons$Chng.Type <- "A.C."
  pc.K.cons$Loss.Units <- 0
  pc.K.cons$Loss.SF <- 0
  pc.K.cons$Gain.Units <- 0
  pc.K.cons$Gain.SF <- 0
  
# 6.4 Split off Those with Real Changes --------------------------------------

  nc.K <- pc.K[pc.K$ChangeSum!=0,]
  
# 6.5 Set up Variables -------------------------------------------------------

  nc.K$Use.Chng <- "None"
  nc.K$Chng.Time <- 0
  nc.K$Chng.Type <- "X"
  nc.K$Loss.Units <- 0
  nc.K$Loss.SF <- 0
  nc.K$Gain.Units <- 0
  nc.K$Gain.SF <- 0
  
  # 6.5.1 Label the Rebuilds
  reb <- which(nc.K$Rebuild == 1)
  nc.K$Chng.Type[nc.K$Rebuild==1] <- "Redev"
  nc.K$Chng.Time[reb] <- nc.K$E.YearBuilt[reb]
  nc.K$Loss.Units[reb] <- nc.K$B.Units[reb] + nc.K$B.aUnits[reb]
  nc.K$Loss.SF[reb] <- nc.K$B.SF[reb] + nc.K$B.aSF[reb]
  nc.K$Gain.Units[reb] <- nc.K$E.Units[reb] + nc.K$E.aUnits[reb]
  nc.K$Gain.SF[reb] <- nc.K$E.SF[reb] + nc.K$E.aSF[reb]
  
  # 6.5.2 Label the Unit-Loss
  unl <- which(nc.K$Chng.Type != "Rebuild" & nc.K$MinusUnits==1)
  nc.K$Chng.Type[unl] <- "Dens"
  nc.K$Chng.Time[unl] <- yearFix(
    AchangeFinder(nc.K[unl,],"NbrUnits", Beg.Year, End.Year))
  nc.K$Loss.Units[unl] <- (nc.K$B.Units[unl] + nc.K$B.aUnits[unl]) - 
    (nc.K$E.Units[unl] + nc.K$E.aUnits[unl])
  nc.K$Loss.SF[unl] <- (nc.K$B.SF[unl] + nc.K$B.aSF[unl]) - 
    (nc.K$E.SF[unl] + nc.K$E.aSF[unl])
  
  # 6.5.3 Label the Unit Gain
  ung <- which(nc.K$Chng.Type != "Rebuild" & nc.K$PlusUnits==1)
  nc.K$Chng.Type[ung] <- "Dens"
  nc.K$Chng.Time[ung] <- yearFix(
    AchangeFinder(nc.K[ung,], "NbrUnits", Beg.Year, End.Year))
  nc.K$Gain.Units[ung] <- (nc.K$E.Units[ung] + nc.K$E.aUnits[ung]) - 
    (nc.K$B.Units[ung] + nc.K$B.aUnits[ung])
  nc.K$Gain.SF[ung] <- (nc.K$E.SF[ung] + nc.K$E.aSF[ung]) - 
    (nc.K$E.SF[ung] + nc.K$E.aSF[ung])  
  
  # 6.5.4 Label the Building Gain
  bgg <- which(nc.K$PlusBldg == 1)
  nc.K$Chng.Type[bgg] <- "New"
  nc.K$Chng.Time[bgg] <- yearFix(
    AchangeFinder(nc.R[bgg,],"NbrBldgs", Beg.Year, End.Year))
  nc.K$Gain.Units[bgg] <- (nc.K$E.Units[bgg] + nc.K$E.aUnits[bgg]) - 
    (nc.K$B.Units[bgg] + nc.K$B.aUnits[bgg])
  nc.K$Gain.SF[bgg] <- (nc.K$E.SF[bgg] + nc.K$E.aSF[bgg]) - 
    (nc.K$B.SF[bgg] + nc.K$B.aSF[bgg])
  
  # 6.5.5 Label the Building Loss
  bgl <- which(nc.K$MinusBldg == 1)
  nc.K$Chng.Type[bgl] <- "Demo"
  nc.K$Chng.Time[bgl] <- yearFix(
    AchangeFinder(nc.R[bgl,],"NbrBldgs", Beg.Year, End.Year))
  nc.K$Loss.Units[bgl] <- (nc.K$B.Units[bgl] + nc.K$B.aUnits[bgl]) - 
    (nc.K$E.Units[bgl] + nc.K$E.aUnits[bgl])
  nc.K$Loss.SF[bgl] <- (nc.K$B.SF[bgl] + nc.K$B.aSF[bgl]) - 
    (nc.K$E.SF[bgl] + nc.K$E.aSF[bgl])
  
# 6.6 Re-combine pc.K ----------------------------------------------------------

  par.K <- rbind(pc.K.cons, nc.K)
  
# 6.7 Check for negatives ------------------------------------------------------

  par.K$Gain.Units[par.K$Loss.Units < 0] <- -par.K$Loss.Units[
    par.K$Loss.Units < 0]
  par.K$Loss.Units[par.K$Loss.Units < 0] <- 0
  
  par.K$Gain.SF[par.K$Loss.SF < 0] <- -par.K$Loss.SF[
    par.K$Loss.SF < 0]
  par.K$Loss.SF[par.K$Loss.SF < 0] <- 0
  
  par.K$Loss.Units[par.K$Gain.Units < 0] <- -par.K$Gain.Units[
    par.K$Gain.Units < 0]
  par.K$Gain.Units[par.K$Gain.Units < 0] <- 0
  
  par.K$Loss.SF[par.K$Gain.SF < 0] <- -par.K$Gain.SF[
    par.K$Gain.SF < 0]
  par.K$Gain.SF[par.K$Gain.SF < 0] <- 0
  
# 6.8 Clean up Fields ----------------------------------------------------------

  par.K$ChangeSum <- NULL
  par.K$MinusBldg <- NULL
  par.K$PlusBldg <- NULL
  par.K$Rebuild <- NULL
  par.K$PlusUnits <- NULL
  par.K$MinusUnits <- NULL
  
}

################################################################################
# 7.0 Apartment Structural Consistency -----------------------------------------

# 7.1 Label the individual changes to the structure(s) -------------------------

   pc.A$ChangeSum <- 0
   pc.A$PlusUnits <- 0
   pc.A$PlusUnits[pc.A$E.Units > pc.A$B.Units] <- 1
   pc.A$MinusUnits <- 0
   pc.A$MinusUnits[pc.A$E.Units < pc.A$B.Units] <- 1
   pc.A$Rebuild <- 0
   pc.A$Rebuild[pc.A$E.YearBuilt > Beg.Year] <- 1
   pc.A$PlusBldg <- 0
   pc.A$PlusBldg[pc.A$E.NbrBldgs > pc.A$B.NbrBldg] <- 1
   pc.A$MinusBldg <- 0
   pc.A$MinusBldg[pc.A$B.NbrBldgs > pc.A$E.NbrBldg] <- 1
   
# 7.2 Sum up Changes -----------------------------------------------------------
   
  pc.A$ChangeSum <- rowSums(pc.A[,(which(
     colnames(pc.A) =="ChangeSum")+1):dim(pc.A)[2]])
   
# 7.3 Divide Off Those with only Assessor's Updates ----------------------------
  
  pc.A.cons <- pc.A[pc.A$ChangeSum==0,]
   pc.A.cons$Use.Chng <- "None"
   pc.A.cons$Chng.Time <- 0
   pc.A.cons$Chng.Type <- "A.C."
   pc.A.cons$Loss.Units <- 0
   pc.A.cons$Loss.SF <- 0
   pc.A.cons$Gain.Units <- 0
   pc.A.cons$Gain.SF <- 0

# 7.4 Split off Those with Real Changes ----------------------------------------

   nc.A <- pc.A[pc.A$ChangeSum!=0,]

# 7.5 Set up Variables ---------------------------------------------------------

   nc.A$Use.Chng <- "None"
   nc.A$Chng.Time <- 0
   nc.A$Chng.Type <- "X"
   nc.A$Loss.Units <- 0
   nc.A$Loss.SF <- 0
   nc.A$Gain.Units <- 0
   nc.A$Gain.SF <- 0
 
 # 7.5.1 Label the Rebuilds
   reb <- which(nc.A$Rebuild == 1)
   nc.A$Chng.Type[nc.A$Rebuild==1] <- "Redev"
   nc.A$Chng.Time[reb] <- nc.A$E.YearBuilt[reb]
   nc.A$Loss.Units[reb] <- nc.A$B.Units[reb] + nc.A$B.aUnits[reb]
   nc.A$Loss.SF[reb] <- nc.A$B.SF[reb] + nc.A$B.aSF[reb]
   nc.A$Gain.Units[reb] <- nc.A$E.Units[reb] + nc.A$E.aUnits[reb]
   nc.A$Gain.SF[reb] <- nc.A$E.SF[reb] + nc.A$E.aSF[reb]
 
 # 7.5.2 Label the Unit-Loss
   unl <- which(nc.A$Chng.Type != "Rebuild" & nc.A$MinusUnits==1)
   nc.A$Chng.Type[unl] <- "Dens"
   nc.A$Chng.Time[unl] <- yearFix(
     AchangeFinder(nc.A[unl,],"NbrUnits", Beg.Year, End.Year))
   nc.A$Loss.Units[unl] <- (nc.A$B.Units[unl] + nc.A$B.aUnits[unl]) - 
                            (nc.A$E.Units[unl] + nc.A$E.aUnits[unl])
   nc.A$Loss.SF[unl] <- (nc.A$B.SF[unl] + nc.A$B.aSF[unl]) - 
                            (nc.A$E.SF[unl] + nc.A$E.aSF[unl])

  # 7.5.3 Label the Unit Gain
   ung <- which(nc.A$Chng.Type != "Rebuild" & nc.A$PlusUnits==1)
   nc.A$Chng.Type[ung] <- "Dens"
   nc.A$Chng.Time[ung] <- yearFix(
       AchangeFinder(nc.A[ung,], "NbrUnits", Beg.Year, End.Year))
   nc.A$Gain.Units[ung] <- (nc.A$E.Units[ung] + nc.A$E.aUnits[ung]) - 
                                  (nc.A$B.Units[ung] + nc.A$B.aUnits[ung])
   nc.A$Gain.SF[ung] <- (nc.A$E.SF[ung] + nc.A$E.aSF[ung]) - 
                                  (nc.A$B.SF[ung] + nc.A$B.aSF[ung])  

  # 7.5.4 Label the Building Gain
   bgg <- which(nc.A$PlusBldg == 1 & nc.A$Rebuild != 1 & nc.A$E.YearBuilt >= Beg.Year)
   nc.A$Chng.Type[bgg] <- "New"
   nc.A$Chng.Time[bgg] <- yearFix(
     AchangeFinder(nc.R[bgg,],"NbrBldgs", Beg.Year, End.Year))
   nc.A$Gain.Units[bgg] <- (nc.A$E.Units[bgg] + nc.A$E.aUnits[bgg]) 
                               
   nc.A$Gain.SF[bgg] <- (nc.A$E.SF[bgg] + nc.A$E.aSF[bgg])  
                        

  # 7.5.5 Label the Building Loss
   bgl <- which(nc.A$MinusBldg == 1)
   nc.A$Chng.Type[bgl] <- "Demo"
   nc.A$Chng.Time[bgl] <- yearFix(
        AchangeFinder(nc.R[bgl,],"NbrBldgs", Beg.Year, End.Year))
   nc.A$Loss.Units[bgl] <- (nc.A$B.Units[bgl] + nc.A$B.aUnits[bgl]) - 
                             (nc.A$E.Units[bgl] + nc.A$E.aUnits[bgl])
   nc.A$Loss.SF[bgl] <- (nc.A$B.SF[bgl] + nc.A$B.aSF[bgl]) - 
                             (nc.A$E.SF[bgl] + nc.A$E.aSF[bgl])

# 7.6 Re-combine pc.R ----------------------------------------------------------

   par.A <- rbind(pc.A.cons, nc.A)

# 7.7 Check for negatives ------------------------------------------------------

   par.A$Gain.Units[par.A$Loss.Units < 0] <- -par.A$Loss.Units[
     par.A$Loss.Units < 0]
   par.A$Loss.Units[par.A$Loss.Units < 0] <- 0

   par.A$Gain.SF[par.A$Loss.SF < 0] <- -par.A$Loss.SF[
     par.A$Loss.SF < 0]
   par.A$Loss.SF[par.A$Loss.SF < 0] <- 0

   par.A$Loss.Units[par.A$Gain.Units < 0] <- -par.A$Gain.Units[
     par.A$Gain.Units < 0]
   par.A$Gain.Units[par.A$Gain.Units < 0] <- 0

   par.A$Loss.SF[par.A$Gain.SF < 0] <- -par.A$Gain.SF[
     par.A$Gain.SF < 0]
   par.A$Gain.SF[par.A$Gain.SF < 0] <- 0

# 7.8 Clean up Fields ----------------------------------------------------------

   par.A$ChangeSum <- NULL
   par.A$MinusBldg <- NULL
   par.A$Rebuild <- NULL
   par.A$PlusUnits <- NULL
   par.A$MinusUnits <- NULL
   par.A$PlusBldg <- NULL

################################################################################
# 8.0 Commercial Structural Consistency  ---------------------------------------

 # 8.1 Label the individual changes to the structure(s) ------------------------

  pc.C$ChangeSum <- 0
  pc.C$Rebuild <- 0
  pc.C$Rebuild[pc.C$E.YearBuilt > Beg.Year 
             & abs((pc.C$B.SF - pc.C$E.SF)/pc.C$B.SF) > .1 
             & pc.C$B.NbrBldgs == pc.C$E.NbrBldgs ] <- 1

  pc.C$PlusBldg <- 0
  pc.C$PlusBldg[pc.C$B.NbrBldgs < pc.C$E.NbrBldgs ] <- 1

  pc.C$MinusBldg <- 0
  pc.C$MinusBldg[pc.C$B.NbrBldgs > pc.C$E.NbrBldgs ] <- 1

  pc.C$AddSize <- 0
  pc.C$AddSize[pc.C$B.NbrBldgs == pc.C$E.NbrBldgs 
             & (pc.C$B.SF - pc.C$E.SF)/pc.C$B.SF < -.1 ] <- 1

  pc.C$MinusSize <- 0
  pc.C$MinusSize[pc.C$B.NbrBldgs == pc.C$E.NbrBldgs 
               & (pc.C$B.SF - pc.C$E.SF)/pc.C$B.SF > .1 ] <- 1

  pc.C$PlusUnits <- 0
  pc.C$PlusUnits[(pc.C$B.Units + pc.C$B.aUnits) > 
                   (pc.C$E.Units + pc.C$E.aUnits)] <- 1

  pc.C$MinusUnits <- 0
  pc.C$MinusUnits[(pc.C$B.Units + pc.C$B.aUnits) < 
                     (pc.C$E.Units + pc.C$E.aUnits)] <- 1
        
 # 8.2 Sum up Changes ----------------------------------------------------------

  pc.C$ChangeSum <- rowSums(pc.C[,(which(
    colnames(pc.C) =="ChangeSum")+1):dim(pc.C)[2]])

 # 8.3 Divide Off Those with only Assessor's Updates ---------------------------

  pc.C.cons <- pc.C[pc.C$ChangeSum==0, ]
  pc.C.cons$Use.Chng <- "None"
  pc.C.cons$Chng.Type <- "A.C."
  pc.C.cons$Chng.Time <- 0
  pc.C.cons$Loss.Units <- 0
  pc.C.cons$Loss.SF <- 0
  pc.C.cons$Gain.Units <- 0
  pc.C.cons$Gain.SF <- 0
  
  # 8.4 Split off Those with Real Changes --------------------------------------

  nc.C <- pc.C[pc.C$ChangeSum!=0, ]

 # 8.5 Set up Variables --------------------------------------------------------

  nc.C$Chng.Type <- "X"
  nc.C$Chng.Time <- 0
  nc.C$Use.Chng <- "None"
  nc.C$Loss.Units <- 0
  nc.C$Loss.SF <- 0
  nc.C$Gain.Units <- 0
  nc.C$Gain.SF <- 0
  
  # 8.5.1 Label the Rebuilds
  reb <- which(nc.C$Rebuild == 1)
  nc.C$Chng.Type[reb] <- "Redev"
  nc.C$Chng.Time[reb] <- nc.C$E.YearBuilt[reb]
  nc.C$Loss.Units[reb] <- nc.C$B.Units[reb] + nc.C$B.aUnits[reb]
  nc.C$Loss.SF[reb] <- nc.C$B.SF[reb] + nc.C$B.aSF[reb]
  nc.C$Gain.Units[reb] <- nc.C$E.Units[reb] + nc.C$E.aUnits[reb]
  nc.C$Gain.SF[reb] <- nc.C$E.SF[reb] + nc.C$E.aSF[reb]
    
  # 8.5.2 Label the Addition of SF
  sfg <- which(nc.C$AddSize==1)
  nc.C$Chng.Type[sfg] <- "Exp"
  nc.C$Chng.Time[sfg] <- yearFix(CchangeFinder(
                           nc.C[sfg,], "BldgGrossSqFt", Beg.Year,End.Year))
  nc.C$Gain.Units[sfg] <- (nc.C$E.Units[sfg] + nc.C$E.aUnits[sfg]) - 
                           (nc.C$B.Units[sfg] + nc.C$B.aUnits[sfg])
  nc.C$Gain.SF[sfg] <- (nc.C$E.SF[sfg] + nc.C$E.aSF[sfg]) - 
                           (nc.C$B.SF[sfg] + nc.C$B.aSF[sfg])
  
  # 8.5.3 Label the Loss of SF
  sfl <- which(nc.C$MinusSize==1)
  nc.C$Chng.Type[sfl] <- "Exp"
  nc.C$Chng.Time[sfl] <- yearFix(CchangeFinder(
                          nc.C[sfl,], "BldgGrossSqFt", Beg.Year,End.Year))
  nc.C$Loss.Units[sfl] <- (nc.C$B.Units[sfl] + nc.C$B.aUnits[sfl]) - 
                          (nc.C$E.Units[sfl] + nc.C$E.aUnits[sfl])
  nc.C$Loss.SF[sfl] <- (nc.C$B.SF[sfl] + nc.C$B.aSF[sfl]) - 
                          (nc.C$E.SF[sfl] + nc.C$E.aSF[sfl])
  
  # 8.5.4 Label the Addition of Units
  ung <- which(nc.C$AddUnits==1)
  nc.C$Chng.Type[ung] <- "Exp"
  nc.C$Chng.Time[ung] <- yearFix(CchangeFinder(
                         nc.C[ung,], "Units", Beg.Year,End.Year))
  nc.C$Gain.Units[ung] <- (nc.C$E.Units[ung] + nc.C$E.aUnits[ung]) - 
                          (nc.C$B.Units[ung] + nc.C$B.aUnits[ung])
  nc.C$Gain.SF[ung] <- (nc.C$E.SF[ung] + nc.C$E.aSF[ung]) - 
                          (nc.C$B.SF[ung] + nc.C$B.aSF[ung])
  
  # 8.5.5 Label the Loss of Units
  unl <- which(nc.C$MinusUnits==1)
  nc.C$Chng.Type[unl] <- "Exp"
  nc.C$Chng.Time[unl] <- yearFix(CchangeFinder(
                         nc.C[unl,], "Units", Beg.Year,End.Year))
  nc.C$Gain.Units[unl] <- (nc.C$B.Units[unl] + nc.C$B.aUnits[unl]) - 
                          (nc.C$E.Units[unl] + nc.C$E.aUnits[unl])
  nc.C$Gain.SF[unl] <- (nc.C$B.SF[unl] + nc.C$B.aSF[unl]) - 
                          (nc.C$E.SF[unl] + nc.C$E.aSF[unl])
  
  # 8.5.6 Label the Gain of Buildings
  bgg <- which(nc.C$PlusBldg==1)
  nc.C$Chng.Type[bgg] <- "New"
  nc.C$Chng.Time[bgg] <- yearFix(CchangeFinder(
               nc.C[bgg,], "BldgNbr", Beg.Year,End.Year))
  nc.C$Gain.Units[bgg] <- (nc.C$E.Units[bgg] + nc.C$E.aUnits[bgg]) - 
                          (nc.C$B.Units[bgg] + nc.C$B.aUnits[bgg])
  nc.C$Gain.SF[bgg] <- (nc.C$E.SF[bgg] + nc.C$E.aSF[bgg]) - 
                          (nc.C$B.SF[bgg] + nc.C$B.aSF[bgg])
  
  # 8.5.7 Label the Gain of Buildings
  bgg <- which(nc.C$PlusBldg==1)
  nc.C$Chng.Type[bgg] <- "New"
  nc.C$Chng.Time[bgg] <- yearFix(CchangeFinder(
    nc.C[bgg,], "BldgNbr", Beg.Year,End.Year))
  nc.C$Gain.Units[bgg] <- (nc.C$E.Units[bgg] + nc.C$E.aUnits[bgg]) - 
    (nc.C$B.Units[bgg] + nc.C$B.aUnits[bgg])
  nc.C$Gain.SF[bgg] <- (nc.C$E.SF[bgg] + nc.C$E.aSF[bgg]) - 
    (nc.C$B.SF[bgg] + nc.C$B.aSF[bgg])
  
  # 8.5.8 Label the Loss of Buildings
  bgl <- which(nc.C$MinusBldg==1)
  nc.C$Chng.Type[bgl] <- "Demo"
  nc.C$Chng.Time[bgl] <- yearFix(CchangeFinder(
                            nc.C[bgl,], "BldgNbr", Beg.Year,End.Year))
  nc.C$Loss.Units[bgl] <- (nc.C$B.Units[bgl] + nc.C$B.aUnits[bgl]) - 
                            (nc.C$E.Units[bgl] + nc.C$E.aUnits[bgl])
  nc.C$Loss.SF[bgl] <- (nc.C$B.SF[bgl] + nc.C$B.aSF[bgl]) - 
                            (nc.C$E.SF[bgl] + nc.C$E.aSF[bgl])
  
# 8.6 Re-combine pc.C ----------------------------------------------------------

  par.C <- rbind(pc.C.cons, nc.C)
  
# 8.7 Check for negatives ------------------------------------------------------

  par.C$Gain.Units[par.C$Loss.Units < 0] <- -par.C$Loss.Units[
    par.C$Loss.Units < 0]
  par.C$Loss.Units[par.C$Loss.Units < 0] <- 0
  
  par.C$Gain.SF[par.C$Loss.SF < 0] <- -par.C$Loss.SF[
    par.C$Loss.SF < 0]
  par.C$Loss.SF[par.C$Loss.SF < 0] <- 0
  
  par.C$Loss.Units[par.C$Gain.Units < 0] <- -par.C$Gain.Units[
    par.C$Gain.Units < 0]
  par.C$Gain.Units[par.C$Gain.Units < 0] <- 0
  
  par.C$Loss.SF[par.C$Gain.SF < 0] <- -par.C$Gain.SF[
    par.C$Gain.SF < 0]
  par.C$Gain.SF[par.C$Gain.SF < 0] <- 0
  
# 8.8 Clean up Fields ----------------------------------------------------------

  par.C$ChangeSum <- NULL
  par.C$MinusBldg <- NULL
  par.C$Rebuild <- NULL
  par.C$PlusUnits <- NULL
  par.C$MinusUnits <- NULL   
  par.C$AddSize <- NULL
  par.C$MinusSize <- NULL 
  par.C$PlusBldg <- NULL 
    
################################################################################  
# 9.0 Recombine ----------------------------------------------------------------
   
 if(dim(pc.K)[1] == 0){
   all.RPyes <- rbind(par.R, par.A, par.C)
 }

 if(dim(pc.K)[1] > 0){
   all.RPyes <- rbind(par.R, par.A, par.K, par.C)
 }
    
################################################################################
# 10.0 Divide up the RP No Properties    ---------------------------------------

# 10.1 Filter out Invalid Topology Types ---------------------------------------

  inv <- which(NC.RPTno$Topo.Type == "Relocated" |
                NC.RPTno$Topo.Type == "Change - Rel" |
                 NC.RPTno$Topo.Type == "Consistent - Rel" |
                  NC.RPTno$Topo.Type == "Change - MP")

  inv.RPno <- NC.RPTno[inv, ]
  val.RPno <- NC.RPTno[-inv, ]

# 10.2 Divide up based on Topology R Type (A, B or E) --------------------------
  
  val.A <- val.RPno[val.RPno$PType == "A", ]
  val.B <- val.RPno[val.RPno$PType == "B", ]
  val.E <- val.RPno[val.RPno$PType == "E", ]

################################################################################
# 11.0 Split A Parcels based on RT and PT Consistencies ------------------------

# 11.1 Split up based on PT Consistency ----------------------------------------
  
  tc <- which(val.A$Topo.Type == "Consistent" |
                val.A$Topo.Type == "Consistent - MP" |
                 val.A$Topo.Type == "Lot Adjustment")

  valA.Cons <- val.A[tc, ]
  valA.NC <- val.A[-tc, ]
  
# 11.2 Split by RT Consistency  ------------------------------------------------
  
  Arc <- valA.NC[valA.NC$B.Class == valA.NC$E.Class,]
  Anc <- valA.NC[valA.NC$B.Class != valA.NC$E.Class,]

################################################################################
# 12.0 Deal with A Parcels PT Consistent, RT/Str Inconsistent -------------

# 12.1 Split up by record type -------------------------------------------------

  valAC.R <- valA.Cons[valA.Cons$B.Class == "R", ]
  valAC.A <- valA.Cons[valA.Cons$B.Class == "A" | valA.Cons$B.Class == "C.A", ]
  valAC.K <- valA.Cons[valA.Cons$B.Class == "K", ]
  valAC.C <- valA.Cons[valA.Cons$B.Class == "C", ]
  valAC.V <- valA.Cons[valA.Cons$B.Class == "V", ]
  valAC.X <- valA.Cons[valA.Cons$B.Class == "X", ]

# 12.2 Deal with Res Parcels ------------------------------------------------------

 # 12.2.2 Set Change.Type and  Use Change 
  valAC.R$Chng.Type <- "X"
  valAC.R$Use.Chng <- "R_to_C"
  valAC.R$Use.Chng[valAC.R$E.Class == "A" |
                     valAC.R$E.Class == "C.A" ] <- "R_to_A"
  valAC.R$Use.Chng[valAC.R$E.Class == "V"] <- "R_to_V"
  valAC.R$Use.Chng[valAC.R$E.Class == "K"] <- "R_to_K"
  valAC.R$Use.Chng[valAC.R$E.Class == "X"] <- "R_to_X"

 # 12.2.3 Set Loss SF and Loss Units 
  valAC.R$Loss.SF <- valAC.R$B.SF + valAC.R$B.aSF
  valAC.R$Loss.Units <- valAC.R$B.Units + valAC.R$B.aUnits

 # 12.2.4 Set Gain Units
  valAC.R$Gain.Units <- 0
  valAC.R$Gain.SF <- 0
  valAC.R$Chng.Time <- 0

 # 12.2.5 Work on Res to Apt Changes
  r2a <- which(valAC.R$Use.Chng == "R_to_A")
  if(length(r2a)>0){
    valAC.R$Chng.Type[r2a] <- "Redev"
    valAC.R$Gain.Units[r2a] <- (valAC.R$E.Units[r2a] 
                                 + valAC.R$E.aUnits[r2a])
    valAC.R$Gain.SF[r2a] <- (valAC.R$E.SF[r2a] 
                                + valAC.R$E.aSF[r2a])
    valAC.R$Chng.Time[r2a] <- valAC.R$E.YearBuilt[r2a]

  # 12.2.5.1 Label Change Type
    valAC.R$Chng.Type[valAC.R$Use.Chng == "R_to_A" & 
                      valAC.R$Chng.Time < Beg.Year] <- "Conv"
    
    valAC.R$Chng.Type[valAC.R$Chng.Type == "Conv" & 
                        valAC.R$Gain.Units == valAC.R$Loss.Units] <- "A.C."
    
  # 12.2.5.2 Label Chng Time for Conversions  
    conv <- which(valAC.R$Chng.Type == "Conv" & valAC.R$Use.Chng == "R_to_A")
    valAC.R$Chng.Time[conv] <- yearFix(
            PchangeFinder(valAC.R[conv, ],"PresentUse", Beg.Year, End.Year))
  }

  # 12.2.6 Work on Res to Comm Changes
  r2c <- which(valAC.R$Use.Chng == "R_to_C")
  if(length(r2c) > 0){
    valAC.R$Chng.Type[r2c] <- "Redev"
    valAC.R$Gain.SF[r2c] <- valAC.R$E.SF[r2c] 
    valAC.R$Gain.Units[r2c] <- (valAC.R$E.Units[r2c] + valAC.R$E.aUnits[r2c]) 
    valAC.R$Chng.Time[r2c] <- valAC.R$E.YearBuilt[r2c]
    
  # 12.2.6.1 Label Change Type
    valAC.R$Chng.Type[valAC.R$Use.Chng == "R_to_C" & 
                        valAC.R$Chng.Time < Beg.Year] <- "Conv"
  
  # 12.2.6.2 Label Chng Time for Conversions  
    conv <- which(valAC.R$Chng.Type == "Conv" & valAC.R$Use.Chng == "R_to_C")
    valAC.R$Chng.Time[conv] <- yearFix(
        PchangeFinder(valAC.R[conv,],"PresentUse", Beg.Year, End.Year))
  }

  # 12.2.7 Work on Res to Condo Changes
  r2k <- which(valAC.R$Use.Chng == "R_to_K")
  if(length(r2k)>0){  
    valAC.R$Chng.Type[r2k] <- "Redev"
    valAC.R$Gain.SF[r2k] <- valAC.R$E.SF[r2k] 
    valAC.R$Gain.Units[r2k] <- (valAC.R$E.Units[r2k] + valAC.R$E.aUnits[r2k]) 
    valAC.R$Chng.Time[r2k] <- valAC.R$E.YearBuilt[r2k]
    
    # 12.2.7.1 Label Change Type
    valAC.R$Chng.Type[valAC.R$Use.Chng == "R_to_K" & 
                        valAC.R$Chng.Time < Beg.Year] <- "Conv"
    
    # 12.2.7.2 Label Chng Time for Conversions  
    conv <- which(valAC.R$Chng.Type == "Conv" & valAC.R$Use.Chng == "R_to_K")
    valAC.R$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.R[conv, ], "PresentUse", Beg.Year, End.Year))
  }
    
  # 12.2.8 Work on Res to Vacant Changes
  r2v <- which(valAC.R$Use.Chng == "R_to_V")
  if(length(r2v) > 0){  
    valAC.R$Chng.Time[r2v] <- yearFix(PchangeFinder(valAC.R[r2v, ]
                            ,"PresentUse", Beg.Year, End.Year))     
    valAC.R$Chng.Type[r2v] <- "Demo"
  }

  # 12.2.9 Work on Res to X Changes
  r2x <- which(valAC.R$Use.Chng == "R_to_X")
  if(length(r2x)>0){  
      valAC.R$Chng.Time[r2x] <- yearFix(PchangeFinder(valAC.R[r2x, ]
                    ,"PresentUse", Beg.Year, End.Year))          
      valAC.R$Chng.Type[r2x] <- "A.C."    
  }

# 12.3 Deal with Apt parcels ---------------------------------------------------

  # 12.3.2 Set Change Type
  valAC.A$Use.Chng <- "A_to_R"
  valAC.A$Use.Chng[valAC.A$E.Class == "A" | 
                      valAC.A$E.Class == "C.A" ] <- "None"
  valAC.A$Use.Chng[valAC.A$E.Class == "C"] <- "A_to_C"
  valAC.A$Use.Chng[valAC.A$E.Class == "K"] <- "A_to_K"
  valAC.A$Use.Chng[valAC.A$E.Class == "V"] <- "A_to_V"
  valAC.A$Use.Chng[valAC.A$E.Class == "X"] <- "A_to_X"

  # 12.3.3 Set Loss Units and Loss Amount
  valAC.A$Loss.SF <- valAC.A$B.SF + valAC.A$B.aSF
  valAC.A$Loss.Units <- valAC.A$B.Units + valAC.A$B.aUnits 

  # 12.3.4 Set Gain Units based on Change Type
  valAC.A$Gain.SF <- 0
  valAC.A$Gain.Units <- 0
  valAC.A$Chng.Time <- 0
  valAC.A$Chng.Type <- "X"
  valAC.A$Chng.Type[valAC.A$Use.Chng == "None"] <- "A.C."

  # 12.3.5 Work of Apt to Res Changes
  a2r <- which(valAC.A$Use.Chng == "A_to_R")
  if(length(a2r)>0){
    valAC.A$Chng.Type[a2r] <- "Redev"
    valAC.A$Gain.Units[a2r] <- valAC.A$E.Units[a2r] + valAC.A$E.aUnits[a2r]
    valAC.A$Gain.SF[a2r] <- valAC.A$E.SF[a2r] + valAC.A$E.aSF[a2r]
    valAC.A$Chng.Time[a2r] <- valAC.A$E.YearBuilt[a2r]
  
   # 12.3.5.1 Label Change Type
    valAC.A$Chng.Type[valAC.A$Use.Chng == "A_to_R" & 
                        valAC.A$Chng.Time < Beg.Year] <- "Conv"
    valAC.A$Chng.Type[valAC.A$Use.Chng == "A_to_R" & 
                        valAC.A$Chng.Time < Beg.Year &
                        valAC.A$Loss.Units == valAC.A$Gain.Units] <- "A.C."
        
    # 12.3.7.2 Label Chng Time for Conversions  
    conv <- which(valAC.A$Chng.Type == "Conv" & valAC.A$Use.Chng == "A_to_R")
    valAC.A$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.A[conv,],"PresentUse", Beg.Year, End.Year))
  }

  # 12.3.6 Work on Apt to Comm Changes
  a2c <- which(valAC.A$Use.Chng == "A_to_C")
  if(length(a2c) > 0){
    valAC.A$Chng.Type[a2c] <- "Redev"
    valAC.A$Gain.SF[a2c] <- valAC.A$E.SF[a2c] + valAC.A$E.aSF[a2c] 
    valAC.A$Gain.Units[a2c] <- (valAC.A$E.Units[a2c] + valAC.A$E.aUnits[a2c]) 
    valAC.A$Chng.Time[a2c] <- valAC.A$E.YearBuilt[a2c]
    
    # 12.3.6.1 Label Change Type
    valAC.A$Chng.Type[valAC.A$Use.Chng == "A_to_C" & 
                        valAC.A$Chng.Time < Beg.Year] <- "Conv"
    valAC.A$Chng.Type[valAC.A$Use.Chng == "A_to_C" & 
                        valAC.A$Chng.Time < Beg.Year &
                        valAC.A$Loss.Units == valAC.A$Gain.Units & 
                        valAC.A$Loss.SF == valAC.A$Gain.SF]  <- "A.C."
    
    # 12.3.6.2 Label Chng Time for Conversions  
    conv <- which(valAC.A$Chng.Type == "Conv" & valAC.A$Use.Chng == "A_to_C")
    valAC.A$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.A[conv,], "PresentUse", Beg.Year, End.Year))
  }

  # 12.3.7 Work on Apt to Condo Changes
  a2k <- which(valAC.A$Use.Chng == "A_to_K")
  if(length(a2k)>0){  
    valAC.A$Chng.Type[a2k] <- "Redev"
    valAC.A$Gain.SF[a2k] <- valAC.A$E.SF[a2k] + valAC.A$E.aSF[a2k] 
    valAC.A$Gain.Units[a2k] <- (valAC.A$E.Units[a2k] + valAC.A$E.aUnits[a2k]) 
    valAC.A$Chng.Time[a2k] <- valAC.A$E.YearBuilt[a2k]
    
    # 12.3.7.1 Label Change Type
    valAC.A$Chng.Type[valAC.A$Use.Chng == "A_to_K" & 
                        valAC.A$Chng.Time < Beg.Year] <- "Conv"
    
    # 12.3.7.2 Label Chng Time for Conversions  
    conv <- which(valAC.A$Chng.Type == "Conv" & valAC.A$Use.Chng == "A_to_K")
    valAC.A$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.A[conv, ], "PresentUse", Beg.Year, End.Year))
  }

 # 12.3.8 Work on Apt to Vacant Changes
  a2v <- which(valAC.A$Use.Chng == "A_to_V")
  if(length(a2v)>0){  
    valAC.A$Chng.Type[a2v] <- "Demo"
    valAC.A$Chng.Time[a2v] <- yearFix(
      PchangeFinder(valAC.A[a2v, ], "PresentUse", Beg.Year, End.Year))
  }

 # 12.3.9 Work on Apt to X Changes
  a2x <- which(valAC.A$Use.Chng == "A_to_X")
  if(length(a2x)>0){  
    valAC.A$Chng.Time[a2x] <- yearFix(PchangeFinder(valAC.A[a2x, ]
                                    ,"PresentUse", Beg.Year, End.Year))          
    valAC.A$Chng.Type[a2x] <- "A.C."    
  }

# 12.4 Deal with Condo parcels -------------------------------------------------

if(dim(valAC.K)[1] > 0){
  
 # 12.4.1 Set Change Type
  valAC.K$Use.Chng <- "K_to_R"
  valAC.K$Use.Chng[valAC.K$E.Class == "K" | 
                   valAC.K$E.Class == "C.K" ] <- "None"
  valAC.K$Use.Chng[valAC.K$E.Class == "C"] <- "K_to_C"
  valAC.K$Use.Chng[valAC.K$E.Class == "A"] <- "K_to_A"
  valAC.K$Use.Chng[valAC.K$E.Class == "V"] <- "K_to_V"
  valAC.K$Use.Chng[valAC.K$E.Class == "X"] <- "K_to_V"

 # 12.4.2 Set Loss Units and Loss Amount
  valAC.K$Loss.SF <- valAC.K$B.SF + valAC.K$B.aSF
  valAC.K$Loss.Units <- valAC.K$B.Units + valAC.K$B.aUnits 

 # 12.4.3 Set Gain Units based on Change Type 
  valAC.K$Gain.SF <- 0
  valAC.K$Gain.Units <- 0
  valAC.K$Chng.Time <- 0
  valAC.K$Chng.Type <- "X"
  valAC.K$Chng.Type[valAC.K$Use.Chng == "None"] <- "A.C."
  
 # 12.4.4 Work of Condo to Res Changes
  k2r <- which(valAC.K$Use.Chng == "K_to_R")
  if(length(k2r) > 0){
    valAC.K$Chng.Type[k2r] <- "Redev"
    valAC.K$Gain.Units[k2r] <- valAC.K$E.Units[k2r] + valAC.K$E.aUnits[k2r]
    valAC.K$Gain.SF[k2r] <- valAC.K$E.SF[k2r] + valAC.K$E.aSF[k2r]
    valAC.K$Chng.Time[k2r] <- valAC.K$E.YearBuilt[k2r]
  
   # 12.4.4.1 Label Change Type
    valAC.K$Chng.Type[valAC.K$Use.Chng == "K_to_R" & 
                      valAC.K$Chng.Time < Beg.Year] <- "Conv"
    valAC.K$Chng.Type[valAC.K$Use.Chng == "K_to_R" & 
                      valAC.K$Chng.Time < Beg.Year &
                      valAC.K$Loss.Units == valAC.K$Gain.Units] <- "A.C."
  
   # 12.4.4.2 Label Chng Time for Conversions  
    conv <- which(valAC.K$Chng.Type == "Conv" & valAC.K$Use.Chng == "K_to_R")
    valAC.K$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.K[conv,], "PresentUse", Beg.Year, End.Year))
  }

# 12.4.5 Work on Condo to Comm Changes
  k2c <- which(valAC.K$Use.Chng == "K_to_C")
  if(length(k2c)>0){
    valAC.K$Chng.Type[k2c] <- "Redev"
    valAC.K$Gain.SF[k2c] <- valAC.K$E.SF[k2c] + valAC.K$E.aSF[k2c] 
    valAC.K$Gain.Units[k2c] <- (valAC.K$E.Units[k2c] + valAC.K$E.aUnits[k2c]) 
    valAC.K$Chng.Time[k2c] <- valAC.K$E.YearBuilt[k2c]
  
  # 12.4.5.1 Label Change Type
    valAC.K$Chng.Type[valAC.K$Use.Chng == "K_to_C" & 
                      valAC.K$Chng.Time < Beg.Year] <- "Conv"
    valAC.K$Chng.Type[valAC.K$Use.Chng == "K_to_C" & 
                      valAC.K$Chng.Time < Beg.Year &
                      valAC.K$Loss.Units == valAC.K$Gain.Units & 
                      valAC.K$Loss.SF == valAC.K$Gain.SF]  <- "A.C."
  
  # 12.4.5.2 Label Chng Time for Conversions  
    conv <- which(valAC.K$Chng.Type == "Conv" & valAC.K$Use.Chng == "K_to_C")
    valAC.K$Chng.Time[conv] <- yearFix(
       PchangeFinder(valAC.K[conv, ], "PresentUse", Beg.Year, End.Year))
  }

 # 12.4.6 Work on Condo to Apt Changes
  k2a <- which(valAC.K$Use.Chng == "K_to_A")
  if(length(a2k)>0){  
    valAC.K$Chng.Type[k2a] <- "Redev"
    valAC.K$Gain.SF[k2a] <- valAC.K$E.SF[k2a] + valAC.K$E.aSF[k2a] 
    valAC.K$Gain.Units[k2a] <- (valAC.K$E.Units[k2a] + valAC.K$E.aUnits[k2a]) 
    valAC.K$Chng.Time[k2a] <- valAC.K$E.YearBuilt[k2a]
  
  # 12.4.6.1 Label Change Type
    valAC.K$Chng.Type[valAC.K$Use.Chng == "K_to_A" & 
                      valAC.K$Chng.Time < Beg.Year] <- "Conv"
  
  # 12.4.6.2 Label Chng Time for Conversions  
    conv <- which(valAC.K$Chng.Type == "Conv" & valAC.K$Use.Chng == "A_to_K")
    valAC.K$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.K[conv,],"PresentUse", Beg.Year, End.Year))
  }

 # 12.4.7 Work on Condo to Vacant Changes
  k2v <- which(valAC.K$Use.Chng == "K_to_V")
  if(length(k2v)>0){  
    valAC.K$Chng.Type[k2v] <- "Demo"
    valAC.K$Chng.Time[k2v] <- yearFix(
      PchangeFinder(valAC.K[k2v, ], "PresentUse", Beg.Year, End.Year))
  }

# 12.4.8 Work on Condo to X Changes
  k2x <- which(valAC.K$Use.Chng == "K_to_X")
  if(length(k2x)>0){  
    valAC.K$Chng.Time[k2x] <- yearFix(
      PchangeFinder(valAC.K[k2x,], "PresentUse", Beg.Year, End.Year))          
    valAC.K$Chng.Type[k2x] <- "A.C."    
  }
}

# 12.5 Deal with Commercial parcels --------------------------------------------

  # 12.5.2 Set Change Type
  valAC.C$Use.Chng <- "C_to_R"
  valAC.C$Use.Chng[valAC.C$E.Class == "A" | 
                    valAC.C$E.Class == "C.A" ] <- "C_to_A"
  valAC.C$Use.Chng[valAC.C$E.Class == "K"] <- "C_to_K"
  valAC.C$Use.Chng[valAC.C$E.Class == "V"] <- "C_to_V"
  valAC.C$Use.Chng[valAC.C$E.Class == "X"] <- "C_to_X"

  # 12.5.3 Set Loss Units and Loss Amount
  valAC.C$Loss.Units <- valAC.C$B.Units + valAC.C$B.aUnits
  valAC.C$Loss.SF <- valAC.C$B.SF + valAC.C$B.aSF

  # 12.5.4 Set Gain Units based on Change Type
  valAC.C$Gain.Units <- 0
  valAC.C$Gain.SF <- 0
  valAC.C$Chng.Time <- 0
  valAC.C$Chng.Type <- "X"
  valAC.C$Chng.Type[valAC.C$Use.Chng == "None"] <- "A.C."

  # 12.5.5 Work on Comm to Res Changes
  c2r <- which(valAC.C$Use.Chng == "C_to_R")
  if(length(c2r)>0){
    valAC.C$Chng.Type[c2r] <- "Redev"
    valAC.C$Gain.Units[c2r] <- valAC.C$E.Units[c2r] + valAC.C$E.aUnits[c2r]
    valAC.C$Gain.SF[c2r] <- valAC.C$E.SF[c2r] + valAC.C$E.aSF[c2r]
    valAC.C$Chng.Time[c2r] <- valAC.C$E.YearBuilt[c2r]
    
  # 12.5.5.1 Label Change Type
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_R" & 
                        valAC.C$Chng.Time < Beg.Year] <- "Conv"
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_R" & 
                        valAC.C$Chng.Time < Beg.Year &
                        valAC.C$Loss.Units == valAC.C$Gain.Units &
                        valAC.C$Loss.SF == valAC.C$Gain.SF] <- "A.C."
    
    # 12.5.5.2 Label Chng Time for Conversions  
    conv <- which(valAC.C$Chng.Type == "Conv" & valAC.C$Use.Chng == "C_to_R")
    valAC.C$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.C[conv,],"PresentUse", Beg.Year, End.Year))  
    }

  # 12.5.6 Work on Comm to Apt Changes
  c2a <- which(valAC.C$Use.Chng == "C_to_A")
  if(length(c2a)>0){
    valAC.C$Chng.Type[c2a] <- "Redev"
    valAC.C$Gain.Units[c2a] <- valAC.C$E.Units[c2a] + valAC.C$E.aUnits[c2a]
    valAC.C$Gain.SF[c2a] <- valAC.C$E.SF[c2a] + valAC.C$E.aSF[c2a]
    valAC.C$Chng.Time[c2a] <- valAC.C$E.YearBuilt[c2a]
  
    # 12.5.6.1 Label Change Type
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_A" & 
                        valAC.C$Chng.Time < Beg.Year] <- "Conv"
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_A" & 
                        valAC.C$E.NbrBldgs > valAC.C$B.NbrBldgs] <- "New"
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_A" & 
                        valAC.C$Chng.Time < Beg.Year &
                        valAC.C$E.Nbr.Bldgs == valAC.C$B.Nbr.Bldgs] <- "A.C."
    
    # 12.5.6.2 Label Chng Time for Conversions  
    conv <- which(valAC.C$Chng.Type == "Conv" & valAC.C$Use.Chng == "C_to_A")
    valAC.C$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.C[conv,],"PresentUse", Beg.Year, End.Year))  
    
  }

  # 12.5.7 Work on Comm to Condo Changes
  c2k <- which(valAC.C$Use.Chng == "C_to_K")
  if(length(c2k)>0){  
    valAC.C$Chng.Type[c2k] <- "Redev"
    valAC.C$Gain.Units[c2k] <- valAC.C$E.Units[c2k] + valAC.C$E.aUnits[c2k]
    valAC.C$Gain.SF[c2k] <- valAC.C$E.SF[c2k] + valAC.C$E.aSF[c2k]
    valAC.C$Chng.Time[c2k] <- valAC.C$E.YearBuilt[c2k]
    
    # 12.5.7.1 Label Change Type
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_K" & 
                        valAC.C$Chng.Time < Beg.Year] <- "Conv"
    valAC.C$Chng.Type[valAC.C$Use.Chng == "C_to_K" & 
                        valAC.C$Chng.Time < Beg.Year &
                        valAC.C$Loss.Units == valAC.C$Gain.Units &
                        valAC.C$Loss.SF == valAC.C$Gain.SF] <- "A.C."
    
    # 12.5.7.2 Label Chng Time for Conversions  
    conv <- which(valAC.C$Chng.Type == "Conv" & valAC.C$Use.Chng == "C_to_K")
    valAC.C$Chng.Time[conv] <- yearFix(
      PchangeFinder(valAC.C[conv,],"PresentUse", Beg.Year, End.Year))  
  }

  # 12.5.8 Work on Comm to Vacant Changes
  c2v <- which(valAC.C$Use.Chng == "C_to_V")
  if(length(c2v)>0){  
    valAC.C$Chng.Type[c2v] <- "Demo"
    valAC.C$Chng.Time[c2v] <- yearFix(PchangeFinder(
         valAC.C[c2v,], "PresentUse", Beg.Year, End.Year))     
  }

  # 12.5.9 Work on Comm to X Changes
  c2x <- which(valAC.C$Use.Chng == "C_to_X")
  if(length(c2x)>0){  
    valAC.C$Chng.Type[c2x] <- "A.C."
    valAC.C$Chng.Time[c2x] <- yearFix(changeFinder(valAC.C[c2x,]
                 ,"PresentUse",Beg.Year,End.Year))          
  }

# 12.6 Deal with Vacant parcels ------------------------------------------------

  # 12.6.2 Set Change Type
  valAC.V$Use.Chng <- "V_to_R"
  valAC.V$Use.Chng[valAC.V$E.Class=="A" | 
                    valAC.V$E.Class=="C.A" ] <- "V_to_A"
  valAC.V$Use.Chng[valAC.V$E.Class=="K"] <- "V_to_K"
  valAC.V$Use.Chng[valAC.V$E.Class=="C"] <- "V_to_C"
  valAC.V$Use.Chng[valAC.V$E.Class=="X"] <- "V_to_X"

  # 12.6.3 Set Loss Units and Loss Amount
  valAC.V$Loss.Units <- 0
  valAC.V$Loss.SF <- 0 

  # 12.6.4 Set Gain Units based on Change Type
  valAC.V$Gain.Units <- 0 
  valAC.V$Gain.SF <- 0
  valAC.V$Chng.Time <- 0
  valAC.V$Chng.Type <- "X"

 # 12.6.5 Work on Vac to Res Changes
  v2r <- which(valAC.V$Use.Chng == "V_to_R")
  if(length(v2r)>0){
    valAC.V$Chng.Type[v2r] <- "New"
    valAC.V$Gain.Units[v2r] <- valAC.V$E.Units[v2r] + valAC.V$E.aUnits[v2r]
    valAC.V$Gain.SF[v2r] <- valAC.V$E.SF[v2r] + valAC.V$E.aSF[v2r]
    valAC.V$Chng.Time[v2r] <- valAC.V$E.YearBuilt[v2r]
  
   # 12.6.5.1 Fix the assessor labeling issues
    valAC.V$Chng.Type[valAC.V$Use.Chng=="V_to_R" & 
                        valAC.V$Chng.Time < Beg.Year] <- "A.C."
  }

 # 12.6.6 Work on Vac to Apt Changes
  v2a <- which(valAC.V$Use.Chng == "V_to_A")
  if(length(v2a)>0){  
    valAC.V$Chng.Type[v2a] <- "New"
    valAC.V$Gain.Units[v2a] <- valAC.V$E.Units[v2a] + valAC.V$E.aUnits[v2a]
    valAC.V$Gain.SF[v2a] <- valAC.V$E.SF[v2a] + valAC.V$E.aSF[v2a]
    valAC.V$Chng.Time[v2a] <- valAC.V$E.YearBuilt[v2a]
  
    # 12.6.6.1 Assessor Changes
    valAC.V$Chng.Type[valAC.V$Use.Chng == "V_to_A" & 
                        valAC.V$Chng.Time < Beg.Year] <- "A.C."
  }

  # 12.6.7 Work on Vac to Condo Changes
  v2k <- which(valAC.V$Use.Chng == "V_to_K")
  if(length(v2k)>0){  
    valAC.V$Chng.Type[v2k] <- "New"
    valAC.V$Gain.Units[v2k] <- valAC.V$E.Units[v2k] + valAC.V$E.aUnits[v2k]
    valAC.V$Gain.SF[v2k] <- valAC.V$E.SF[v2k] + valAC.V$E.aSF[v2k]
    valAC.V$Chng.Time[v2k] <- valAC.V$E.YearBuilt[v2k]
  
    # 12.6.7.1 Assessor Changes
    valAC.V$Chng.Type[valAC.V$Use.Chng == "V_to_K" & 
                        valAC.V$Chng.Time < Beg.Year] <- "A.C."
  }

  # 12.6.8 Work on Vac to Comm Changes
  v2c <- which(valAC.V$Use.Chng == "V_to_C")
  if(length(v2c)>0){
    valAC.V$Chng.Type[v2c] <- "New"
    valAC.V$Gain.Units[v2c] <- valAC.V$E.Units[v2c] + valAC.V$E.aUnits[v2c]
    valAC.V$Gain.SF[v2c] <- valAC.V$E.SF[v2c] + valAC.V$E.aSF[v2c]
    valAC.V$Chng.Time[v2c] <- valAC.V$E.YearBuilt[v2c]
  
    # 12.6.8.1 Assessor Changes
    valAC.V$Chng.Type[valAC.V$Chng.Type=="V_to_C" & 
                        valAC.V$Chng.Time < Beg.Year] <- "A.C."
  }

  # 12.6.9 Work on Vac to X Changes
  v2x <- which(valAC.V$Use.Chng == "V_to_X")
  if(length(v2x)>0){  
    valAC.V$Chng.Type[v2x] <- "A.C."
    valAC.V$Chng.Time[v2x] <- yearFix(PchangeFinder(valAC.V[v2x,]
                     ,"PresentUse",Beg.Year,End.Year))          
  }

# 12.7 Deal with X parcels  ----------------------------------------------------

  # 12.7.2 Set Change Type
  valAC.X$Use.Chng <- "X_to_R"
  valAC.X$Use.Chng[valAC.X$E.Class=="A" | 
                    valAC.X$E.Class=="C.A" ] <- "X_to_A"
  valAC.X$Use.Chng[valAC.X$E.Class=="K"] <- "X_to_K"
  valAC.X$Use.Chng[valAC.X$E.Class=="C"] <- "X_to_C"
  valAC.X$Use.Chng[valAC.X$E.Class=="V"] <- "X_to_V"

  # 12.7.3 Set Loss Units and Loss Amount
  valAC.X$Loss.Units <- 0
  valAC.X$Loss.SF <- 0 

  # 12.7.4 Set Gain Units based on Change Type
  valAC.X$Gain.Units <- 0
  valAC.X$Gain.SF <- 0
  valAC.X$Chng.Time <- 0
  valAC.X$Chng.Type <- "X"

 # 12.7.5 Work on X to Res Changes
  x2r <- which(valAC.X$Use.Chng == "X_to_R")
  if(length(x2r)>0){
    valAC.X$Chng.Type[x2r] <- "New"
    valAC.X$Gain.Units[x2r] <- valAC.X$E.Units[x2r] + valAC.X$E.aUnits[x2r]
    valAC.X$Gain.SF[x2r] <- valAC.X$E.SF[x2r] + valAC.X$E.aSF[x2r]
    valAC.X$Chng.Time[x2r] <- valAC.X$E.YearBuilt[x2r]
  
  # 12.7.5.1 Fix the assessor labeling issues
  valAC.X$Chng.Type[valAC.X$Use.Chng == "X_to_R" & 
                      valAC.X$Chng.Time < Beg.Year] <- "A.C."
  }

 # 12.7.6 Work on X to Apt Changes
  x2a <- which(valAC.X$Use.Chng == "X_to_A")
  if(length(x2a)>0){  
    valAC.X$Gain.Units[x2a] <- valAC.X$E.Units[x2a] + valAC.X$E.aUnits[x2a]
    valAC.X$Gain.SF[x2a] <- valAC.X$E.SF[x2a] + valAC.X$E.aSF[x2a]
    valAC.X$Chng.Time[x2a] <- valAC.X$E.YearBuilt[x2a]
  
   # 12.7.6.1 Assessor Changes
    valAC.X$Chng.Type[valAC.X$Use.Chng == "X_to_A" & 
                      valAC.X$Chng.Time < Beg.Year] <- "A.C."
  }

 # 12.7.7 Work on X to Condo Changes
  x2k <- which(valAC.X$Use.Chng == "X_to_K")
  if(length(x2k)>0){  
    valAC.X$Chng.Type[x2k] <- "New"
    valAC.X$Gain.Units[x2k] <- valAC.X$E.Units[x2k] + valAC.X$E.aUnits[x2k]
    valAC.X$Gain.SF[x2k] <- valAC.X$E.SF[x2k] + valAC.X$E.aSF[x2k]
    valAC.X$Chng.Time[x2k] <- valAC.X$E.YearBuilt[x2k]
  
   # 12.7.7.1 Assessor Changes
    valAC.X$Chng.Type[valAC.X$Use.Chng == "X_to_K" & 
                      valAC.X$Chng.Time < Beg.Year] <- "A.C."
  }

 # 12.7.8 Work on X to Comm Changes
  x2c <- which(valAC.X$Use.Chng == "X_to_C")
  if(length(x2c)>0){
    valAC.X$Chng.Type[x2c] <- "New"
    valAC.X$Gain.Units[x2c] <- valAC.X$E.Units[x2c] + valAC.X$E.aUnits[x2c]
    valAC.X$Gain.SF[x2c] <- valAC.X$E.SF[x2c] + valAC.X$E.aSF[x2c]
    valAC.X$Chng.Time[x2c] <- valAC.X$E.YearBuilt[x2c]
  
   # 12.7.8.1 Assessor Changes
    valAC.X$Chng.Type[valAC.X$Use.Chng == "X_to_C" & 
                      valAC.X$Chng.Time < Beg.Year] <- "A.C."
  }

# 12.7.9 Work on Vac to X Changes
  x2v <- which(valAC.X$Use.Chng == "X_to_V")
  if(length(x2v)>0){  
    valAC.X$Chng.Type[x2v] <- "A.C."
    valAC.X$Chng.Time[x2v] <- yearFix(PchangeFinder(valAC.C[x2v,]
                                  ,"PresentUse",Beg.Year,End.Year))          
  }

# 12.8 Recombine ---------------------------------------------------------------

 if(dim(valAC.K)[1] > 0){
   all.PyRSn <- rbind(valAC.R, valAC.A, valAC.K, valAC.C, valAC.V, valAC.X)
 }

 if(dim(valAC.K)[1] == 0){
   all.PyRSn <- rbind(valAC.R, valAC.A, valAC.C, valAC.V, valAC.X)
 }

################################################################################
# 13.0 Deal with Val.A Parcels that are PT Incons but are RT Cons --------------

# 13.1 Split into record types
  Arc.R <- Arc[Arc$B.Class=="R",]
  Arc.A <- Arc[Arc$B.Class=="A" | Arc$B.Class == "C.A",]
  Arc.C <- Arc[Arc$B.Class=="C",]
  Arc.K <- Arc[Arc$B.Class=="K",]
  Arc.V <- Arc[Arc$B.Class=="V",]
  Arc.X <- Arc[Arc$B.Class=="X",]

# 13.2 Deal with Residential ---------------------------------------------------

 # 13.2.1 Set Change Type
  Arc.R$Use.Chng <- "None"
  Arc.R$Chng.Type <- "None"
  Arc.R$Chng.Type [Arc.R$E.YearBuilt <= Beg.Year &
                     (Arc.R$E.Units + Arc.R$E.aUnits) ==
                       (Arc.R$B.Units + Arc.R$B.aUnits) &
                     (Arc.R$E.SF + Arc.R$E.aSF) ==
                       (Arc.R$B.SF + Arc.R$B.aSF)] <- "None"

  Arc.R$Rebuild <- 0
  Arc.R$Rebuild[Arc.R$E.YearBuilt >= Beg.Year &
                 Arc.R$B.YearBuilt < Beg.Year] <- 1

  Arc.R$NewADU <- 0
  Arc.R$NewADU[Arc.R$E.YearBuilt < Beg.Year &
                Arc.R$E.aYB >= Beg.Year] <- 1

  Arc.R$ADUSplitOff <- 0
  Arc.R$ADUSplitOff[(Arc.R$Topo.Type=="Split - Retain" |
                       Arc.R$Topo.Type=="Lot Adj - Split") & 
                       Arc.R$E.YearBuilt < Beg.Year &
                      Arc.R$B.aSF > 0 & Arc.R$E.aSF == 0] <- 1

  Arc.R$AddUnits <- 0
  Arc.R$AddUnits[(Arc.R$E.Units + Arc.R$E.aUnits) >
                   (Arc.R$B.Units + Arc.R$B.aUnits) &
                    Arc.R$Rebuild == 0 &
                     Arc.R$NewADU == 0] <- 1

  Arc.R$MinusUnits <- 0
  Arc.R$MinusUnits[(Arc.R$E.Units + Arc.R$E.aUnits) <
                    (Arc.R$B.Units + Arc.R$B.aUnits)  &
                     Arc.R$Rebuild == 0 &
                      Arc.R$NewADU == 0] <- 1

 # 13.2.2 Set Loss Type and Amount
  Arc.R$Loss.Units <- 0
  Arc.R$Loss.SF <- 0
  Arc.R$Gain.Units <- 0
  Arc.R$Gain.SF <- 0
  Arc.R$Chng.Time <- 0

 # 13.2.3 Label the Rebuilds
  reb <- which(Arc.R$Rebuild == 1)
  Arc.R$Chng.Type[reb] <- "Redev"
  Arc.R$Chng.Time[reb] <- Arc.R$E.YearBuilt[reb]
  Arc.R$Loss.Units[reb] <- Arc.R$B.Units[reb] + Arc.R$B.aUnits[reb]
  Arc.R$Loss.SF[reb] <- Arc.R$B.SF[reb] + Arc.R$B.aSF[reb]
  Arc.R$Gain.Units[reb] <- Arc.R$E.Units[reb] + Arc.R$E.aUnits[reb]
  Arc.R$Gain.SF[reb] <- Arc.R$E.SF[reb] + Arc.R$E.aSF[reb]

 # 13.2.4 Label the New Accessory Dwelling Units
  nadu <- which(Arc.R$NewADU == 1 & Arc.R$Rebuild != 1)
  Arc.R$Chng.Type[nadu] <- "New"
  Arc.R$Chng.Time[nadu] <- Arc.R$E.aYB[nadu]
  Arc.R$Gain.Units[nadu] <- Arc.R$E.aUnits[nadu]
  Arc.R$Gain.SF[nadu] <- Arc.R$E.aSF[nadu]

 # 13.2.5 Label the Unit Losses 
  unl <- which(Arc.R$MinusUnits==1)
  Arc.R$Chng.Type[unl] <- "Dens"
  Arc.R$Chng.Time[unl] <- yearFix(
    RchangeFinder(Arc.R[unl, ],"NbrLivingUnits", Beg.Year, End.Year))
  Arc.R$Loss.Units[unl] <- (Arc.R$B.Units[unl] + Arc.R$B.aUnits[unl]) - 
    (Arc.R$E.Units[unl] + Arc.R$E.aUnits[unl])
  Arc.R$Loss.SF[unl] <- (Arc.R$B.SF[unl] + Arc.R$B.aSF[unl]) - 
    (Arc.R$E.SF[unl] + Arc.R$E.aSF[unl])

 # 13.2.6 Label the Unit Gains 
  ung <- which(Arc.R$PlusUnits==1)
  Arc.R$Chng.Type[ung] <- "Dens"
  Arc.R$Chng.Time[ung] <- yearFix(
    RchangeFinder(Arc.R[ung,], "NbrLivingUnits", Beg.Year, End.Year))
  Arc.R$Gain.Units[ung] <- (Arc.R$E.Units[ung] + Arc.R$E.aUnits[ung]) - 
    (Arc.R$B.Units[ung] + Arc.R$B.aUnits[ung])
  Arc.R$Gain.SF[ung] <- (Arc.R$E.SF[ung] + Arc.R$E.aSF[ung]) - 
  (Arc.R$E.SF[ung] + Arc.R$E.aSF[ung])

 # 13.2.7 Label the Split Off Accessory Dwelling Units
  adus <- which(Arc.R$ADUSplitOff == 1 & Arc.R$Rebuild != 1)
  Arc.R$Chng.Type[adus] <- "None"
#   Arc.R$Chng.Time[adus] <- yearFix(
#     RchangeFinder(Arc.R[unl, ],"NbrLivingUnits", Beg.Year, End.Year))
#   Arc.R$Loss.Units[nadu] <- Arc.R$E.aUnits[adus]
#   Arc.R$Loss.SF[nadu] <- Arc.R$E.aSF[adus]

 #13.2.8 Clean Up
 Arc.R$Rebuild <- NULL
 Arc.R$ADUSplitOff <- NULL
 Arc.R$AddUnits <- NULL
 Arc.R$MinusUnits <- NULL
 Arc.R$NewADU <- NULL

# 13.3 Label Apartment parcels -------------------------------------------------

  Arc.A$Use.Chng <- "None"
  Arc.A$Chng.Type <- "None"
  Arc.A$Chng.Type [Arc.A$E.YearBuilt <= Beg.Year &
                   (Arc.A$E.Units + Arc.A$E.aUnits) ==
                   (Arc.A$B.Units + Arc.A$B.aUnits) &
                   (Arc.A$E.SF + Arc.A$E.aSF) ==
                   (Arc.A$B.SF + Arc.A$B.aSF)] <- "None"

  Arc.A$Rebuild <- 0
  Arc.A$Rebuild[Arc.A$E.YearBuilt >= Beg.Year &
                Arc.A$B.YearBuilt < Beg.Year] <- 1

  Arc.A$AddUnits <- 0
  Arc.A$AddUnits[(Arc.A$E.Units + Arc.A$E.aUnits) >
                 (Arc.A$B.Units + Arc.A$B.aUnits) &
                 Arc.A$Rebuild == 0 &
                 Arc.A$NewADU == 0] <- 1

  Arc.A$MinusUnits <- 0
  Arc.A$MinusUnits[(Arc.A$E.Units + Arc.A$E.aUnits) <
                   (Arc.A$B.Units + Arc.A$B.aUnits)  &
                   Arc.A$Rebuild == 0 &
                   Arc.A$NewADU == 0] <- 1

  Arc.A$AddBldg <- 0
  Arc.A$AddBldg[Arc.A$E.NbrBldgs > Arc.A$B.NbrBldgs &
                 Arc.A$Rebuild == 0 & Arc.A$NewADU == 0] <- 1

  Arc.A$MinusBldg <- 0
  Arc.A$MinusBldg[Arc.A$E.NbrBldgs < Arc.A$B.NbrBldgs &
                 Arc.A$Rebuild == 0 & Arc.A$NewADU == 0] <- 1

 # 13.3.2 Set Change Type
  Arc.A$Loss.Units <- 0
  Arc.A$Loss.SF <- 0
  Arc.A$Gain.Units <- 0
  Arc.A$Gain.SF <- 0
  Arc.A$Chng.Time <- 0

 # 13.3.3 Label the Rebuilds
  reb <- which(Arc.A$Rebuild == 1)
  Arc.A$Chng.Type[reb] <- "Redev"
  Arc.A$Chng.Time[reb] <- Arc.A$E.YearBuilt[reb]
  Arc.A$Loss.Units[reb] <- Arc.A$B.Units[reb] + Arc.A$B.aUnits[reb]
  Arc.A$Loss.SF[reb] <- Arc.A$B.SF[reb] + Arc.A$B.aSF[reb]
  Arc.A$Gain.Units[reb] <- Arc.A$E.Units[reb] + Arc.A$E.aUnits[reb]
  Arc.A$Gain.SF[reb] <- Arc.A$E.SF[reb] + Arc.A$E.aSF[reb]

 # 13.3.4 Label the Unit Losses 
  unl <- which(Arc.A$MinusUnits == 1 & Arc.A$MinusBldg != 1)
  Arc.A$Chng.Type[unl] <- "Dens"
  Arc.A$Chng.Time[unl] <- yearFix(
    AchangeFinder(Arc.A[unl, ],"NbrUnits", Beg.Year, End.Year))
  Arc.A$Loss.Units[unl] <- (Arc.A$B.Units[unl] + Arc.A$B.aUnits[unl]) - 
    (Arc.A$E.Units[unl] + Arc.A$E.aUnits[unl])
  Arc.A$Loss.SF[unl] <- (Arc.A$B.SF[unl] + Arc.A$B.aSF[unl]) - 
    (Arc.A$E.SF[unl] + Arc.A$E.aSF[unl])

 # 13.3.5 Label the Unit Gains 
  ung <- which(Arc.A$AddUnits == 1 & Arc.A$AddBldg != 1)
  Arc.A$Chng.Type[ung] <- "Dens"
  Arc.A$Chng.Time[ung] <- yearFix(
    AchangeFinder(Arc.A[ung,], "NbrUnits", Beg.Year, End.Year))
  Arc.A$Gain.Units[ung] <- (Arc.A$E.Units[ung] + Arc.A$E.aUnits[ung]) - 
    (Arc.A$B.Units[ung] + Arc.A$B.aUnits[ung])
  Arc.A$Gain.SF[ung] <- (Arc.A$E.SF[ung] + Arc.A$E.aSF[ung]) - 
    (Arc.A$E.SF[ung] + Arc.A$E.aSF[ung])

 # 13.3.6 Label the Bldg Loss 
  bgl <- which(Arc.A$MinusBldg == 1)
  Arc.A$Chng.Type[bgl] <- "Demo"
  Arc.A$Chng.Time[bgl] <- yearFix(
    AchangeFinder(Arc.A[bgl, ],"NbrUnits", Beg.Year, End.Year))
  Arc.A$Loss.Units[bgl] <- (Arc.A$B.Units[bgl] + Arc.A$B.aUnits[bgl]) - 
    (Arc.A$E.Units[bgl] + Arc.A$E.aUnits[bgl])
  Arc.A$Loss.SF[bgl] <- (Arc.A$B.SF[bgl] + Arc.A$B.aSF[bgl]) - 
    (Arc.A$E.SF[bgl] + Arc.A$E.aSF[bgl])

 # 13.3.7 Label the Bldg Gain 
  bgg <- which(Arc.A$AddBldg == 1)
  Arc.A$Chng.Type[bgg] <- "New"
  Arc.A$Chng.Time[bgg] <- yearFix(
    AchangeFinder(Arc.A[bgg, ],"NbrUnits", Beg.Year, End.Year))
  Arc.A$Loss.Units[bgg] <- (Arc.A$B.Units[bgg] + Arc.A$B.aUnits[bgg]) - 
    (Arc.A$E.Units[bgg] + Arc.A$E.aUnits[bgg])
  Arc.A$Loss.SF[bgg] <- (Arc.A$B.SF[bgg] + Arc.A$B.aSF[bgg]) - 
    (Arc.A$E.SF[bgg] + Arc.A$E.aSF[bgg])

 # 13.3.8 Clean Up
  Arc.A$Rebuild <- NULL
  Arc.A$AddBldg <- NULL
  Arc.A$AddUnits <- NULL
  Arc.A$MinusUnits <- NULL
  Arc.A$MinusBldg <- NULL

# 13.4 Label Condo parcels -------------------------------------------------

if(dim(Arc.K)[1] > 0){
  
# 13.4.1 Setup  
  Arc.K$Use.Chng <- "None"
  Arc.K$Chng.Type <- "None"
  Arc.K$Chng.Type [Arc.K$E.YearBuilt <= Beg.Year &
                   (Arc.K$E.Units + Arc.K$E.aUnits) ==
                   (Arc.K$B.Units + Arc.K$B.aUnits) &
                   (Arc.K$E.SF + Arc.K$E.aSF) ==
                   (Arc.K$B.SF + Arc.K$B.aSF)] <- "None"

  Arc.K$Rebuild <- 0
  Arc.K$Rebuild[Arc.K$E.YearBuilt >= Beg.Year &
                Arc.K$B.YearBuilt < Beg.Year] <- 1

  Arc.K$AddUnits <- 0
  Arc.K$AddUnits[(Arc.K$E.Units + Arc.K$E.aUnits) >
                 (Arc.K$B.Units + Arc.K$B.aUnits) &
                 Arc.K$Rebuild == 0 &
                 Arc.K$NewADU == 0] <- 1

  Arc.K$MinusUnits <- 0
  Arc.K$MinusUnits[(Arc.K$E.Units + Arc.K$E.aUnits) <
                   (Arc.K$B.Units + Arc.K$B.aUnits)  &
                   Arc.K$Rebuild == 0 &
                   Arc.K$NewADU == 0] <- 1

  Arc.K$AddBldg <- 0
  Arc.K$AddBldg[Arc.K$E.NbrBldgs > Arc.K$B.NbrBldgs &
                Arc.K$Rebuild == 0 & Arc.K$NewADU == 0] <- 1

  Arc.K$MinusBldg <- 0
  Arc.K$MinusBldg[Arc.K$E.NbrBldgs < Arc.K$B.NbrBldgs &
                  Arc.K$Rebuild == 0 & Arc.K$NewADU == 0] <- 1

# 13.4.2 Set Change Type
  Arc.K$Loss.Units <- 0
  Arc.K$Loss.SF <- 0
  Arc.K$Gain.Units <- 0
  Arc.K$Gain.SF <- 0
  Arc.K$Chng.Time <- 0

# 13.4.3 Label the Rebuilds
  reb <- which(Arc.K$Rebuild == 1)
  Arc.K$Chng.Type[reb] <- "Redev"
  Arc.K$Chng.Time[reb] <- Arc.K$E.YearBuilt[reb]
  Arc.K$Loss.Units[reb] <- Arc.K$B.Units[reb] + Arc.K$B.aUnits[reb]
  Arc.K$Loss.SF[reb] <- Arc.K$B.SF[reb] + Arc.K$B.aSF[reb]
  Arc.K$Gain.Units[reb] <- Arc.K$E.Units[reb] + Arc.K$E.aUnits[reb]
  Arc.K$Gain.SF[reb] <- Arc.K$E.SF[reb] + Arc.K$E.aSF[reb]

# 13.4.4 Label the Unit Losses 
  unl <- which(Arc.K$MinusUnits == 1 & Arc.K$MinusBldg != 1)
  Arc.K$Chng.Type[unl] <- "Dens"
  Arc.K$Chng.Time[unl] <- yearFix(
    AchangeFinder(Arc.K[unl, ],"NbrUnits", Beg.Year, End.Year))
  Arc.K$Loss.Units[unl] <- (Arc.K$B.Units[unl] + Arc.K$B.aUnits[unl]) - 
    (Arc.K$E.Units[unl] + Arc.K$E.aUnits[unl])
  Arc.K$Loss.SF[unl] <- (Arc.K$B.SF[unl] + Arc.K$B.aSF[unl]) - 
    (Arc.K$E.SF[unl] + Arc.K$E.aSF[unl])

# 13.4.5 Label the Unit Gains 
  ung <- which(Arc.K$AddUnits == 1 & Arc.K$AddBldg != 1)
  Arc.K$Chng.Type[ung] <- "Dens"
  Arc.K$Chng.Time[ung] <- yearFix(
    AchangeFinder(Arc.K[ung,], "NbrUnits", Beg.Year, End.Year))
  Arc.K$Gain.Units[ung] <- (Arc.K$E.Units[ung] + Arc.K$E.aUnits[ung]) - 
    (Arc.K$B.Units[ung] + Arc.K$B.aUnits[ung])
  Arc.K$Gain.SF[ung] <- (Arc.K$E.SF[ung] + Arc.K$E.aSF[ung]) - 
    (Arc.K$E.SF[ung] + Arc.K$E.aSF[ung])

# 13.4.6 Label the Bldg Loss 
  bgl <- which(Arc.K$MinusBldg == 1)
  Arc.K$Chng.Type[bgl] <- "Demo"
  Arc.K$Chng.Time[bgl] <- yearFix(
    AchangeFinder(Arc.K[bgl, ],"NbrUnits", Beg.Year, End.Year))
  Arc.K$Loss.Units[bgl] <- (Arc.K$B.Units[bgl] + Arc.K$B.aUnits[bgl]) - 
    (Arc.K$E.Units[bgl] + Arc.K$E.aUnits[bgl])
  Arc.K$Loss.SF[bgl] <- (Arc.K$B.SF[bgl] + Arc.K$B.aSF[bgl]) - 
    (Arc.K$E.SF[bgl] + Arc.K$E.aSF[bgl])

# 13.4.7 Label the Bldg Gain 
  bgg <- which(Arc.K$AddBldg == 1)
  Arc.K$Chng.Type[bgg] <- "New"
  Arc.K$Chng.Time[bgg] <- yearFix(
    AchangeFinder(Arc.K[bgg, ],"NbrUnits", Beg.Year, End.Year))
  Arc.K$Loss.Units[bgg] <- (Arc.K$B.Units[bgg] + Arc.K$B.aUnits[bgg]) - 
    (Arc.K$E.Units[bgg] + Arc.K$E.aUnits[bgg])
  Arc.K$Loss.SF[bgg] <- (Arc.K$B.SF[bgg] + Arc.K$B.aSF[bgg]) - 
    (Arc.K$E.SF[bgg] + Arc.K$E.aSF[bgg])

 # 13.4.8 Clean Up
 Arc.K$Rebuild <- NULL
 Arc.K$AddBldg <- NULL
 Arc.K$AddUnits <- NULL
 Arc.K$MinusUnits <- NULL
 Arc.K$MinusBldg <- NULL
    
 }  

# 13.5 Label Commericial Propeties ---------------------------------------------

 # 13.5.1 Set Change Type
  Arc.C$Use.Chng <- "None"
  Arc.C$Chng.Type <- "None"
  Arc.C$Chng.Time <- 0
  Arc.C$Chng.Type [Arc.C$E.YearBuilt <= Beg.Year &
                   (Arc.C$E.Units + Arc.C$E.aUnits) ==
                   (Arc.C$B.Units + Arc.C$B.aUnits) &
                   (Arc.C$E.SF + Arc.C$E.aSF) ==
                   (Arc.C$B.SF + Arc.C$B.aSF)] <- "None"

  Arc.C$Rebuild <- 0
  Arc.C$Rebuild[Arc.C$E.YearBuilt >= Beg.Year &
                Arc.C$B.YearBuilt < Beg.Year] <- 1

  Arc.C$AddSize <- 0
  Arc.C$AddSize[(Arc.C$E.SF + Arc.C$E.aSF) >
                 (Arc.C$B.SF + Arc.C$B.aSF) &
                 Arc.C$Rebuild == 0] <- 1

  Arc.C$MinusUnits <- 0
  Arc.C$MinusUnits[(Arc.C$E.SF + Arc.C$E.aSF) <
                   (Arc.C$B.SF + Arc.C$B.aSF)  &
                   Arc.C$Rebuild == 0] <- 1

  Arc.C$AddUnits <- 0
  Arc.C$AddUnits[(Arc.C$E.Units + Arc.C$E.aUnits) >
                 (Arc.C$B.Units + Arc.C$B.aUnits) &
                 Arc.C$Rebuild == 0] <- 1

  Arc.C$MinusUnits <- 0
  Arc.C$MinusUnits[(Arc.C$E.Units + Arc.C$E.aUnits) <
                   (Arc.C$B.Units + Arc.C$B.aUnits)  &
                   Arc.C$Rebuild == 0] <- 1

  Arc.C$AddBldg <- 0
  Arc.C$AddBldg[Arc.C$E.NbrBldgs > Arc.C$B.NbrBldgs &
                 Arc.C$Rebuild == 0] <- 1

  Arc.C$MinusBldg <- 0
  Arc.C$MinusBldg[Arc.C$E.NbrBldgs < Arc.C$B.NbrBldgs &
                   Arc.C$Rebuild == 0] <- 1

# 13.5.1 Set Loss Type and Amount
  Arc.C$Loss.Units <- 0
  Arc.C$Loss.SF <- 0
  Arc.C$Gain.Units <- 0
  Arc.C$Gain.SF <- 0
  Arc.C$Chng.Time <- 0

 # 13.5.2 Label the Rebuilds
  reb <- which(Arc.C$Rebuild == 1)
  Arc.C$Chng.Type[reb] <- "Redev"
  Arc.C$Chng.Time[reb] <- Arc.C$E.YearBuilt[reb]
  Arc.C$Loss.Units[reb] <- Arc.C$B.Units[reb] + Arc.C$B.aUnits[reb]
  Arc.C$Loss.SF[reb] <- Arc.C$B.SF[reb] + Arc.C$B.aSF[reb]
  Arc.C$Gain.Units[reb] <- Arc.C$E.Units[reb] + Arc.C$E.aUnits[reb]
  Arc.C$Gain.SF[reb] <- Arc.C$E.SF[reb] + Arc.C$E.aSF[reb]

 # 13.5.3 Label the Size Losses 
  sfl <- which(Arc.C$MinusSize == 1)
  Arc.C$Chng.Type[sfl] <- "Exp"
  Arc.C$Chng.Time[sfl] <- yearFix(
    CchangeFinder(Arc.C[sfl, ],"BldgGrossSqFt", Beg.Year, End.Year))
  Arc.C$Loss.Units[sfl] <- (Arc.C$B.Units[sfl] + Arc.C$B.aUnits[sfl]) - 
    (Arc.C$E.Units[sfl] + Arc.C$E.aUnits[sfl])
  Arc.C$Loss.SF[sfl] <- (Arc.C$B.SF[sfl] + Arc.C$B.aSF[sfl]) - 
    (Arc.C$E.SF[sfl] + Arc.C$E.aSF[sfl])

# 13.5.4 Label the Size Gains 
  sfg <- which(Arc.C$AddSize == 1)
  Arc.C$Chng.Type[sfg] <- "Exp"
  Arc.C$Chng.Time[sfg] <- yearFix(
    CchangeFinder(Arc.C[sfg, ],"BldgGrossSqFt", Beg.Year, End.Year))
  Arc.C$Loss.Units[sfg] <- (Arc.C$B.Units[sfg] + Arc.C$B.aUnits[sfg]) - 
    (Arc.C$E.Units[sfg] + Arc.C$E.aUnits[sfg])
  Arc.C$Loss.SF[sfg] <- (Arc.C$B.SF[sfg] + Arc.C$B.aSF[sfg]) - 
    (Arc.C$E.SF[sfg] + Arc.C$E.aSF[sfg])

 # 13.5.5 Label the Unit Loss 
  unl <- which(Arc.C$MinusUnits == 1)
  Arc.C$Chng.Type[unl] <- "Dens"
  Arc.C$Chng.Type[Arc.C$MinusUnits == 1 & (Arc.C$MinusSize == 1 |
                    Arc.C$AddSize == 1)] <- "Exp"
  Arc.C$Chng.Time[unl] <- yearFix(
    CchangeFinder(Arc.C[unl, ],"Units", Beg.Year, End.Year))
  Arc.C$Loss.Units[unl] <- (Arc.C$B.Units[unl] + Arc.C$B.aUnits[unl]) - 
    (Arc.C$E.Units[unl] + Arc.C$E.aUnits[unl])
  Arc.C$Loss.SF[unl] <- (Arc.C$B.SF[unl] + Arc.C$B.aSF[unl]) - 
    (Arc.C$E.SF[unl] + Arc.C$E.aSF[unl])

 # 13.5.6 Label the Unit Gains 
  ung <- which(Arc.C$AddUnits == 1)
  Arc.C$Chng.Type[ung] <- "Dens"
  Arc.C$Chng.Type[Arc.C$AddUnits == 1 & (Arc.C$MinusSize == 1 |
                         Arc.C$AddSize == 1)] <- "Exp"
  Arc.C$Chng.Time[ung] <- yearFix(
    CchangeFinder(Arc.C[ung, ],"Units", Beg.Year, End.Year))
  Arc.C$Loss.Units[ung] <- (Arc.C$B.Units[ung] + Arc.C$B.aUnits[ung]) - 
    (Arc.C$E.Units[ung] + Arc.C$E.aUnits[ung])
  Arc.C$Loss.SF[ung] <- (Arc.C$B.SF[ung] + Arc.C$B.aSF[ung]) - 
  (Arc.C$E.SF[ung] + Arc.C$E.aSF[ung])

 # 13.5.7 Label the Bldg Loss 
  bgl <- which(Arc.C$MinusBldg == 1)
  Arc.C$Chng.Type[bgl] <- "Demo"
  Arc.C$Chng.Time[bgl] <- yearFix(
    CchangeFinder(Arc.C[bgl, ],"BldgGrossSqFt", Beg.Year, End.Year))
  Arc.C$Loss.Units[bgl] <- (Arc.C$B.Units[bgl] + Arc.C$B.aUnits[bgl]) - 
    (Arc.C$E.Units[bgl] + Arc.C$E.aUnits[bgl])
  Arc.C$Loss.SF[bgl] <- (Arc.C$B.SF[bgl] + Arc.C$B.aSF[bgl]) - 
    (Arc.C$E.SF[bgl] + Arc.C$E.aSF[bgl])

 # 13.5.8 Label the Building Gain
  bgg <- which(Arc.C$AddBldg == 1)
  Arc.C$Chng.Type[bgg] <- "New"
  Arc.C$Chng.Time[bgg] <- yearFix(
    CchangeFinder(Arc.C[bgg, ],"BldgGrossSqFt", Beg.Year, End.Year))
  Arc.C$Loss.Units[bgg] <- (Arc.C$B.Units[bgg] + Arc.C$B.aUnits[bgg]) - 
    (Arc.C$E.Units[bgg] + Arc.C$E.aUnits[bgg])
  Arc.C$Loss.SF[bgg] <- (Arc.C$B.SF[bgg] + Arc.C$B.aSF[bgg]) - 
    (Arc.C$E.SF[bgg] + Arc.C$E.aSF[bgg])

# 13.5.9 Clean Up
  Arc.C$Rebuild <- NULL
  Arc.C$AddBldg <- NULL  
  Arc.C$AddUnits <- NULL
  Arc.C$MinusUnits <- NULL
  Arc.C$MinusBldg <- NULL
  Arc.C$AddSize <- NULL
  Arc.C$MinusSize <- NULL

# 13.6 Label Vacant Parcels ----------------------------------------------------

 # 13.6.1 Set Chng.Type
  Arc.V$Chng.Type <- "None"
  Arc.V$Use.Chng <- "None"

 # 13.6.2 Set Loss units and amounts
  Arc.V$Loss.Units <- 0
  Arc.V$Loss.SF <- 0 

 # 13.6.3 Set Gain Units and Amounts
  Arc.V$Gain.Units <- 0
  Arc.V$Gain.SF <- 0 

 # 13.6.4 Set time that the lot size changed 
  Arc.V$Chng.Time <- yearFix(PchangeFinder(Arc.V,"SqFtLot"
                                        ,Beg.Year,End.Year))

# 13.7 Recombine --------------------------------------------------------------- 

if(dim(Arc.K)[1] > 0){
  all.RyPSn <- rbind(Arc.R, Arc.A, Arc.K, Arc.C, Arc.V)
}

if(dim(Arc.K)[1] == 0){
  all.RyPSn <- rbind(Arc.R, Arc.A, Arc.C, Arc.V)
}

################################################################################
# 14.0 Deal with Val.A Parcels that are PT Incons and RT Incons     ------------

# 14.1 Split into Record Type 
  Anc.R <- Anc[Anc$B.Class=="R",]
  Anc.A <- Anc[Anc$B.Class=="A" | Anc$B.Class == "C.A",]
  Anc.C <- Anc[Anc$B.Class=="C",]
  Anc.K <- Anc[Anc$B.Class=="K" | Anc$B.Class == "C.K",]
  Anc.V <- Anc[Anc$B.Class=="V",]
  Anc.X <- Anc[Anc$B.Class=="X",]

# 14.2 Deal with Residential ---------------------------------------------------
 
 # 14.2.1 Label Use.Chng
  Anc.R$Use.Chng <- "R_to_C"
  Anc.R$Use.Chng[Anc.R$E.Class=="A" | Anc.R$E.Class=="C.A" ] <- "R_to_A"
  Anc.R$Use.Chng[Anc.R$E.Class=="V"] <- "R_to_V"
  Anc.R$Use.Chng[Anc.R$E.Class=="K"] <- "R_to_K"
  Anc.R$Use.Chng[Anc.R$E.Class=="X"] <- "R_to_X"

 # 14.2.2 Set Loss SF and Loss Units 
  Anc.R$Loss.SF <- Anc.R$B.SF + Anc.R$B.aSF
  Anc.R$Loss.Units <- Anc.R$B.Units + Anc.R$B.aUnits

 # 14.2.3 Set Gain Units
  Anc.R$Gain.Units <- 0
  Anc.R$Gain.SF <- 0
  Anc.R$Chng.Time <- 0
  Anc.R$Chng.Type <- "X"

 # 14.2.4 Work on Res to Apt Changes
  r2a <- which(Anc.R$Use.Chng == "R_to_A")
  if(length(r2a)>0){
    Anc.R$Chng.Type[r2a] <- "Redev"
    Anc.R$Gain.Units[r2a] <- (Anc.R$E.Units[r2a] 
                              + Anc.R$E.aUnits[r2a])
    Anc.R$Gain.SF[r2a] <- (Anc.R$E.SF[r2a] 
                           + Anc.R$E.aSF[r2a])
    Anc.R$Chng.Time[r2a] <- Anc.R$E.YearBuilt[r2a]
  
  # 14.2.4.1 Label Change Type
    Anc.R$Chng.Type[Anc.R$Use.Chng == "R_to_A" & 
                      Anc.R$Chng.Time < Beg.Year] <- "Conv"
  
    Anc.R$Chng.Type[Anc.R$Chng.Type=="Conv" & 
                      Anc.R$Gain.Units == Anc.R$Loss.Units] <- "A.C."
  
  # 14.2.4.2 Label Chng Time for Conversions  
  conv <- which(Anc.R$Chng.Type == "Conv" & Anc.R$Use.Chng == "R_to_A")
  Anc.R$Chng.Time[conv] <- yearFix(
    PchangeFinder(Anc.R[conv,],"PresentUse", Beg.Year, End.Year))
 }

 # 14.2.5 Work on Res to Comm Changes
  r2c <- which(Anc.R$Use.Chng=="R_to_C")
  if(length(r2c)>0){
    Anc.R$Chng.Type[r2c] <- "Redev"
    Anc.R$Gain.SF[r2c] <- Anc.R$E.SF[r2c] 
    Anc.R$Gain.Units[r2c] <- (Anc.R$E.Units[r2c] + Anc.R$E.aUnits[r2c]) 
    Anc.R$Chng.Time[r2c] <- Anc.R$E.YearBuilt[r2c]
  
  # 14.2.5.1 Label Change Type
  Anc.R$Chng.Type[Anc.R$Use.Chng == "R_to_C" & 
                   Anc.R$Chng.Time < Beg.Year] <- "Conv"
  
  # 14.2.5.2 Label Chng Time for Conversions  
  conv <- which(Anc.R$Chng.Type == "Conv" & Anc.R$Use.Chng == "R_to_C")
  Anc.R$Chng.Time[conv] <- yearFix(
    PchangeFinder(Anc.R[conv,],"PresentUse", Beg.Year, End.Year))
 } 

 # 14.2.6 Work on Res to Condo Changes
  r2k <- which(Anc.R$Use.Chng == "R_to_K")
  if(length(r2k)>0){  
    Anc.R$Chng.Type[r2k] <- "Redev"
    Anc.R$Gain.SF[r2k] <- Anc.R$E.SF[r2k] 
    Anc.R$Gain.Units[r2k] <- (Anc.R$E.Units[r2k] + Anc.R$E.aUnits[r2k]) 
    Anc.R$Chng.Time[r2k] <- Anc.R$E.YearBuilt[r2k]
  
  # 14.2.6.1 Label Change Type
    Anc.R$Chng.Type[Anc.R$Use.Chng == "R_to_K" & 
                      Anc.R$Chng.Time < Beg.Year] <- "Conv"
  
  # 14.2.6.2 Label Chng Time for Conversions  
    conv <- which(Anc.R$Chng.Type == "Conv" & Anc.R$Use.Chng == "R_to_K")
    Anc.R$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.R[conv,],"PresentUse", Beg.Year, End.Year))
  }

 # 14.2.7 Work on Res to Vacant Changes
  r2v <- which(Anc.R$Use.Chng == "R_to_V")
  if(length(r2v)>0){  
    Anc.R$Chng.Time[r2v] <- yearFix(PchangeFinder(Anc.R[r2v,]
                                    ,"PresentUse",Beg.Year,End.Year))     
    Anc.R$Chng.Type[r2v] <- "Demo"
  }

 # 14.2.8 Work on Res to X Changes
  r2x <- which(Anc.R$Use.Chng == "R_to_X")
  if(length(r2x)>0){  
    Anc.R$Chng.Time[r2x] <- yearFix(PchangeFinder(Anc.R[r2x,]
                                     ,"PresentUse",Beg.Year,End.Year))          
    Anc.R$Chng.Type[r2x] <- "A.C."    
  }

# 14.3 Label Apartment Properties ----------------------------------------------

 # 14.3.1 Set Change Type
  Anc.A$Use.Chng <- "A_to_R"
  Anc.A$Use.Chng[Anc.A$E.Class=="A" | 
                   Anc.A$E.Class=="C.A" ] <- "None"
  Anc.A$Use.Chng[Anc.A$E.Class=="C"] <- "A_to_C"
  Anc.A$Use.Chng[Anc.A$E.Class=="K"] <- "A_to_K"
  Anc.A$Use.Chng[Anc.A$E.Class=="V"] <- "A_to_V"
  Anc.A$Use.Chng[Anc.A$E.Class=="X"] <- "A_to_X"

 # 14.3.2 Set Loss Units and Loss Amount
  Anc.A$Loss.SF <- Anc.A$B.SF + Anc.A$B.aSF
  Anc.A$Loss.Units <- Anc.A$B.Units + Anc.A$B.aUnits 

 # 14.3.3 Set Gain Units based on Change Type
  Anc.A$Gain.SF <- 0
  Anc.A$Gain.Units <- 0
  Anc.A$Chng.Time <- 0
  Anc.A$Chng.Type <- "X"
  Anc.A$Chng.Type[Anc.A$Use.Chng == "None"] <- "A.C."

 # 14.3.4 Work of Apt to Res Changes
  a2r <- which(Anc.A$Use.Chng=="A_to_R")
  if(length(a2r)>0){
    Anc.A$Chng.Type[a2r] <- "Redev"
    Anc.A$Gain.Units[a2r] <- Anc.A$E.Units[a2r] + Anc.A$E.aUnits[a2r]
    Anc.A$Gain.SF[a2r] <- Anc.A$E.SF[a2r] + Anc.A$E.aSF[a2r]
    Anc.A$Chng.Time[a2r] <- Anc.A$E.YearBuilt[a2r]
  
  # 14.3.4.1 Label Change Type
    Anc.A$Chng.Type[Anc.A$Use.Chng == "A_to_R" & 
                      Anc.A$Chng.Time < Beg.Year] <- "Conv"
    Anc.A$Chng.Type[Anc.A$Use.Chng == "A_to_R" & 
                      Anc.A$Chng.Time < Beg.Year &
                      Anc.A$Loss.Units == Anc.A$Gain.Units] <- "A.C."
  
  # 14.3.4.2 Label Chng Time for Conversions  
    conv <- which(Anc.A$Chng.Type == "Conv" & Anc.A$Use.Chng == "A_to_R")
    Anc.A$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.A[conv,],"PresentUse", Beg.Year, End.Year))
  }

 # 14.3.5 Work on Apt to Comm Changes
  a2c <- which(Anc.A$Use.Chng == "A_to_C")
  if(length(a2c)>0){
    Anc.A$Chng.Type[a2c] <- "Redev"
    Anc.A$Gain.SF[a2c] <- Anc.A$E.SF[a2c] + Anc.A$E.aSF[a2c] 
    Anc.A$Gain.Units[a2c] <- (Anc.A$E.Units[a2c] + Anc.A$E.aUnits[a2c]) 
    Anc.A$Chng.Time[a2c] <- Anc.A$E.YearBuilt[a2c]
  
  # 14.3.5.1 Label Change Type
    Anc.A$Chng.Type[Anc.A$Use.Chng == "A_to_C" & 
                      Anc.A$Chng.Time < Beg.Year] <- "Conv"
    Anc.A$Chng.Type[Anc.A$Use.Chng == "A_to_C" & 
                      Anc.A$Chng.Time < Beg.Year &
                      Anc.A$Loss.Units == Anc.A$Gain.Units & 
                      Anc.A$Loss.SF == Anc.A$Gain.SF]  <- "A.C."
  
  # 14.3.5.2 Label Chng Time for Conversions  
    conv <- which(Anc.A$Chng.Type == "Conv" & Anc.A$Use.Chng == "A_to_C")
    Anc.A$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.A[conv,], "PresentUse", Beg.Year, End.Year))
  }

 # 14.3.6 Work on Apt to Condo Changes
  a2k <- which(Anc.A$Use.Chng == "A_to_K")
  if(length(a2k)>0){  
    Anc.A$Chng.Type[a2k] <- "Redev"
    Anc.A$Gain.SF[a2k] <- Anc.A$E.SF[a2k] + Anc.A$E.aSF[a2k] 
    Anc.A$Gain.Units[a2k] <- (Anc.A$E.Units[a2k] + Anc.A$E.aUnits[a2k]) 
    Anc.A$Chng.Time[a2k] <- Anc.A$E.YearBuilt[a2k]
  
  # 14.3.6.1 Label Change Type
    Anc.A$Chng.Type[Anc.A$Use.Chng == "A_to_K" & 
                      Anc.A$Chng.Time < Beg.Year] <- "Conv"
  
  # 14.3.6.2 Label Chng Time for Conversions  
    conv <- which(Anc.A$Chng.Type == "Conv" & Anc.A$Use.Chng == "A_to_K")
    Anc.A$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.A[conv,],"PresentUse", Beg.Year, End.Year))
  }

 # 14.3.7 Work on Apt to Vacant Changes
  a2v <- which(Anc.A$Use.Chng == "A_to_V")
  if(length(a2v)>0){  
    Anc.A$Chng.Type[a2v] <- "Demo"
    Anc.A$Chng.Time[a2v] <- yearFix(
      PchangeFinder(Anc.A[a2v,],"PresentUse", Beg.Year, End.Year))
  }

 # 14.3.8 Work on Apt to X Changes
  a2x <- which(Anc.A$Use.Chng == "A_to_X")
  if(length(a2x)>0){  
    Anc.A$Chng.Time[a2x] <- yearFix(PchangeFinder(Anc.A[a2x,]
                                    ,"PresentUse",Beg.Year,End.Year))          
    Anc.A$Chng.Type[a2x] <- "A.C."    
  }

# 14.4 Deal with Condo parcels -------------------------------------------------

if(dim(Anc.K)[1]> 0){
  
  # 14.4.1 Set Change Type
  Anc.K$Use.Chng <- "K_to_R"
  Anc.K$Use.Chng[Anc.K$E.Class=="K" | 
                     Anc.K$E.Class=="C.K" ] <- "None"
  Anc.K$Use.Chng[Anc.K$E.Class=="C"] <- "K_to_C"
  Anc.K$Use.Chng[Anc.K$E.Class=="A"] <- "K_to_A"
  Anc.K$Use.Chng[Anc.K$E.Class=="V"] <- "K_to_V"
  Anc.K$Use.Chng[Anc.K$E.Class=="X"] <- "K_to_V"
  
  # 14.4.2 Set Loss Units and Loss Amount
  Anc.K$Loss.SF <- Anc.K$B.SF + Anc.K$B.aSF
  Anc.K$Loss.Units <- Anc.K$B.Units + Anc.K$B.aUnits 
  
  # 14.4.3 Set Gain Units based on Change Type 
  Anc.K$Gain.SF <- 0
  Anc.K$Gain.Units <- 0
  Anc.K$Chng.Time <- 0
  Anc.K$Chng.Type <- "X"
  Anc.K$Chng.Type[Anc.K$Use.Chng == "None"] <- "A.C."
  
  # 14.4.4 Work of Condo to Res Changes
  k2r <- which(Anc.K$Use.Chng == "K_to_R")
  if(length(k2r) > 0){
    Anc.K$Chng.Type[k2r] <- "Redev"
    Anc.K$Gain.Units[k2r] <- Anc.K$E.Units[k2r] + Anc.K$E.aUnits[k2r]
    Anc.K$Gain.SF[k2r] <- Anc.K$E.SF[k2r] + Anc.K$E.aSF[k2r]
    Anc.K$Chng.Time[k2r] <- Anc.K$E.YearBuilt[k2r]
    
    # 14.4.4.1 Label Change Type
    Anc.K$Chng.Type[Anc.K$Use.Chng == "K_to_R" & 
                        Anc.K$Chng.Time < Beg.Year] <- "Conv"
    Anc.K$Chng.Type[Anc.K$Use.Chng == "K_to_R" & 
                        Anc.K$Chng.Time < Beg.Year &
                        Anc.K$Loss.Units == Anc.K$Gain.Units] <- "A.C."
    
    # 14.4.4.2 Label Chng Time for Conversions  
    conv <- which(Anc.K$Chng.Type == "Conv" & Anc.K$Use.Chng == "K_to_R")
    Anc.K$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.K[conv,],"PresentUse", Beg.Year, End.Year))
  }
  
  # 14.4.5 Work on Condo to Comm Changes
  k2c <- which(Anc.K$Use.Chng == "K_to_C")
  if(length(k2c)>0){
    Anc.K$Chng.Type[k2c] <- "Redev"
    Anc.K$Gain.SF[k2c] <- Anc.K$E.SF[k2c] + Anc.K$E.aSF[k2c] 
    Anc.K$Gain.Units[k2c] <- (Anc.K$E.Units[k2c] + Anc.K$E.aUnits[k2c]) 
    Anc.K$Chng.Time[k2c] <- Anc.K$E.YearBuilt[k2c]
    
    # 14.4.5.1 Label Change Type
    Anc.K$Chng.Type[Anc.K$Use.Chng == "K_to_C" & 
                        Anc.K$Chng.Time < Beg.Year] <- "Conv"
    Anc.K$Chng.Type[Anc.K$Use.Chng == "K_to_C" & 
                        Anc.K$Chng.Time < Beg.Year &
                        Anc.K$Loss.Units == Anc.K$Gain.Units & 
                        Anc.K$Loss.SF == Anc.K$Gain.SF]  <- "A.C."
    
    # 14.4.5.2 Label Chng Time for Conversions  
    conv <- which(Anc.K$Chng.Type == "Conv" & Anc.K$Use.Chng == "K_to_C")
    Anc.K$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.K[conv,],"PresentUse", Beg.Year, End.Year))
  }
  
  # 14.4.6 Work on Condo to Apt Changes
  k2a <- which(Anc.K$Use.Chng == "K_to_A")
  if(length(a2k)>0){  
    Anc.K$Chng.Type[k2a] <- "Redev"
    Anc.K$Gain.SF[k2a] <- Anc.K$E.SF[k2a] + Anc.K$E.aSF[k2a] 
    Anc.K$Gain.Units[k2a] <- (Anc.K$E.Units[k2a] + Anc.K$E.aUnits[k2a]) 
    Anc.K$Chng.Time[k2a] <- Anc.K$E.YearBuilt[k2a]
    
    # 14.4.6.1 Label Change Type
    Anc.K$Chng.Type[Anc.K$Use.Chng == "K_to_A" & 
                        Anc.K$Chng.Time < Beg.Year] <- "Conv"
    
    # 14.4.6.2 Label Chng Time for Conversions  
    conv <- which(Anc.K$Chng.Type == "Conv" & Anc.K$Use.Chng == "A_to_K")
    Anc.K$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.K[conv,],"PresentUse", Beg.Year, End.Year))
  }
  
  # 14.4.7 Work on Condo to Vacant Changes
  k2v <- which(Anc.K$Use.Chng == "K_to_V")
  if(length(k2v)>0){  
    Anc.K$Chng.Type[k2v] <- "Demo"
    Anc.K$Chng.Time[k2v] <- yearFix(
      PchangeFinder(Anc.K[k2v,],"PresentUse", Beg.Year, End.Year))
  }
  
  # 14.4.8 Work on Condo to X Changes
  k2x <- which(Anc.K$Use.Chng == "K_to_X")
  if(length(k2x)>0){  
    Anc.K$Chng.Time[k2x] <- yearFix(
      PchangeFinder(Anc.K[k2x,], "PresentUse", Beg.Year, End.Year))          
    Anc.K$Chng.Type[k2x] <- "A.C."    
  }
}

# 14.5 Commercial Properties ---------------------------------------------------

 # 14.5.1 Set Change Type
  Anc.C$Use.Chng <- "C_to_R"
  Anc.C$Use.Chng[Anc.C$E.Class=="A" | 
                   Anc.C$E.Class=="C.A" ] <- "C_to_A"
  Anc.C$Use.Chng[Anc.C$E.Class=="K"] <- "C_to_K"
  Anc.C$Use.Chng[Anc.C$E.Class=="V"] <- "C_to_V"
  Anc.C$Use.Chng[Anc.C$E.Class=="X"] <- "C_to_X"

 # 14.5.2 Set Loss Units and Loss Amount
  Anc.C$Loss.Units <- Anc.C$B.Units + Anc.C$B.aUnits
  Anc.C$Loss.SF <- Anc.C$B.SF + Anc.C$B.aSF

 # 14.5.3 Set Gain Units based on Change Type
  Anc.C$Gain.Units <- 0
  Anc.C$Gain.SF <- 0
  Anc.C$Chng.Time <- 0
  Anc.C$Chng.Type <- "X"
  Anc.C$Chng.Type[Anc.C$Use.Chng == "None"] <- "A.C."

 # 14.5.4 Work on Comm to Res Changes
  c2r <- which(Anc.C$Use.Chng == "C_to_R")
  if(length(c2r)>0){
    Anc.C$Chng.Type[c2r] <- "Redev"
    Anc.C$Gain.Units[c2r] <- Anc.C$E.Units[c2r] + Anc.C$E.aUnits[c2r]
    Anc.C$Gain.SF[c2r] <- Anc.C$E.SF[c2r] + Anc.C$E.aSF[c2r]
    Anc.C$Chng.Time[c2r] <- Anc.C$E.YearBuilt[c2r]
  
  # 14.5.4.1 Label Change Type
    Anc.C$Chng.Type[Anc.C$Use.Chng == "C_to_R" & 
                      Anc.C$Chng.Time < Beg.Year] <- "Conv"
    Anc.C$Chng.Type[Anc.C$Use.Chng == "C_to_R" & 
                      Anc.C$Chng.Time < Beg.Year &
                      Anc.C$Loss.Units == Anc.C$Gain.Units &
                      Anc.C$Loss.SF == Anc.C$Gain.SF] <- "A.C."
  
  # 14.5.4.2 Label Chng Time for Conversions  
    conv <- which(Anc.C$Chng.Type == "Conv" & Anc.C$Use.Chng == "C_to_R")
    Anc.C$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.C[conv,],"PresentUse", Beg.Year, End.Year))  
  }

 # 14.5.5 Work on Comm to Apt Changes
  c2a <- which(Anc.C$Use.Chng == "C_to_A")
  if(length(c2a)>0){
    Anc.C$Chng.Type[c2a] <- "Redev"
    Anc.C$Gain.Units[c2a] <- Anc.C$E.Units[c2a] + Anc.C$E.aUnits[c2a]
    Anc.C$Gain.SF[c2a] <- Anc.C$E.SF[c2a] + Anc.C$E.aSF[c2a]
    Anc.C$Chng.Time[c2a] <- Anc.C$E.YearBuilt[c2a]
  
   # 14.5.5.1 Label Change Type
    Anc.C$Chng.Type[Anc.C$Use.Chng == "C_to_A" & 
                      Anc.C$Chng.Time < Beg.Year] <- "Conv"
    Anc.C$Chng.Type[Anc.C$Use.Chng == "C_to_A" & 
                      Anc.C$Chng.Time < Beg.Year &
                      Anc.C$Loss.Units == Anc.C$Gain.Units &
                      Anc.C$Loss.SF == Anc.C$Gain.SF] <- "A.C."
  
    # 14.5.5.2 Label Chng Time for Conversions  
    conv <- which(Anc.C$Chng.Type == "Conv" & Anc.C$Use.Chng == "C_to_A")
    Anc.C$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.C[conv,],"PresentUse", Beg.Year, End.Year))  
  }

 # 14.5.6 Work on Comm to Condo Changes
  c2k <- which(Anc.C$Use.Chng == "C_to_K")
  if(length(c2k)>0){  
    Anc.C$Chng.Type[c2k] <- "Redev"
    Anc.C$Gain.Units[c2k] <- Anc.C$E.Units[c2k] + Anc.C$E.aUnits[c2k]
    Anc.C$Gain.SF[c2k] <- Anc.C$E.SF[c2k] + Anc.C$E.aSF[c2k]
    Anc.C$Chng.Time[c2k] <- Anc.C$E.YearBuilt[c2k]
  
  # 14.5.6.1 Label Change Type
    Anc.C$Chng.Type[Anc.C$Use.Chng == "C_to_K" & 
                      Anc.C$Chng.Time < Beg.Year] <- "Conv"
    Anc.C$Chng.Type[Anc.C$Use.Chng == "C_to_K" & 
                      Anc.C$Chng.Time < Beg.Year &
                      Anc.C$Loss.Units == Anc.C$Gain.Units &
                      Anc.C$Loss.SF == Anc.C$Gain.SF] <- "A.C."
  
  # 14.5.6.2 Label Chng Time for Conversions  
    conv <- which(Anc.C$Chng.Type == "Conv" & Anc.C$Use.Chng == "C_to_K")
    Anc.C$Chng.Time[conv] <- yearFix(
      PchangeFinder(Anc.C[conv,],"PresentUse", Beg.Year, End.Year))  
  }

 # 14.5.7 Work on Comm to Vacant Changes
  c2v <- which(Anc.C$Use.Chng == "C_to_V")
  if(length(c2v)>0){  
    Anc.C$Chng.Type[c2v] <- "Demo"
    Anc.C$Chng.Time[c2v] <- yearFix(PchangeFinder(
      Anc.C[c2v,], "PresentUse", Beg.Year, End.Year))     
  }

 # 14.5.8 Work on Comm to X Changes
  c2x <- which(Anc.C$Use.Chng == "C_to_X")
  if(length(c2x)>0){  
    Anc.C$Chng.Type[c2x] <- "A.C."
    Anc.C$Chng.Time[c2x] <- yearFix(changeFinder(Anc.C[c2x,]
                                     ,"PresentUse",Beg.Year,End.Year))          
  }

# 14.6 Vacant Properties -------------------------------------------------------

 # 14.6.1 Set Change Type
  Anc.V$Use.Chng <- "V_to_R"
  Anc.V$Use.Chng[Anc.V$E.Class=="A" | 
                   Anc.V$E.Class=="C.A" ] <- "V_to_A"
  Anc.V$Use.Chng[Anc.V$E.Class=="K"] <- "V_to_K"
  Anc.V$Use.Chng[Anc.V$E.Class=="C"] <- "V_to_C"
  Anc.V$Use.Chng[Anc.V$E.Class=="X"] <- "V_to_X"

 # 14.6.2 Set Loss Units and Loss Amount
  Anc.V$Loss.Units <- 0
  Anc.V$Loss.SF <- 0 

 # 14.6.3 Set Gain Units based on Change Type
  Anc.V$Gain.Units <- 0 
  Anc.V$Gain.SF <- 0
  Anc.V$Chng.Time <- 0
  Anc.V$Chng.Type <- "X"

 # 14.6.4 Work on Vac to Res Changes
  v2r <- which(Anc.V$Use.Chng == "V_to_R")
  if(length(v2r)>0){
    Anc.V$Chng.Type[v2r] <- "New"
    Anc.V$Gain.Units[v2r] <- Anc.V$E.Units[v2r] + Anc.V$E.aUnits[v2r]
    Anc.V$Gain.SF[v2r] <- Anc.V$E.SF[v2r] + Anc.V$E.aSF[v2r]
    Anc.V$Chng.Time[v2r] <- Anc.V$E.YearBuilt[v2r]
  
  # 14.6.4.1 Fix the assessor labeling issues
    Anc.V$Chng.Type[Anc.V$Use.Chng=="V_to_R" & 
                      Anc.V$Chng.Time < Beg.Year] <- "A.C."
  }

  # 14.6.5 Work on Vac to Apt Changes
  v2a <- which(Anc.V$Use.Chng == "V_to_A")
  if(length(v2a)>0){  
    Anc.V$Chng.Type[v2a] <- "New"
    Anc.V$Gain.Units[v2a] <- Anc.V$E.Units[v2a] + Anc.V$E.aUnits[v2a]
    Anc.V$Gain.SF[v2a] <- Anc.V$E.SF[v2a] + Anc.V$E.aSF[v2a]
    Anc.V$Chng.Time[v2a] <- Anc.V$E.YearBuilt[v2a]
  
  # 14.6.5.1 Assessor Changes
    Anc.V$Chng.Type[Anc.V$Use.Chng == "V_to_A" & 
                      Anc.V$Chng.Time < Beg.Year] <- "A.C."
  }

  # 14.6.6 Work on Vac to Condo Changes
  v2k <- which(Anc.V$Use.Chng == "V_to_K")
  if(length(v2k)>0){  
    Anc.V$Chng.Type[v2k] <- "New"
    Anc.V$Gain.Units[v2k] <- Anc.V$E.Units[v2k] + Anc.V$E.aUnits[v2k]
    Anc.V$Gain.SF[v2k] <- Anc.V$E.SF[v2k] + Anc.V$E.aSF[v2k]
    Anc.V$Chng.Time[v2k] <- Anc.V$E.YearBuilt[v2k]
  
  # 14.6.6.1 Assessor Changes
    Anc.V$Chng.Type[Anc.V$Use.Chng == "V_to_K" & 
                      Anc.V$Chng.Time < Beg.Year] <- "A.C."
  }

 # 14.6.7 Work on Vac to Comm Changes
  v2c <- which(Anc.V$Use.Chng == "V_to_C")
  if(length(v2c)>0){
    Anc.V$Chng.Type[v2c] <- "New"
    Anc.V$Gain.Units[v2c] <- Anc.V$E.Units[v2c] + Anc.V$E.aUnits[v2c]
    Anc.V$Gain.SF[v2c] <- Anc.V$E.SF[v2c] + Anc.V$E.aSF[v2c]
    Anc.V$Chng.Time[v2c] <- Anc.V$E.YearBuilt[v2c]
  
  # 14.6.7.1 Assessor Changes
    Anc.V$Chng.Type[Anc.V$Chng.Type=="V_to_C" & 
                      Anc.V$Chng.Time < Beg.Year] <- "A.C."
  }

 # 14.6.8 Work on Vac to X Changes
  v2x <- which(Anc.V$Use.Chng == "V_to_X")
  if(length(v2x)>0){  
    Anc.V$Chng.Type[v2x] <- "A.C."
    Anc.V$Chng.Time[v2x] <- yearFix(PchangeFinder(Anc.C[v2x,]
                                ,"PresentUse",Beg.Year,End.Year))          
  }

# 14.7 Recombine ---------------------------------------------------------------

if(dim(Anc.K)[1] > 0){
  all.RPSn <- rbind(Anc.R, Anc.A, Anc.K, Anc.C, Anc.V)
}

if(dim(Anc.K)[1] == 0){
  all.RPSn <- rbind(Anc.R, Anc.A, Anc.C, Anc.V)
}

################################################################################
# 15.0 Combine all A parcels  --------------------------------------------------

 # 15.1 Rename to make sense
  Invalids <- inv.RPno
  RPYes <- all.RPyes
  A.PYes <- all.PyRSn
  A.RYes <- all.RyPSn
  A.No <- all.RPSn

# 15.2 Apply a source for diagnostics
  Invalids$Source <- "Invalids"
  All.Cons$Source <- "All.Cons"
  RPYes$Source <- "RPYes"
  A.PYes$Source <- "A.PYes"
  A.RYes$Source <- "A.RYes"
  A.No$Source <- "A.No"

# 15.3 Combine all
  All.A <- rbind(All.Cons, RPYes, A.RYes, A.PYes, A.No)

################################################################################
# 16.0 Deal with Parcels Matched from Beg and End   ----------------------------

  val.B <- val.RPno[val.RPno$PType=="B",]
  val.E <- val.RPno[val.RPno$PType=="E",]

# 16.1 Prep Data Fields --------------------------------------------------------
  
 # 16.1.1 Convert Fields to Characters

  val.B$PINX <- as.character(val.B$PINX)
  val.B$Parent <- as.character(val.B$Parent)
  val.E$PINX <- as.character(val.E$PINX)
  val.E$Parent <- as.character(val.E$Parent)

 # 16.1.2 Set up Blank Fields
  
  val.B$Use.Chng <- "X"
  val.B$Chng.Type <- "None"
  val.B$Chng.Time <- 0
  val.B$Loss.Units <- 0
  val.B$Loss.SF <- 0
  val.B$Gain.Units <- 0
  val.B$Gain.SF <- 0
  val.B$Match <- 0

  val.E$Use.Chng <- "X"
  val.E$Chng.Type <- "None"
  val.E$Chng.Time <- 0
  val.E$Loss.Units <- 0
  val.E$Loss.SF <- 0
  val.E$Gain.Units <- 0
  val.E$Gain.SF <- 0
  val.E$Match <- 0

 # 16.1.3 Set up Field Locators
  bu <- which(colnames(val.B) == "B.Use")
  bc <- which(colnames(val.B) == "B.Class")
  eu <- which(colnames(val.E) == "E.Use")
  ec <- which(colnames(val.E) == "E.Class")

# 16.2 Match The Renamed Instances --------------------------------------------- 

 # 16.2.1 Locate potential matches

  Bnr.Del <- which(val.B$Topo.Type == "Delete - Renamed" | 
                     val.B$Topo.Type == "Delete - Lot Adj - Renamed")
  Enr.Del <- which(val.E$Topo.Type == "New - Renamed")

 # 16.2.2 Loop Through to Match

  for (i in 1:length(Enr.Del)){
   a <- which(val.B$Parent[Bnr.Del] == val.E$PINX[Enr.Del[i]])
   if(length(a) > 0){
     val.B$Match[Bnr.Del[a[1]]] <- 1
     val.E$Match[Enr.Del[i]] <- 1
     
     val.E[Enr.Del[i],bu:bc] <- val.B[Bnr.Del[a],bu:bc]     
   }   
  }

 # 16.2.3 Isolate Matched and Non-Matched
  
  BE.Rename <- val.E[val.E$Match == 1, ]
  val.B <- val.B[val.B$Match == 0,]
  val.E <- val.E[val.E$Match == 0,]
  
# 16.3 Match the Bs to Remaining E's--------------------------------------------

 # 16.3.1 Locate Potential B Matches
  
  vb <- which(is.na(val.B$Parent) == 0)

 # 16.3.2 Loop through to Match
  for (i in 1:length(vb)){
    a <- which(val.E$PINX == val.B$Parent[vb[i]])[1]
    if(length(a) > 0 & is.na(a) == 0){
      val.B$Match[vb[i]] <- 1
      val.E$Match[a] <- 1
      val.B[vb[i],eu:ec] <- val.E[a,eu:ec]
    }   
  }

 # 16.3.3 Isolate Matched and Non-Matched
  
  BE.Join <- rbind(val.B[val.B$Match == 1, ],
                   val.E[val.E$Match == 1, ])
  val.B <- val.B[val.B$Match == 0,]
  val.E <- val.E[val.E$Match == 0,]

 # 16.3.4 Combine total loss of B Joined Parcels
  
  # 16.3.4.1 Set up necessary variables

  bej.b <- BE.Join[BE.Join$PType == "B",]
  bej.e <- BE.Join[BE.Join$PType == "E",]
  vbp <- as.data.frame(table(bej.e$PINX))

  bej.e$B.Zone <- as.character(bej.e$B.Zone)
  bej.e$B.Class <- as.character(bej.e$B.Class)

  # 16.3.4.2 Begin Loop Through
  for(i in 1:dim(vbp)[1]){
   
    # Identify Matches
    
    a <- which(bej.b$Parent == vbp$Var1[i])
    e <- which(bej.e$PINX == vbp$Var1[i])
    
    # Extract Old Data
    old.b <- bej.b[a,bu:bc]
    
    # Summaryize Old Data
    if(dim(old.b)[1] > 0){
      
      ou <- table(old.b$B.Use)
      bej.e$B.Use[e] <- ifelse(length(ou) == 1, as.numeric(ou), 0)
      
      bz <- table(as.character(old.b$B.Zone))
      bej.e$B.Zone[e] <- ifelse(length(bz) == 1, row.names(bz), "Multiple")
      
      bej.e$B.LotSF[e] <- sum(old.b$B.LotSF)
      bej.e$B.SF[e] <- sum(old.b$B.SF)
      bej.e$B.YearBuilt[e] <- max(old.b$B.YearBuilt)
      bej.e$B.Cond[e] <- max(old.b$B.Cond)
      bej.e$B.Units[e] <- sum(old.b$B.Units)
      bej.e$B.Stories[e] <- max(old.b$B.Stories)
      bej.e$B.NbrBldgs[e] <- sum(old.b$B.NbrBldgs)
      bej.e$B.aUnits[e] <- sum(old.b$B.aUnits)
      bej.e$B.aSF[e] <- sum(old.b$B.aSF)
      bej.e$B.aYB[e] <- max(old.b$B.aYB)
      
      oc <- table(as.character(old.b$B.Class))
      bej.e$B.Class[e] <- ifelse(length(oc) == 1, row.names(oc), "M")
    }
  }  

 # 16.3.5 Remove B. Parcels
  BE.Join <- bej.e

# 16.4 Match the Es to All Parcels  --------------------------------------------

 # 16.4.1 Identify Potential Matches
  val.E$Parent[is.na(val.E$Parent)] <- "..9999999999"
  val.E <- val.E[order(val.E$B.YearBuilt,decreasing=T),]
  val.E <- val.E[order(val.E$Parent),]

  ve <- which(is.na(val.E$Parent) == 0)
  

 # 16.4.2 Set up variables
  
  AA <- All.A
  VB <- val.B
  VB$Source <- "VB"
  VB$Match <- NULL
  
  AA <- rbind(AA, VB)
  AA$Match <- 0

 # 16.4.2 Run Through Loop
  for (i in 1:length(ve)){
    
    a <- which(AA$PINX == val.E$Parent[ve[i]])
      
    if(length(a) > 0){
     
      val.E$Match[ve[i]] <- 1

      # if match is not a B only parcel
      if(AA$Source != "VB"){
        val.E$Use.Chng[ve[i]] <- ifelse(AA$B.Class[a] == val.E$E.Class[ve[i]],
                                      "None",paste0(AA$B.Class[a], "_to_", 
                                            val.E$E.Class[ve[i]]))
      
        val.E$Chng.Type[ve[i]] <- ifelse((AA$Chng.Type[a] == "Demo" |
                                          AA$Chng.Type[a] == "Redev")  &
                                       val.E$E.YearBuilt[ve[i]] >= Beg.Year,
                                       "Redev", "X")
      
        val.E$Chng.Type[ve[i]] <- ifelse((AA$Chng.Type[a] == "Demo" |
                                          AA$Chng.Type[a] == "Redev") &
                                       val.E$E.YearBuilt[ve[i]] < Beg.Year &
                                       val.E$Use.Chng[ve[i]] != "None",
                                       "Conv", val.E$Chng.Type[ve[i]])
      
        val.E$Chng.Type[ve[i]] <- ifelse((AA$Chng.Type[a] == "Demo" |
                                          AA$Chng.Type[a] == "Redev") &
                                         val.E$E.YearBuilt[ve[i]] < Beg.Year &
                                         val.E$Use.Chng[ve[i]] == "None",
                                         "Exp", val.E$Chng.Type[ve[i]])
      
        val.E$Chng.Type[ve[i]] <- ifelse((AA$Chng.Type[a] == "Demo" |
                                          AA$Chng.Type[a] == "Redev") &
                                         val.E$E.YearBuilt[ve[i]] < Beg.Year &
                                         val.E$Use.Chng[ve[i]] == "None" &
                              (val.E$E.Units[ve[i]] + val.E$E.aUnits[ve[i]]) !=
                              (AA$B.Units[a] + AA$B.aUnits[a]),
                                       "Dens", val.E$Chng.Type[ve[i]])
            
        val.E$Chng.Type[ve[i]] <- ifelse((AA$Chng.Type[a] != "Demo" &
                                          AA$Chng.Type[a] != "Redev") &
                                       val.E$E.YearBuilt[ve[i]] >= Beg.Year,
                                       "New", val.E$Chng.Type[ve[i]])
      
        val.E$Chng.Type[ve[i]] <- ifelse((AA$Chng.Type[a] != "Demo" &
                                          AA$Chng.Type[a] != "Redev") &
                                       val.E$E.YearBuilt[ve[i]] < Beg.Year,
                                       "None", val.E$Chng.Type[ve[i]])
      
        val.E$Chng.Time[ve[i]] <- ifelse(val.E$E.YearBuilt[ve[i]] > Beg.Year,
                                       val.E$E.YearBuilt[ve[i]], 
                                       val.E$PChng.Year[ve[i]])
      
        val.E$Chng.Time[ve[i]] <- ifelse(val.E$Chng.Time[ve[i]] == -1,
                                       Beg.Year, val.E$Chng.Time[ve[i]])
    
        
        if(val.E$Chng.Type[ve[i]] == "Demo" | val.E$Chng.Type[ve[i]] == "Redev"){
          val.E$Gain.Units[ve[i]] <- val.E$E.Units[ve[i]] + val.E$E.aUnits[ve[i]] 
          val.E$Gain.SF[ve[i]] <- val.E$E.SF[ve[i]] + val.E$E.aSF[ve[i]] 
        }
        
        if(val.E$Chng.Type[ve[i]] == "New"){
          val.E$Gain.Units[ve[i]] <- val.E$E.Units[ve[i]] + val.E$E.aUnits[ve[i]] 
          val.E$Gain.SF[ve[i]] <- val.E$E.SF[ve[i]] + val.E$E.aSF[ve[i]] 
        }
        
        if(val.E$Chng.Type[ve[i]] == "Exp" | val.E$Chng.Type[ve[i]] == "Dens" ){
          val.E$Gain.Units[ve[i]] <- (val.E$E.Units[ve[i]] + 
                                        val.E$E.aUnits[ve[i]]) - 
                                     (AA$B.Units[a] + 
                                        AA$B.aUnits[a]) 
          
          val.E$Gain.SF[ve[i]] <- (val.E$E.SF[ve[i]] + 
                                     val.E$E.aSF[ve[i]]) - 
                                  (AA$B.SF[a] + 
                                     AA$B.aSF[a])   
        }
        
        if(val.E$Chng.Type[ve[i]] == "Conv"){
          val.E$Gain.Units[ve[i]] <- (val.E$E.Units[ve[i]] + 
                                        val.E$E.aUnits[ve[i]]) - 
                                     (AA$B.Units[a] + 
                                        AA$B.aUnits[a]) 
          
          val.E$Gain.SF[ve[i]] <- (val.E$E.SF[ve[i]] + 
                                     val.E$E.aSF[ve[i]]) - 
                                  (AA$B.SF[a] + 
                                     AA$B.aSF[a])
         }
       }
      
      # if match is a B only parcel
      if(AA$Source[a] == "VB"){
        vba <- which(val.B$PINX==val.E$Parent[ve[i]])
        val.B$Match[vba] <- 1
      
        val.E$Use.Chng[ve[i]] <- ifelse(AA$B.Class[a] == val.E$E.Class[ve[i]],
                                        "None",paste0(AA$B.Class[a], "_to_", 
                                                      val.E$E.Class[ve[i]]))
        
        val.E$Chng.Time[ve[i]] <- ifelse(val.E$E.YearBuilt[ve[i]] > Beg.Year,
                                         val.E$E.YearBuilt[ve[i]], 
                                         val.E$PChng.Year[ve[i]])
        
        val.E$Chng.Time[ve[i]] <- ifelse(val.E$Chng.Time[ve[i]] == -1,
                                         Beg.Year, val.E$Chng.Time[ve[i]])
        
        # if the first time matching
        if(AA$Match[a] == 0){
          
          val.E$Chng.Type[ve[i]] <- ifelse(AA$B.SF[a] > 0 
                                           & val.E$E.YearBuilt[ve[i]] > Beg.Year,
                                           "Redev",val.E$Chng.Type[ve[i]])
          val.E$Chng.Type[ve[i]] <- ifelse(AA$B.SF[a] > 0 
                                           & val.E$E.YearBuilt[ve[i]] <= Beg.Year,
                                           "Demo",val.E$Chng.Type[ve[i]])
          val.E$Chng.Type[ve[i]] <- ifelse(AA$B.SF[a] == 0 
                                           & val.E$E.YearBuilt[ve[i]] > Beg.Year,
                                           "New",val.E$Chng.Type[ve[i]])
          val.E$Chng.Type[ve[i]] <- ifelse(AA$B.YearBuilt[a] == 
                                             val.E$E.YearBuilt[ve[i]] &
                                           AA$B.NbrBldgs[a] == 
                                             val.E$E.NbrBldgs[ve[i]] &
                                           AA$B.SF[a] != val.E$E.SF[ve[i]], 
                                           "Exp",val.E$Chng.Type[ve[i]])
          val.E$Chng.Type[ve[i]] <- ifelse(AA$B.YearBuilt[a] == 
                                             val.E$E.YearBuilt[ve[i]] &
                                           AA$B.NbrBldgs[a] == 
                                             val.E$E.NbrBldgs[ve[i]] &
                                           AA$B.Class[a] != val.E$E.Class[ve[i]], 
                                             "Conv",val.E$Chng.Type[ve[i]])
          val.E$Chng.Type[ve[i]] <- ifelse(AA$B.YearBuilt[a] == 
                                             val.E$E.YearBuilt[ve[i]] &
                                           AA$B.NbrBldgs[a] == 
                                             val.E$E.NbrBldgs[ve[i]] &
                                           AA$B.Class[a] == 
                                             val.E$E.Class[ve[i]] &
                                           AA$B.SF[a] == val.E$E.SF[ve[i]] &
                                           AA$B.Units[a] != 
                                             val.E$E.Units[ve[i]], 
                                           "Dens",val.E$Chng.Type[ve[i]])
          
          if(val.E$Chng.Type == "New" | val.E$Chng.Type == "Redev" |
               val.E$Chng.Type == "Conv" ){
               val.E$Gain.Units[ve[i]] <- (val.E$E.Units[ve[i]] + 
                                        val.E$E.aUnits[ve[i]]) 
               val.E$Gain.SF[ve[i]] <- (val.E$E.SF[ve[i]] + 
                                             val.E$E.aSF[ve[i]]) 
               val.E$Loss.Units[ve[i]] <- (AA$B.Units[a] + 
                                             AA$B.aUnits[a]) 
               val.E$Loss.SF[ve[i]] <- (AA$B.SF[a] + 
                                             AA$B.aSF[a]) 
          }
          if(val.E$Chng.Type == "Exp" | val.E$Chng.Type == "Dens"){
            val.E$Gain.Units[ve[i]] <- (val.E$E.Units[ve[i]] + 
                                          val.E$E.aUnits[ve[i]]) -  
                                       (AA$B.Units[a] + 
                                          AA$E.aUnits[a]) 
              
            val.E$Gain.SF[ve[i]] <- (val.E$E.SF[ve[i]] + 
                                       val.E$E.aSF[ve[i]]) - 
                                    (AA$E.SF[a] + 
                                       AA$E.aSF[a]) 
          }
          val.E[ve[i],which(colnames(val.E)=="B.Use"):which(
                                        colnames(val.E)=="B.Class")] <- 
          AA[a,which(colnames(AA)=="B.Use"):which(colnames(AA)=="B.Class")]  
        }
      
        # if not the first time matching
        if(AA$Match[a] == 1){
          val.E$Chng.Type[ve[i]] <- ifelse(val.E$E.YearBuilt[ve[i]] > Beg.Year,
                                           "New",val.E$Chng.Type[ve[i]])
          val.E$Chng.Type[ve[i]] <- ifelse(val.E$E.YearBuilt[ve[i]] > Beg.Year &
                                           AA$B.SF[a]>0,"Redev",
                                           val.E$Chng.Type[ve[i]])
          
          if(val.E$Chng.Type[ve[i]] == "New" | val.E$Chng.Type[ve[i]] == "Redev"){
            val.E$Gain.Units[ve[i]] <- val.E$E.Units[ve[i]] + val.E$E.aUnits[ve[i]] 
            val.E$Gain.SF[ve[i]] <- val.E$E.SF[ve[i]] + val.E$E.aSF[ve[i]] 
          }
          if(val.E$Chng.Type[ve[i]] == "Conv" | val.E$Chng.Type[ve[i]] == "Dens" |
               val.E$Chng.Type[ve[i]] == "Exp"){
            val.E$Gain.Units[ve[i]] <- (val.E$E.Units[ve[i]] + 
                                          val.E$E.aUnits[ve[i]]) - 
                                       (AA$B.Units[a] + AA$B.aUnits[a])

            val.E$Gain.SF[ve[i]] <- (val.E$E.SF[ve[i]] + val.E$E.aSF[ve[i]]) - 
                                    (AA$B.SF[a] + AA$B.aSF[a])
          }
        }
      AA$Match[a] <- 1
      }
    }   
  }

 # 16.4.3 Split off From val.E

  BE.Split <- val.E[val.E$Match == 1, ]
  BE.Split$Match <- NULL
  BE.Split$Source <- "BE.Split"
  val.E <- val.E[val.E$Match == 0, ]
  val.B <- val.B[val.B$Match == 0, ]
  val.E$Match <- NULL

# 16.5 Calculate changes to Matched Parcels (Rename and Join only)  ------------
  
 # 16.5.1 Combine to remainging  
  
  BE.Match <- rbind(BE.Rename, BE.Join)

 # 16.5.2 Set Use.Chng
  BE.Match$Use.Chng <- "None"
  BE.Match$Use.Chng[BE.Match$B.Class != BE.Match$E.Class] <- paste0(
      BE.Match$B.Class[BE.Match$B.Class != BE.Match$E.Class]
      ,"_to_", BE.Match$E.Class[BE.Match$B.Class != BE.Match$E.Class])

  # 16.5.3 Set Chng.Type
  BE.Match$Chng.Type <- "None"
  BE.Match$Chng.Type[BE.Match$E.YearBuilt >= Beg.Year] <- "Redev"
  BE.Match$Chng.Type[BE.Match$Chng.Type != "Redev" &
                       BE.Match$Use.Chng != "None"] <- "Conv"
  BE.Match$Chng.Type[BE.Match$Chng.Type == "Conv" &
                     BE.Match$B.Class == "V"] <- "New"
  BE.Match$Chng.Type[BE.Match$Chng.Type == "None" &
                     (BE.Match$B.SF + BE.Match$B.aSF) != 
                       (BE.Match$E.SF + BE.Match$E.aSF)] <- "Exp"
  BE.Match$Chng.Type[BE.Match$Chng.Type == "None" &
                     (BE.Match$B.Units + BE.Match$B.aUnits) != 
                     (BE.Match$E.Units + BE.Match$E.aUnits)] <- "Dens"

  # 16.5.4 Set Chng.Time
  BE.Match$Chng.Time <- BE.Match$E.YearBuilt
  BE.Match$Chng.Time[BE.Match$Chng.Time < Beg.Year] <- BE.Match$PChng.Year[
    BE.Match$Chng.Time < Beg.Year]
  
  bct <- which(BE.Match$Chng.Time < Beg.Year)
  BE.Match$Chng.Time[bct] <- yearFix(PchangeFinder(
                  BE.Match[bct,], "PresentUse",Beg.Year,End.Year,rev=T))

  # 16.5.5 Set SF and Units Gained
  BE.Match$Gain.Units <- 0
  ung <- which((BE.Match$E.Units + BE.Match$E.aUnits) >
                  (BE.Match$B.Units + BE.Match$B.aUnits))
  BE.Match$Gain.Units[ung] <- (BE.Match$E.Units[ung] + BE.Match$E.aUnits[ung]) - 
                                (BE.Match$B.Units[ung] + BE.Match$B.aUnits[ung])

  BE.Match$Gain.SF <- 0
  sfg <- which((BE.Match$E.SF + BE.Match$E.aSF) >
                          (BE.Match$B.SF + BE.Match$B.aSF))
  BE.Match$Gain.SF[sfg] <- (BE.Match$E.SF[sfg] + BE.Match$E.aSF[sfg]) - 
                              (BE.Match$B.SF[sfg] + BE.Match$B.aSF[sfg])
                            
  # 16.5.6 Set SF and Units Lost
  BE.Match$Loss.Units <- 0
  unl <- which((BE.Match$E.Units + BE.Match$E.aUnits) <
               (BE.Match$B.Units + BE.Match$B.aUnits))
  BE.Match$Loss.Units[unl] <- (BE.Match$B.Units[unl] + BE.Match$B.aUnits[unl]) - 
    (BE.Match$E.Units[unl] + BE.Match$E.aUnits[unl])

  BE.Match$Loss.SF <- 0
  sfl <- which((BE.Match$E.SF + BE.Match$E.aSF) <
               (BE.Match$B.SF + BE.Match$B.aSF))
  BE.Match$Loss.SF[sfl] <- (BE.Match$B.SF[sfl] + BE.Match$B.aSF[sfl]) - 
                (BE.Match$E.SF[sfl] + BE.Match$E.aSF[sfl])

  BE.Match$Match <- NULL
  BE.Match$Source <- "BE.Match"

################################################################################
# 17.0 Deal with Beg Year Only Parcels, Unmatched  -----------------------------

  Bnr <- val.B

 # 17.1 Deal with Delete-Joins

  Bnr.del <- Bnr[Bnr$Topo.Type == "Delete - Join",]
  Bnr <- Bnr[Bnr$Topo.Type != "Delete - Join",]
  
  re <- 0
  All.A$Match <- 0
  Bnr.del$Match <- 0
  # 17.1.1
  for (i in 1:dim(Bnr.del)[1]){
    a <- which(All.A$PINX == Bnr.del$Parent[i])
    if(length(a)>0){
      Bnr.del$Match[i] <- 1
      All.A$Loss.Units[a] <- All.A$Loss.Units[a] +
                              Bnr.del$B.Units[i] +
                               Bnr.del$B.aUnits[i]
      re <- re+1
      
      All.A$Match[a] <- 1  
      if(All.A$Chng.Type[a]=="New" & Bnr.del$B.SF[i]>0){
        All.A$Chng.Type[a] <- "Redev"
      }
    }
  }

  All.A$Match <- NULL
  B.Matched <- Bnr.del[Bnr.del$Match==1,]
  B.Matched$Source <- "B.Matched"

  Bnr <- rbind(Bnr, Bnr.del[Bnr.del$Match==0,])

  Bnr$Chng.Type <- "Demo"
  Bnr$Chng.Type[Bnr$B.YearBuilt >= 1995] <- "Conv"

  Bnr$Use.Chng <- "None"
  Bnr$Loss.Units <- 0
  Bnr$Loss.Units[Bnr$Chng.Type == "Demo"] <- Bnr$B.Units[Bnr$Chng.Type=="Demo"]+
                                             Bnr$B.aUnits[Bnr$Chng.Type=="Demo"]
  Bnr$Loss.SF <- 0
  Bnr$Loss.SF[Bnr$Chng.Type == "Demo"] <- Bnr$B.SF[Bnr$Chng.Type=="Demo"]+
                                          Bnr$B.aSF[Bnr$Chng.Type=="Demo"]
  Bnr$Gain.Units <- 0
  Bnr$Gain.SF <- 0
  Bnr$Chng.Time <- Bnr$PChng.Year

  Bnr$Match <- NULL
  Bnr$Source <- "Bnr" 

################################################################################
# 18.0 Deal with Parcels not existing at the Beg.Year    -----------------------

  Enr <- val.E

  Enr$Chng.Type <- "None"
  Enr$Chng.Type[Enr$E.YearBuilt >= Beg.Year] <- "Redev"

  Enr$Use.Chng <- "None"
  Enr$Gain.Units <- 0
  Enr$Gain.Units[Enr$Chng.Type == "Redev"] <- Enr$E.Units[
      Enr$Chng.Type=="Redev"] + Enr$E.aUnits[Enr$Chng.Type=="Redev"]

  Enr$Gain.SF <- 0
  Enr$Gain.SF[Enr$Chng.Type == "Redev"] <- Enr$E.SF[
       Enr$Chng.Type=="Redev"] + Enr$E.aSF[Enr$Chng.Type=="Redev"]

  Enr$Loss.Units <- 0
  Enr$Loss.SF <- 0
  Enr$Chng.Time <- Enr$PChng.Year
  Enr$Chng.Time[Enr$Chng.Time == -1] <- Beg.Year
  Enr$Chng.Time[Enr$Chng.Type == "Redev"] <- Enr$E.YearBuilt[
                                                Enr$Chng.Type == "Redev"] 
  Enr$Match <- NULL
  Enr$Prev <- NULL
  Enr$Source <- "Enr"

################################################################################
# 19.0 Combine all -------------------------------------------------------------

# 19.1 Set Count.Type

  All.A$Count.Type <- "A"
  BE.Match$Count.Type <- "A"
  BE.Split$Count.Type <- "A"
  Bnr$Count.Type <- "B"
  Enr$Count.Type <- "E"

# 19.2 Combine all
  All.PH <- rbind(All.A, BE.Match, BE.Split, Bnr, Enr)

# 19.3 Fix Negative Units

  nlu <- which(All.PH$Loss.Units < 0)
  nls <- which(All.PH$Loss.SF < 0)
  ngu <- which(All.PH$Gain.Units < 0)
  ngs <- which(All.PH$Gain.SF < 0)

  All.PH$Gain.Units[nlu] <- All.PH$Gain.Units[nlu] + -(All.PH$Loss.Units[nlu])
  All.PH$Loss.Units[nlu] <- 0

  All.PH$Gain.SF[nls] <- All.PH$Gain.SF[nls] + -(All.PH$Loss.SF[nls])
  All.PH$Loss.SF[nls] <- 0

  All.PH$Loss.Units[ngu] <- All.PH$Loss.Units[ngu] + -(All.PH$Gain.Units[ngu])
  All.PH$Gain.Units[ngu] <- 0

  All.PH$Loss.SF[ngs] <- All.PH$Loss.SF[ngs] + -(All.PH$Gain.SF[ngs])
  All.PH$Gain.SF[ngs] <- 0

# 19.4 Fix NAs
  All.PH$Chng.Time[is.na(All.PH$Chng.Time)] <- 0

# 19.5 Fix Issues of New Commerical with old age
  nco <- which(All.PH$Chng.Type == "New" & All.PH$Chng.Time < Beg.Year)  
  All.PH$Chng.Time[nco] <- yearFix(CchangeFinder(
    All.PH[nco,], "BldgNbr", Beg.Year,End.Year))

# 19.6 Fix Isses of Assessor Corrections
  acy <- which(All.PH$Chng.Type == "A.C.")
  All.PH$Loss.Units[acy] <- 0
  All.PH$Loss.SF[acy] <- 0
  All.PH$Gain.Units[acy] <- 0
  All.PH$Gain.SF[acy] <- 0
  All.PH$Chng.Time[acy] <- 0

# 19.7 Fix issues of Demo with gains
  dem <- which(All.PH$Chng.Type == "Demo")
  All.PH$Gain.SF[dem] <- 0
  All.PH$Gain.Units[dem] <- 0

# 19.8 Fix Harbor View Property (8590400545)
  All.PH$Loss.SF[All.PH$PINX=="..8590400545"] <- 0
  All.PH$Gain.SF[All.PH$PINX=="..8590400545"] <- All.PH$E.aSF[
    All.PH$PINX=="..8590400545"]
  All.PH$Chng.Type[All.PH$PINX=="..8590400545"] <- "A.C."
  All.PH$Use.Chng[All.PH$PINX=="..8590400545"] <- "None"

return(All.PH)
}    
