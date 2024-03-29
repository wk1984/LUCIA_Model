################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: Parcel_Geometry                                              #
#                                                                              #
#        Compares the data and geometry from two years                         #                                                                                    ###          by Andy Krause                                                  ###
#        To determine the changes to the parcel over that time                 #
#                                                                              #
#        Most Recent Update: 11/2/2013                                         #
#                                                                              # 
###############################################################################'

calculateGeometry <- function(clip, Beg.Year, End.Year, B.Parcel, E.Parcel, 
                              B.Data, E.Data, SizePar, AreaPar){

# 0.0 Set Global Parameters, Load Libraries and Files --------------------------

# 0.1 Load Libraries -----------------------------------------------------------

  require(maptools)
  require(rgeos)
  require(RODBC)

# 0.2 Load Files ---------------------------------------------------------------

  source("D://Code//R//General//R_Helpers//Spatial_Helpers.R")
  source("D://Code//R//General//R_Helpers//Basic_Helpers.R")
  source("D://Code//R//Research//LUCIA_Model//parcelFinder.R")  
  source("D://Code//R//Research//LUCIA_Model//yearFix.R")

# 0.3 Set Global Parameters ----------------------------------------------------

  if(!exists("SizePar")) SizePar <-  .1 # % Change to Consider as Geometry Change
  if(!exists("AreaPar")) AreaPar <- 3000     # # of Feet to Search 
  if(!exists("Beg.Year")) Beg.Year <- 1999    # Year Analysis should begin
  if(!exists("End.Year")) End.Year <- 2013    # Year Analysis ends

################################################################################
# 1.0 Clip GIS and CAMA data ---------------------------------------------------

# 1.1 Clip Beginning Year Parcel Files to Desired Geographic Area --------------

 # 1.1.1 Add X, Y
  begXY <- t(sapply(slot(B.Parcel, "polygons"), function(i) slot(i, "labpt")))
  B.Parcel@data$X <- begXY[ ,1]
  B.Parcel@data$Y <- begXY[ ,2]
  
 # 1.1.2 Limit to Parcels with Clip Bbox
  beg1 <- B.Parcel[B.Parcel@data$X > clip@bbox[1, 1] &
                   B.Parcel@data$X < clip@bbox[1, 2] &
                   B.Parcel@data$Y > clip@bbox[2, 1] &
                   B.Parcel@data$Y < clip@bbox[2, 2], ]
  
 # 1.1.3 Convert to SPDF  
  begXY1 <- SpatialPointsDataFrame(t(sapply(slot(beg1, "polygons"),
                                      function(i) slot(i, "labpt"))), beg1@data)
  
 # 1.1.4 Clip to Clip Extent
  clipint <- gIntersects(clip, begXY1, byid=T)
  beg <- beg1[which(clipint), ]

# 1.2 Clip End Year Parcel Files to Desired Geographic Area --------------------

 # 1.2.1 Add X, Y
  endXY <- t(sapply(slot(E.Parcel, "polygons"), function(i) slot(i, "labpt")))
  E.Parcel@data$X <- endXY[ ,1]
  E.Parcel@data$Y <- endXY[ ,2]
  
 # 1.2.2 Limit to Parcels with Clip Bbox
  end1 <- E.Parcel[E.Parcel@data$X > clip@bbox[1, 1] &
                   E.Parcel@data$X < clip@bbox[1, 2] &
                   E.Parcel@data$Y > clip@bbox[2, 1] &
                   E.Parcel@data$Y < clip@bbox[2, 2], ]

 # 1.2.3 Convert to SPDF  
  endXY1 <- SpatialPointsDataFrame(t(sapply(slot(end1, "polygons"),
                                      function(i) slot(i, "labpt"))), end1@data)
 
 # 1.2.4 Clip to Clip Extent
  clipint <- gIntersects(clip, endXY1, byid=T)
  end <- end1[which(clipint), ]

################################################################################
# 2.0 Fix the Parcel and Point data files --------------------------------------

# 2.1 Fix and Merge Beginning Year Parcel Data ---------------------------------

 # 2.1.1 Add PINX
  beg@data$PINX <- paste0("..", beg@data$PIN)

 # 2.1.2 Merge ShapeFile
  beg <- mergeShapeFile(beg, B.Data, "PINX", "PINX", allX=TRUE)

 # 2.1.3 Trim to Needed Fields
  beg@data <- beg@data[ ,c("PINX", "X", "Y", "PresentUse",
                          "CurrentZoning", "SqFtLot")]
  
 # 2.1.4 Clean up Fields
  beg@data$PresentUse[is.na(beg@data$PresentUse)] <- -99
  beg@data$CurrentZoning <- as.character(beg@data$CurrentZoning)
  beg@data$CurrentZoning[is.na(beg@data$CurrentZoning)] <- "NA"
  beg@data$SqFtLot[is.na(beg@data$SqFtLot)] <- -99

# 2.2 Fix and Merge End Year Parcel Data ---------------------------------------

 # 2.2.1 Add PINX
  end@data$PINX <- paste0("..", end@data$PIN)

 # 2.2.2 Merge ShapeFile
  end <- mergeShapeFile(end, E.Data, "PINX", "PINX", allX=TRUE)

 # 2.2.3 Trim to Needed Fields
  end@data <- end@data[ ,c("PINX", "X", "Y", "PresentUse",
                           "CurrentZoning", "SqFtLot")]
 # 2.1.4 Clean up Fields
  end@data$PresentUse[is.na(end@data$PresentUse)] <- -99
  end@data$CurrentZoning <- as.character(end@data$CurrentZoning)
  end@data$CurrentZoning[is.na(end@data$CurrentZoning)] <- "NA"
  end@data$SqFtLot[is.na(end@data$SqFtLot)] <- -99

################################################################################
# 3.0 Create a master list and label as A (all), B (beg) or E (end) ------------

# 3.1 Match End Parcels to Beginning parcels -----------------------------------

 # 3.1.1 Create temp list of Beg PINx and Merge to End
  bb <- beg@data[,c("PINX", "PresentUse")]
  bbb <- merge(bb, end@data[ ,c("PINX", "PresentUse")],
               by.x="PINX", by.y="PINX", all.x=T)
  colnames(bbb)[dim(bbb)[2]] <- "Type"
  
 # 3.1.2 Remove Duplicates
  bbb <- rmDup(bbb, "PINX")  

 # 3.1.3 Label as B (beginning only) or A (all years)
  bbb$Type <- ifelse(is.na(bbb$Type), "B", "A")
  bbb$PresentUse.x <- NULL

# 3.2 Match Beginning Parcels to End Parcels -----------------------------------

 # 3.2.1 Create temp list of End PINx and Merge to Beg
  ee <- end@data[ ,c("PINX", "PresentUse")]
  eee <- merge(ee,beg@data[ ,c("PINX", "PresentUse")],
               by.x="PINX", by.y="PINX", all.x=T)
  colnames(eee)[dim(eee)[2]] <- "Type"

 # 3.2.2 Label as E (end only) or A (all years)
  eee$Type <- ifelse(is.na(eee$Type), "E", "A")
  eee$PresentUse.x <- NULL

# 3.3 Comine A and B from 3.1 with only E from 3.2 -----------------------------
  Parcel.List <- rbind(bbb, eee[eee$Type == "E", ])

 # 3.3.1 Sort by Type and then PINX
  Parcel.List <- Parcel.List[order(Parcel.List$Type, Parcel.List$PINX), ]

# 3.9 Clean up Temp Data -------------------------------------------------------
  rm(bb); rm(bbb); rm(ee); rm(eee); rm(begXY); rm(endXY)

################################################################################
# 4.0 Add variables and Chop into A, B and E chunks ----------------------------

# 4.1 Add additional variables to Parcel.List ----------------------------------
  Parcel.List$Topo.Type <- "X"
  Parcel.List$Parent <- "X"
  Parcel.List$NbrChildren <- 0
  Parcel.List$Beg.Zone <- "X"
  Parcel.List$End.Zone <- "X"
  Parcel.List$Size.Diff <- 0

# 4.2 Chop into A, B and E chunks ----------------------------------------------
  plA <- Parcel.List[Parcel.List$Type == "A", ]
  plB <- Parcel.List[Parcel.List$Type == "B", ]
  plE <- Parcel.List[Parcel.List$Type == "E", ]

################################################################################
# 5.0 Label the "A" Parcels  ---------------------------------------------------

# 5.1 Start Loop through All "A" Parcels ---------------------------------------

for (i in 1:dim(plA)[1]){
   

# 5.2 Initial Setup and Variable Declarations ----------------------------------  
  
  # 5.2.1 Define Variable Indicating no change in size
  nosizechange <- 0  
  
  # 5.2.2 Set up initial beginning and ending parcels 
  bp.i <- beg[beg@data$PINX == plA$PINX[i], ]
  ep.i <- end[end@data$PINX == plA$PINX[i], ]

# 5.3 Calculate lot size differences between the bp and ep ---------------------  
  
 # 5.3.1 Calculate Lot Adjustment Percentage
  lotadj <- round((gArea(ep.i) - gArea(bp.i)) / gArea(bp.i), 3)
  
 # 5.3.2 Assign this difference to plA file
  plA$Size.Diff[i] <- lotadj

 # 5.3.3 Change nosizechange indicator if size dif > SizePar
  if(abs(lotadj) <= SizePar){nosizechange <- 1}

# 5.4 Label Consistent parcels as such -----------------------------------------
   
  # 5.4.1 If all match up, give it consistent
  if(nosizechange == 1){
    plA$Topo.Type[i] <- "Consistent"
    plA$NbrChildren[i] <- 0
    plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
    plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
    plA$Parent[i] <- "NA"
  } 
 

# 5.5 Test for the odd case where ep.i and bp.i do not intersect ---------------
  
 # 5.5.1 Determine if they intersect
  bp.ep.int <- gIntersects(bp.i, ep.i, byid=T)
  
  # 5.5.2 If Multi Polygon Parcel, label as such
  if(length(bp.ep.int) > 1){
    
    # If Multi Polygon is same size
    if(nosizechange == 1){
      plA$Topo.Type[i] <- "Consistent - MP"
      plA$NbrChildren[i] <- 0
      plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      plA$Parent[i] <- "NA"
      bp.ep.int <- TRUE
    }
    
    # If Multi Polygon is not the same size
    if(nosizechange == 0){
      plA$Topo.Type[i] <- "Change - MP"
      plA$NbrChildren[i] <- (-99)
      plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      plA$Parent[i] <- "NA"
      nosizechange <- 1
      bp.ep.int <- TRUE
    }
  } # Ends 5.5.2 If

  # 5.5.3 If parcel has physically moved, doesn't intersect and is diff size
  if(!bp.ep.int & length(bp.ep.int) <= 1){
    
    # If size hasn't changed
    if(abs(lotadj) <= SizePar){
      plA$Topo.Type[i] <- "Consistent - Rel" 
      plA$NbrChildren[i] <- (-99)
      plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      plA$Parent[i] <- "NA"
      nosizechange <- 1 # so that the following calculations do not occur
    }
    
    # If size has changed
    if(abs(lotadj) > SizePar){
      plA$Topo.Type[i] <- "Change - Rel" 
      plA$NbrChildren[i] <- (-99)
      plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      plA$Parent[i] <- "NA"
      nosizechange <- 1 # so that the following calculations do not occur
    }
  } # Ends 5.5.3 If  
} # Ends 5.1 If

# 5.6 Separate into Consistent and Not -----------------------------------------
  
  plA.Cons <- plA[plA$Topo.Type != "X", ]
  plA <- plA[plA$Topo.Type == "X", ]

# 5.7 Deal with remaining non-consistent plA parcels ---------------------------

 # 5.7.1 Start Loop
for(i in 1:dim(plA)[1]){

 # 5.7.2 Select beginning and Ending Parcels
  bp.i <- beg[beg@data$PINX == plA$PINX[i], ]
  ep.i <- end[end@data$PINX == plA$PINX[i], ]
  
 # 5.7.2 Select Area Parcels and determine intersects
  area.par.E <- end[end@data$X > (bp.i@bbox[1, 1] - AreaPar)
                    & end@data$X < (bp.i@bbox[1, 2] + AreaPar)
                    & end@data$Y > (bp.i@bbox[2, 1] - AreaPar)
                    & end@data$Y < (bp.i@bbox[2, 2] + AreaPar), ]
 
   # Determine which e.par intersect bp.i
   adj.int.E <- area.par.E[which(gIntersects(bp.i, area.par.E, byid=T)), ]
    
   # Loop through each (necessary because of overlappers)
   int.area.E <- rep(0, length(adj.int.E))
   for(ae in 1:length(adj.int.E)){
     int.area.E[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                         / gArea(adj.int.E[ae, ]))
   }
    
   # Determine which intersect at least SizePar (eliminates small overlaps)
   ie.ind <- length(which(int.area.E > SizePar))
    
   if(ie.ind == 0){
     inv.E <- NULL
     inv.area.E <- NULL
   }
    
   if(ie.ind != 0){
     inv.E <- adj.int.E[which(int.area.E > SizePar), ]
     inv.area.E <- int.area.E[which(int.area.E > SizePar)]
   }
    
  
# 5.8 Deal with those with Inv.E == 0 ------------------------------------------    
  
    if(length(inv.E) == 0){  
      
  # 5.8.1 Estimate Intesects B of E / B 
      bp.over<-rep(0,length(int.area.E))
      for(ae in 1:length(adj.int.E)){
        bp.over[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                           / gArea(bp.i))
      }
      bp.max <- bp.over[which(bp.over == max(bp.over))]
      
  # 5.8.2 Test for small interior join parcel
      if(bp.max >= .5){
        plA$Topo.Type[i] <- "Join - Retain"
        plA$NbrChildren[i] <- (-99)
        plA$Parent[i] <- ep.i@data$PINX
        plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      }  
      
  # 5.8.3 Label those that simply expanded into open space more than 50%
      if(length(adj.int.E) >= 1 & sum(bp.over) < .5){ 
        plA$Topo.Type[i] <- "Lot Adj - Exp"
        plA$NbrChildren[i] <- (-99)
        plA$Parent[i] <- ep.i@data$PINX
        plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      }
      
  # 5.8.4 Label those that completely relocated position (no intersect)
      if(length(adj.int.E) < 1){ 
        plA$Topo.Type[i] <- "Relocated"
        plA$NbrChildren[i] <- (-99)
        plA$Parent[i] <- "NA"
        plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      }
    } # Ends 5.8.1 If
      
# 5.9 Test those with only one intersecting parcel
    
    if(length(inv.E) == 1){ 
      
    # 5.9.1 Test for a possible join scenario with overlap of b.par on ep.i
      
      # Limit to parcels in large area (speeds up calcs)
      area.par.B <- beg[beg@data$X > (ep.i@bbox[1, 1] - AreaPar)
                        & beg@data$X < (ep.i@bbox[1, 2] + AreaPar)
                        & beg@data$Y > (ep.i@bbox[2, 1] - AreaPar)
                        & beg@data$Y < (ep.i@bbox[2, 2] + AreaPar), ]
      
      # Determine which b.par intersect ep.i
      adj.int.B <- area.par.B[which(gIntersects(ep.i, area.par.B, byid=T)), ]
      int.area.B <- rep(0, dim(adj.int.B)[1])
      for(ae in 1:length(adj.int.B)){
        int.area.B[ae] <- (gArea(gIntersection(ep.i, adj.int.B[ae, ])) 
                        / gArea(adj.int.B[ae, ]))
      }
 
      # Determine which intersect at least SizePar (eliminates small overlaps)
      inv.B <- adj.int.B[which(int.area.B > SizePar), ]
      inv.area.B <- int.area.B[which(int.area.B > SizePar)]
      
    # 5.9.2 Label those with a simple case of lot adjusment
      if(length(which(inv.area.B > .5)) <= 1){
        plA$Topo.Type[i] <- "Lot Adjustment"
        plA$NbrChildren[i] <- (-99)
        plA$Parent[i] <- "NA"
        plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      }
      
     # 5.9.3 Label those where more than one b.par joined to make ep.i
      if(length(which(inv.area.B > .5)) > 1){
        plA$Topo.Type[i] <- "Join - Retain"
        plA$NbrChildren[i] <- length(inv.area.B)
        plA$Parent[i] <- ep.i@data$PINX
        plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])      
      }
  }# Ends 5.9 If 

# 5.10 Test those with more than one involved parcel ---------------------------   
  
  if(length(inv.E) > 1){
      
  # 5.10.1 Label those with multiple involved parcels more than 50% in bp.i
    if(length(which(inv.area.E > .5)) > 1){  
      plA$Topo.Type[i] <- "Split - Retain"
      plA$NbrChildren[i] <- length(inv.area.E)
      plA$Parent[i] <- ep.i@data$PINX
      plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])      
    }
    
  # 5.10.2 Those involved in a minority part of a split
    if(length(which(inv.area.E > .5)) <= 1){
        
    # Sum up all involved parcels
      mp <- NULL
      for(k in 1:length(inv.area.E)){
        mp <- c(mp, which(beg@data$PINX == inv.E@data$PINX[k]))
      }
      mp.area <- (gArea(inv.E) / gArea(beg[mp, ])) / gArea(beg[mp, ])
        
    # If changes in involved parcels is more than size par
      if(abs(mp.area) > SizePar){
        plA$Topo.Type[i] <- "Lot Adj - Split"
        plA$NbrChildren[i] <- length(inv.area.E)
        plA$Parent[i] <- ep.i@data$PINX
        plA$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plA$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      }
      
    # If change in involved parcels is less than sizepar
      if(abs(mp.area) < SizePar){
        for(k in 1:length(mp)){
          ik <- which(plA$PINX == beg@data$PINX[mp[k]])
          plA$Topo.Type[ik] <- "Lot Adjustment"
          plA$NbrChildren[ik] <- 0
          plA$Parent[ik] <- "NA"
          plA$Beg.Zone[ik] <- as.character(beg@data$CurrentZoning[mp[k]])
          plA$End.Zone[ik] <- as.character(end@data$CurrentZoning[mp[k]])
        } # Ends k 
      }# If     
    }# Ends 5.10.2 If      
  }# Ends the 5.10 If 
} # Ends the 5.7 Loop

################################################################################
# 6.0 Label the "B" parcels  ---------------------------------------------------

# 6.1 Start Loop through All B Parcels -----------------------------------------

for (i in 1:dim(plB)[1]){
    
 # 6.1.1 Set up initial beginning and ending parcels 
  bp.i <- beg[beg@data$PINX == plB$PINX[i], ]
  
  
# 6.2 Test for multiple parcels ------------------------------------------------

  if(length(bp.i) > 1){
    plB$Topo.Type[i] <- "Multi-Polygon"
    plB$NbrChildren[i] <- (-99)
    plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
    plB$End.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
    plB$Parent[i] <- "NA"
  }

# 6.3 If not Multi-parcel polygon ----------------------------------------------
  
  if(length(bp.i)==1){
  
  # 6.3.1 Limit to parcels in large area (speeds up calcs) and intersect
    area.par.E <- end[end@data$X > (bp.i@bbox[1,1] - AreaPar)
                    & end@data$X < (bp.i@bbox[1,2] + AreaPar)
                    & end@data$Y > (bp.i@bbox[2,1] - AreaPar)
                    & end@data$Y < (bp.i@bbox[2,2] + AreaPar), ]
  
  # 6.3.2 Record length of all area parcels intersecting
    ap.E.int <- length(which(gIntersects(bp.i, area.par.E, byid=T)))
    
  # 6.3.3 When bp.i intersects nothing
    if(ap.E.int == 0){
      adj.int.E <- 0
      int.area.E <- NULL
      inv.area.E <- NULL
      iae.ind <- (-1)
      plB$Topo.Type[i] = "Relocated"
      plB$NbrChildren[i] <- (-99)
      plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plB$End.Zone[i] <- "NA"
      plB$Parent[i] <- "NA"
    }
    
  # 6.3.4 When bp.i intersects, calculate the % intersect
    if(ap.E.int > 0){ 
      adj.int.E <- area.par.E[which(gIntersects(bp.i, area.par.E, byid=T)), ]
      int.area.E<-rep(0,length(adj.int.E))
      for(ae in 1:length(adj.int.E)){
        int.area.E[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                           / gArea(adj.int.E[ae, ]))
      }
      
    # Set indicator if intersect > SizePar
      iae.ind <- length(which(int.area.E > SizePar))
    } # Ends 6.3.4 If
    
    
# 6.4 If not intersects more than SizePar --------------------------------------
    
    if(iae.ind == 0){
      
      # 6.4.1 Calc intersect as % of bp.i
      bp.area<-rep(0, length(int.area.E))
      for(ae in 1:length(adj.int.E)){
        bp.area[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                        / gArea(bp.i))
      }
      bp.area.int <- which(bp.area > .5)
      
      # 6.4.2 If no intersect at least half of bp.i
      if(length(bp.area.int) == 0){
        plB$Topo.Type[i] = "Relocated"
        plB$NbrChildren[i] <- (-99)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- "NA"
        plB$Parent[i] <- "NA"
      }
         
      # 6.4.3 If interest is more than half of bp.i
      if(length(bp.area.int) > 0){
        e.ref <- adj.int.E[which(bp.area == max(bp.area)), ]
        plB$Topo.Type[i] = "Delete - Join (S)"
        plB$NbrChildren[i] <- (-99)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- as.character(e.ref@data$CurrentZoning[1])
        plB$Parent[i] <- as.character(e.ref@data$PINX[1]) 
      }   
      
      # 6.4.4 for all intersect less than SizePar set inv.E to NULL
      inv.E <- NULL
      inv.area.E <- NULL
    }# Ends 6.4 If
    
    
# 6.5 Investigate those with intersects > SizePar ------------------------------
    
    # 6.5.1 Calc Involved e.par areas
    if(iae.ind > 0){
      inv.E <- adj.int.E[which(int.area.E > SizePar), ]
      inv.area.E <- int.area.E[which(int.area.E > SizePar)]
    }
    
    # 6.5.2 For those with no int.area    
    if(length(inv.area.E) == 0){
      plB$Topo.Type[i] = "Relocated"
      plB$NbrChildren[i] <- (-99)
      plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
      plB$End.Zone[i] <- "NA"
      plB$Parent[i] <- "NA"  
    }  
  
    # 6.5.3 For those with only one involved E.par
    if(length(inv.area.E) == 1){
  
      # If involved area is less that 1-SizePar
      if(inv.area.E < (1 - SizePar)){
        bp.area <- rep(0,length(int.area.E))
        for(ae in 1:length(adj.int.E)){
          bp.area[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                          / gArea(bp.i))
        }
        
        e.ref <- adj.int.E[which(bp.area == max(bp.area)),]
        plB$Topo.Type[i] = "Delete - Join"
        plB$NbrChildren[i] <- (-99)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- as.character(e.ref@data$CurrentZoning[1])
        plB$Parent[i] <- as.character(e.ref@data$PINX[1]) 
      }  
      
      # If involved area is more than 1-SizePar
      if(inv.area.E >= (1-SizePar)){
        bp.area<-rep(0,length(int.area.E))
        for(ae in 1:length(adj.int.E)){
          bp.area[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                          / gArea(bp.i))
        }
        e.ref <- adj.int.E[which(bp.area == max(bp.area)), ]
        plB$Topo.Type[i] = "Delete - Renamed" 
        plB$NbrChildren[i] <- (-99)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- as.character(e.ref@data$CurrentZoning[1])
        plB$Parent[i] <- as.character(e.ref@data$PINX[1])   
      }  
  
    }# Ends 6.5.3 If  
  
    
# 6.6 For those with more than one involved E.par ------------------------------
    
    if(length(inv.area.E) > 1){
      
    # 6.6.1 If none are more than .5, then it is the small end of a split
      if(length(which(inv.area.E > .5)) == 0){
        bp.area <- rep(0, length(int.area.E))
        for(ae in 1:length(adj.int.E)){
          bp.area[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                          / gArea(bp.i))
        }
        e.ref <- adj.int.E[which(bp.area == max(bp.area)), ]
        plB$Topo.Type[i] = "Delete - Lot Adj - Spl"
        plB$NbrChildren[i] <- (-99)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- as.character(e.ref@data$CurrentZoning[1])
        plB$Parent[i] <- as.character(e.ref@data$PINX[1]) 
      }  
      
      # 6.6.2 If only one is more than .5 then it is a rename and lot adj
      if(length(which(inv.area.E > .5)) == 1){ 
        bp.area <- rep(0, length(int.area.E))
        for(ae in 1:length(adj.int.E)){
          bp.area[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                          / gArea(bp.i))
        }
        e.ref <- adj.int.E[which(bp.area == max(bp.area)),]
        plB$Topo.Type[i] = "Delete - Lot Adj - Renamed"
        plB$NbrChildren[i] <- (-99)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- as.character(e.ref@data$CurrentZoning[1])
        plB$Parent[i] <- as.character(e.ref@data$PINX) 
      } 
      
      # 6.6.3 If more than one is .5, then it is a delete and split
      if(length(which(inv.area.E > .5)) > 1){ 
        bp.area <- rep(0, length(adj.int.E))
        for(ae in 1:length(adj.int.E)){
          bp.area[ae] <- (gArea(gIntersection(bp.i, adj.int.E[ae, ])) 
                             / gArea(adj.int.E[ae, ]))
        }
        e.ref <- adj.int.E[which(bp.area > .5),]
        plB$Topo.Type[i] = "Delete - Split"
        plB$NbrChildren[i] <- length(e.ref)
        plB$Beg.Zone[i] <- as.character(bp.i@data$CurrentZoning[1])
        plB$End.Zone[i] <- names(which(table(as.character(
               e.ref@data$CurrentZoning)) == max(table(as.character(
                 e.ref@data$CurrentZoning)))))[1]
        plB$Parent[i] <- bp.i@data$PINX 
      } 
    }# Ends 6.6 If 
  } # Ends 6.4 If
}# Ends 6.1 Loop

# 6.7 Finds the data within the files and fix missing years --------------------

  plB <- parcelFinder(plB, "B", Beg.Year, End.Year)
  plB$PChng.Year <- yearFix(plB$PChng.Year)

################################################################################
# 7.0 Label the "E" Parcels    -------------------------------------------------

# 7.1 Start Loop through All E Parcels -----------------------------------------

for (i in 1:dim(plE)[1]){
  
  # 7.1.1 Set up initial beginning and ending parcels 
  ep.i <- end[end@data$PINX == plE$PINX[i], ]

# 7.2 Test for multiple parcels ------------------------------------------------
  
  if(length(ep.i) > 1){
    plE$Topo.Type[i] <- "Multi-Polygon Parcel"
    plE$NbrChildren[i] <- (-99)
    plE$Beg.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
    plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
    plE$Parent[i] <- "NA"
  }
 
  
# 7.3 If not Multi-parcel polygon ----------------------------------------------
  
  if(length(ep.i) == 1){
    
   # 7.3.1 Limit to parcels in large area (speeds up calcs)
    area.par.B <- beg[beg@data$X > (ep.i@bbox[1, 1] - AreaPar)
                      & beg@data$X < (ep.i@bbox[1, 2] + AreaPar)
                      & beg@data$Y > (ep.i@bbox[2, 1] - AreaPar)
                      & beg@data$Y < (ep.i@bbox[2, 2] + AreaPar),]

    ap.B.int <- length(which(gIntersects(ep.i, area.par.B, byid=T)))
    
   # 7.3.2 When ep.i intersects nothing
    if(ap.B.int == 0){
      adj.int.B <- 0
      int.area.B <- 0
      iab.ind <- (-1)
      plE$Topo.Type[i] = "Relocated"
      plE$NbrChildren[i] <- (-99)
      plE$Beg.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
      plE$Parent[i] <- "NA"
    }
    
   # 7.3.3 When ep.i intersects, calculate the % intersect
    if(ap.B.int != 0){ 
      adj.int.B <- area.par.B[which(gIntersects(ep.i, area.par.B, byid=T)), ]
      int.area.B<-rep(0,length(adj.int.B))
      for(ab in 1:length(adj.int.B)){
        int.area.B[ab] <- (gArea(gIntersection(ep.i, adj.int.B[ab,])) 
                           / gArea(adj.int.B[ab,]))
      }
            
    # Set indicator if intersect > SizePar
      iab.ind <- length(which(int.area.B > SizePar))
    } # Ends 7.3.3 If

# 7.4 If not intersects more than SizePar --------------------------------------
    
  if(iab.ind == 0){
      
      # 7.4.1 Calc intersect as % of ep.i
      ep.area<-rep(0,length(int.area.B))
      for(ab in 1:length(adj.int.B)){
        ep.area[ab] <- (gArea(gIntersection(ep.i, adj.int.B[ab,])) 
                        / gArea(ep.i))
      }
            
      ep.area.int <- which(ep.area > .5)
      
      # 7.4.2 If no intersect at least half of bp.i
      if(length(ep.area.int) == 0){
        plE$Topo.Type[i] = "Relocated"
        plE$NbrChildren[i] <- (-99)
        plE$Beg.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
        plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
        plE$Parent[i] <- "NA"
      }
      
      # 7.4.3 If interest is more than half of bp.i  
      if(length(ep.area.int) > 0){
        b.ref <- adj.int.B[which(ep.area.int == max(ep.area.int)), ]
        plE$Topo.Type[i] = "New - Split (S)"
        plE$NbrChildren[i] <- (-99)
        plE$Beg.Zone[i] <- as.character(b.ref@data$CurrentZoning[1])
        plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
        plE$Parent[i] <- b.ref@data$PINX[1]
      }   
      
      # 7.4.4 for all intersect less than SizePar set inv.E to NULL
      inv.B <- NULL
      inv.area.B <- NULL
    
  }# Ends 7.4 If
    
    
# 7.5 Investigate those with intersects > SizePar ------------------------------
    
  # 7.5.1 Calc Involved e.par areas
   if(iab.ind > 0){
      inv.B <- adj.int.B[which(int.area.B > SizePar),]
      inv.area.B <- int.area.B[which(int.area.B > SizePar)]
   }
    
  # 7.5.2 For those with no int.area    
   if(length(int.area.B) == 0){
       plE$Topo.Type[i] = "Relocated"
       plE$NbrChildren[i] <- (-99)
       plE$Beg.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
       plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
       plE$Parent[i] <- "NA"
   }  
    
  # 7.5.3 For those with only one involved E.par
   if(length(int.area.B) > 0 & length(inv.area.B) == 1){
      
   # If involved area is less that 1-SizePar
     if(inv.area.B < (1 - SizePar)){
       b.ref <- inv.B[1, ]
       plE$NbrChildren[i] <- (-99)
       plE$Beg.Zone[i] <- as.character(b.ref@data$CurrentZoning[1])
       plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
       plE$Parent[i] <- b.ref@data$PINX[1]
       plE$Topo.Type[i] = "New - Split"
     }  
      
    # If involved area is more than 1-SizePar
     if(inv.area.B >= (1 - SizePar)){
       b.ref <- inv.B[1, ]
       plE$NbrChildren[i] <- (-99)
       plE$Beg.Zone[i] <- as.character(b.ref@data$CurrentZoning[1])
       plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
       plE$Parent[i] <- b.ref@data$PINX[1]
       plE$Topo.Type[i] = "New - Renamed"
     }  
      
   }# Ends 7.5.3 If  
    
 # 7.5.4 Those with more than one inv B
   if(length(int.area.B) > 0 & length(inv.area.B) > 1){
      
    # Split and Join situation
     if((length(which(inv.area.B > SizePar)) +
          length(which(inv.area.B < (1 - SizePar)))) > 1){
       bp.area <- rep(0, length(int.area.B))
       for(ab in 1:length(adj.int.B)){
         bp.area[ab] <- (gArea(gIntersection(ep.i, adj.int.B[ab,])) 
                          / gArea(ep.i))
       }
       b.ref <- adj.int.B[which(bp.area == max(bp.area)),]
       plE$Topo.Type[i] = "New - Split + Join" 
       plE$NbrChildren[i] <- (-99)
       plE$Beg.Zone[i] <- as.character(b.ref@data$CurrentZoning[1])
       plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
       plE$Parent[i] <- b.ref@data$PINX[1]
     }
     
    # Split Situation
     if(length(which(inv.area.B > SizePar & inv.area.B < (1 - SizePar))) == 1){
       bp.area <- rep(0,length(int.area.B))
       for(ab in 1:length(adj.int.B)){
         bp.area[ab] <- (gArea(gIntersection(ep.i, adj.int.B[ab, ])) 
                         / gArea(ep.i))
       }
                
       b.ref <- adj.int.B[which(bp.area==max(bp.area)),]
       plE$NbrChildren[i] <- (-99)
       plE$Beg.Zone[i] <- as.character(b.ref@data$CurrentZoning[1])
       plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
       plE$Parent[i] <- b.ref@data$PINX[1]
       plE$Topo.Type[i] = "New - Split"  
     }
            
     # Small Split
     if(length(which(inv.area.B > SizePar)) == 0){
        bp.area <- rep(0,length(int.area.B))
        for(ab in 1:length(adj.int.B)){
          bp.area[ab] <- (gArea(gIntersection(ep.i, adj.int.B[ab, ])) 
                          / gArea(ep.i))
        }
        b.ref <- adj.int.B[which(bp.area==max(bp.area)),]
        plE$Beg.Zone[i] <- as.character(b.ref@data$CurrentZoning[1])
        plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
        plE$Parent[i] <- b.ref@data$PINX[1]
        plE$Topo.Type[i] = "New - Split (S)"
     }
      
  # 7.5.5 Join
     if(length(which(inv.area.B > SizePar)) >= 1){
      
      # Straight Join
      if(length(which(inv.area.B >= .5)) > 1){
         bp.area <- rep(0,length(adj.int.B))
         for(ab in 1:length(adj.int.B)){
           bp.area[ab] <- (gArea(gIntersection(ep.i, adj.int.B[ab, ])) 
                               / gArea(adj.int.B[ab,]))
         }          
         b.ref <- adj.int.B[which(bp.area > .5), ]
         plE$Topo.Type[i] = "New - Join"
         plE$NbrChildren[i] <- length(b.ref)
         plE$Beg.Zone[i] <-   names(which(table(as.character(
                         b.ref@data$CurrentZoning)) == max(table(as.character(
                             b.ref@data$CurrentZoning)))))[1]
         plE$End.Zone[i] <- as.character(ep.i@data$CurrentZoning[1])
         plE$Parent[i] <- "NA" 
      } 
     }# Ends 7.5.5 If
    }# Ends 7.5.4 If      
  } # Ends 7.3 If 
}# Ends 7.1 Loop

# 7.6 Finds the data within the files and fix missing years --------------------

  plE <- parcelFinder(plE, "E", Beg.Year, End.Year)
  plE$PChng.Year <- yearFix(plE$PChng.Year)

################################################################################
# 8.0 Put back together and write out ------------------------------------------

# 8.1 Add Change Variables to plA ----------------------------------------------

  plA$PChng.Year <- 0
  plA.Cons$PChng.Year <- 0

# 8.2 Comine all together ------------------------------------------------------

  Parcel.List <-rbind(plA.Cons, plA, plB, plE)

# 8.3 Return Results -----------------------------------------------------------
 
  return(Parcel.List)

}

