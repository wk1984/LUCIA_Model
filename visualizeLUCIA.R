################################################################################
#                                                                              #           
#         LUCIA MODEL: visualizeLUCIA Functions()                              #                    
#         by Andy Krause                                                       #
#                                                                              #
#         A set of visualization functions for output                          #
#         from the LUCIA model                                                 #
#                                                                              #
#         Last Update: 11/14/2013                                              #
#                                                                              #
################################################################################

# 1.0 Base Plotting of Shapefiles ----------------------------------------------

basePlot <- function(parcels, clip, bor.col, fill.col, lWd, toAdd = FALSE){

  # Clip to desired area
  par.clip <- selectByArea(parcels, clip)
  
  # Remove Road parcels, if any
  par.clip <- par.clip[substr(par.clip@data$PINX, 9, 12) != "5555", ] 
  
  # If adding to existing
  if(toAdd){
   plot(par.clip, bor=bor.col, col=fill.col, lwd=lWd, add=T)
  }
  
  # If creating new
  if(!toAdd){
    plot(par.clip, bor=bor.col, col=fill.col, lwd=lWd, add=T)
  }
}

# 2.0 Plotting with changes shown (TO COME) ------------------------------------




# 3.0 Plot Shapefiles in RGL ---------------------------------------------------
 
rgl2dPlot <- function(parcel, height, r.col, lWd, toAdd=FALSE){
  
  # Remove road parcels, if any
  parcel <- parcel[substr(parcel@data$PINX, 9, 12) != "5555", ] 
  
  
  # If adding to existing
  if(toAdd){   
    rgl.shapefile(parcel, height, r.col, lWd)
  }
 
  # If creating new
  if(!toAdd){
    rgl.open()   
    rgl.shapefile(parcel, height, r.col, lWd)
  }

# 4.0 Plot Structures in RGL ---------------------------------------------------






