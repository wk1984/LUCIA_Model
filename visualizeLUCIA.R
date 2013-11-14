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
  par.clip <- selectByArea(parcels, clip)
  
  if(toAdd){
   plot(par.clip, bor=bor.col, col=fill.col, lwd=lWd, add=T)
  }
  
  if(!toAdd){
    plot(par.clip, bor=bor.col, col=fill.col, lwd=lWd, add=T)
  }
}

# 2.0 Plotting with changes shown (TO COME) ------------------------------------




# 3.0 Plot Shapefiles in RGL ---------------------------------------------------
# 
# rgl2dPlot <- function(parcel, height, color, lWd){
#   
#   
# 
# rgl.shapefile(par.beg, 0, "gray80", 1)
# 








