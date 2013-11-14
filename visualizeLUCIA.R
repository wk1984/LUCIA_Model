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
    plot(par.clip, bor=bor.col, col=fill.col, lwd=lWd)
  }
}

# 2.0 Plotting with changes shown (TO COME) ------------------------------------

changePlot  <- function(parcels, clip, change.file, break.field,
                        c.breaks, c.col, c.pch, cex.par){
  
  # Clip to desired area
  par.clip <- selectByArea(parcels, clip)
  
  # Remove Road parcels, if any
  par.clip <- par.clip[substr(par.clip@data$PINX, 9, 12) != "5555", ] 
  
  # Merge Changes to Parcel
  par.chng <- mergeShapeFile(par.clip, change.file, "PINX", "PINX")
  
  # assign colors and shapes
  par.chng$Pcol <- c.col[1]
  par.chng$Pshp <- c.pch[1]
  break.data <- par.chng@data[,which(names(par.chng@data) == break.field)]
  for(i in 2:length(c.breaks)){
    par.chng$Pcol[break.data > c.breaks[i]] <- c.col[i]
    par.chng$Pshp[break.data > c.breaks[i]] <- c.pch[i]
  }

  # Plot Change Map

  # Plot Change Boundary
  plot(clip, bor="gray20", lwd=2)

  # Plot Base Parcels
  plot(par.clip, bor="white", col="gray90", add=T)

  # Plot Change Points
  points(par.chng$X, par.chng$Y,
          col=par.chng@data$Pcol,
          pch=par.chng@data$Pshp,
          cex=((break.data) ^ (1 / 3) / cex.par)
  )

}

## Adding legends: TO COME

# 3.0 Plot Shapefiles in RGL ---------------------------------------------------
 
rgl2dPlot <- function(parcel, height=10, r.col=1, lWd=1, bg.color="white"
                      , toAdd=FALSE){
  
  # Remove road parcels, if any
  parcel <- parcel[substr(parcel@data$PINX, 9, 12) != "5555", ] 
  
  # If adding to existing
  if(toAdd){   
    bg3d(bg.color)
    rgl.shapefile(parcel, height, r.col, lWd)
  }
 
  # If creating new
  if(!toAdd){
    rgl.open()   
    bg3d(bg.color)
    rgl.shapefile(parcel, height, r.col, lWd)
  }
}

# 4.0 Plot Structures in RGL ---------------------------------------------------

rgl3dPlot <- function(build.parcels, chng.table, b.SF, b.ESF, b.Height,
                      b.StoryHeight, b.col="white", b.alpha=.5, b.roof=F){
  
 # add data to table
 build.parcels <- mergeShapeFile(build.parcels, chng.table, "PINX", "PINX",
                                 allX=F)
  
 # find Offset
  build.me <- findOffset(build.parcels, SF=b.SF, Height = b.Height
                         , ESF = b.ESF)
  
 # extract stories
  stories <- build.me@data[,which(names(build.me@data)==b.Height)]
  
 # plot buildings
  rgl.polygon(build.me, stories * b.StoryHeight, b.col, b.alpha,
            build.me@data$Offset, roof=b.roof)
}






