################################################################################
#                                                                              #
#   Server for Shiny App.  Displays LUCIA Change Maps                          #
#   Example in City of Seattle                                                 #
#                                                                              #
#   by Andy Krause:  11/17/2013                                                #
#                                                                              #
################################################################################

# load libraries

library(shiny)
library(simPH)
library(sp)
library(RODBC)
library(maptools)
library(rgeos)
library(stringr)
library(rgl)

# load in necessary data
 
load(paste0("D://Data//Research//Dissertation//LUCIA_Paper//",
                   "Seattle_LUCIA_Shiny_Workspace.RData"))

source("D://Code//R//Research//LUCIA_Model//tabulateChanges.R")
source("D://Code//R//Research//LUCIA_Model//visualizeLUCIA.R")
source("D://Code//R//General//Toolsets//rgl_SpatialTools.R")
source("D://Code//R//General//R_Helpers//Spatial_Helpers.R")

# start server

shinyServer(function(input, output) {

 # Set Text  
  formulaText <- reactive({
    paste0("Change Map for ", input$urbanvillage, " Urban Village")
  })
  
 # Set output caption  
  output$caption <- renderText({
    formulaText()
  })
 
 #Create output plot  
  output$Change.Plot <- renderPlot({
    
    clip <- UVs[UVs@data$Code == input$urbanvillage,]
    clip.bb <- clip@bbox
    
    
    Change.Table <- tabulateChanges(clip, beg.parcels, end.parcels, Par.Hist,
                                    Beg.Year, End.Year, CType="Raw")
    
#     plot(c(clip.bb[1,1], clip.bb[1,2]), c(clip.bb[2,1], clip.bb[2,2]),col=0,
#          xaxt="n", yaxt="n", xlab="", ylab="")
      changePlot(end.parcels, clip, Change.Table, "Gain.Units",
               c(5, 25, 100), c(3, 4, 5), c(15, 15, 15), 3)
    
      legend("topleft", c("1-5 Units", "6-25 Units", "26+ Units"),
             col=c(3,4,5), pch=15, cex=.9)
      plot(clip, lwd=2, bor=1, add=T)
    
  })
})