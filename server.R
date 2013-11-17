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
 
save.image(paste0("D://Data//Research//Dissertation//LUCIA_Paper//",
                   "Seattle_LUCIA_Shiny_Workspace.RData"))

source("D://Code//R//Research//LUCIA_Model//visualizeLUCIA.R")

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
    
    Change.Table <- tabulateChanges(clip, beg.parcels, end.parcels, Par.Hist,
                                    Beg.Year, End.Year, CType="Raw")
    
    changePlot(end.parcels, clip, Change.Table, "Gain.Units",
               c(5, 25, 100), c(3, 4, 5), c(15, 15, 15), 3)
    plot(clip, lwd=2, bor=1, add=T)
    
  })
})