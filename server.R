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

# load in necessary data

save.image(paste0("D://Data//Research//Dissertation//LUCIA_Paper//",
                  "Seattle_LUCIA_Workspace.RData"))

# start server

shinyServer(function(input, output) {

 # Set Text  
  formulaText <- reactive({
    paste0("Change Map for ", input$variable, " Urban Village")
  })
  
 # Set output caption  
  output$caption <- renderText({
    formulaText()
  })
 
 # Create output plot  
  output$Change.Plot <- renderPlot({
    
  # Get data/model from inputs
    x.model <- get(input$model)
    x.variable <- redev.data[ ,input$variable]

  })
})