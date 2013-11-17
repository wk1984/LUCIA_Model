################################################################################
#                                                                              #
#   Server for Shiny App.  Displays simulated coxPH                            #
#   results from Redevelopment Analysis in City of Seattle                     #
#                                                                              #
#   by Andy Krause:  11/17/2013                                                #
#                                                                              #
################################################################################

# load libraries

library(shiny)
library(simPH)
library(sp)

# load in necessary data

load(paste0("D://Data//Research//Dissertation//Redev_Paper//",
            "Redev_Analysis_Workspace.RData"))

# start server

shinyServer(function(input, output) {

 # Set Text  
  formulaText <- reactive({
    paste("Hazard Ratios for ", input$variable)
  })
  
 # Set output caption  
  output$caption <- renderText({
    formulaText()
  })
 
 # Create output plot  
  output$Redev.Plot <- renderPlot({
    
  # Get data/model from inputs
    x.model <- get(input$model)
    x.variable <- redev.data[ ,input$variable]

  # Determine if selected model has selected variable  
    x.inModel <- length(which(names(x.model$coef) == input$variable))
    
  # If variable not in Model
    if(x.inModel == 0){
      
      m.name <- names(input)[1]
      
      # Plot a blank chart with warning
      plot(c(0, 0, 1, 1),
           c(0, 1, 0, 1),
           col=0)
       text(.5, .5, 
            paste0(input$variable, " variable not in ", m.name),
            col=2, cex=2,
            yaxt='n', xaxt='n',xlab="", ylab="")
     }
    
  # If variable is in Model
    if(x.inModel != 0){
      
      # Set simulation resolution
       xmin <- min(x.variable)
       xmax <- max(x.variable)
       xseq <- (xmax - xmin) / 20
       
     # Simulate impact 
       Sim1 <- coxsimLinear(x.model,
                            b = input$variable, 
                            qi = "Hazard Ratio",
                            ci = 0.95,
                            Xj = seq(xmin, xmax, by = xseq),
                            Xl=rep(mean(x.variable), 21),
                            spin = TRUE)
       
     # Create Simulation Plot   
       simPlot <- simGG(Sim1, ribbons=TRUE, alpha=.5)
      
     # Add warning if Binary Variable
       if(min(x.variable) == 0 & max(x.variable) == 1){
         simPlot <- simPlot + annotate("text",
                                       x = .5,
                                       y = mean(Sim1$QI),
                                       label = "Results from a Binary Variable May be Meaningless",
                                       size= 8,
                                       color="red")
       }
       
     # Print out plot   
       print(simPlot)
     }
  })
})