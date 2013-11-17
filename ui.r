################################################################################
#                                                                              #
#   User Interface for Shiny App.  Displays simulated coxPH                    #
#   results from Redevelopment Analysis in City of Seattle                     #
#                                                                              #
#   by Andy Krause:  11/17/2013                                                #
#                                                                              #
################################################################################

# Set library(s)

  library(shiny)

# Define UI for miles per gallon application

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Relative Hazards: Compared to Mean"),
  
  # Sidebar with controls to select the model and the variable to display

  sidebarPanel(
    selectInput("model", "Model:",
                list("Model 1a" = "Mod.1a", 
                     "Model 1b" = "Mod.1b",
                     "Model 2" = "Mod.2",
                     "Model 3a" = "Mod.3a",
                     "Model 3b" = "Mod.3b",
                     "Model 3c" = "Mod.3c",
                     "Model 4" = "Mod.4")),
    
    selectInput("variable", "Variable:",
                list("Urban Center Village" = "UCV",
                     "Hub Urban Village" = "HUV",
                     "Residential Urban Village" = "RUV",
                     "LowRise2" = "LR2",
                     "LowRise3" = "LR3",
                     "RC Overlay" = "RC.Overlay",
                     "Structure SqFt" = "LNSF",
                     "Building Grade" = "BldgGrade",
                     "Building Condition" = "B.Cond",
                     "Year Built" = "B.YearBuilt",
                     "Extra Units" = "X.Units",
                     "Acc. Dwelling Unit" = "ADU",
                     "Topography" = "Topography",
                     "View" = "View",
                     "Lot Size" = "LNLotSF",
                     "Lot Shape" = "ShapeFactor",
                     "FAR Difference" = "FAR.Dif",
                     "Median Income" = "MedInc",
                     "Land Use Mix" = "LUMIX",
                     "Neighborhood Density" = "Nbh.FAR",
                     "Adj to Non Res Use" = "Adj.CIMUse",
                     "Intersection Density" = "Int.Dens",
                     "Land Leverage" = "LandLev",
                     "Neighborhood Price Trend" = "Nbh.Trend",
                     "Neighborhood Price Uncertainty" = "Nbh.Uncert",
                     "Neighborhood Redev Activity" = "Nbh.Redev",
                     "Adj to Redev Activity" = "Adj.Redev"
                     ))
   
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),  
    plotOutput("Redev.Plot", width="100%")
  )
))
