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
  headerPanel("Housing Unit Change by Development Size"),
  
  # Sidebar with controls to select the model and the variable to display

  sidebarPanel(
    selectInput("urbanvillage", "Urban Village:",
                list("12th Avenue" = "A12",
                     "23rd and Jackson" = "A23",
                     "Admiral" = "ADM",
                     "Aurora" = "AUR",
                     "Ballard" = "BAL",
                     "Belltown" = "BEL",
                     "Bitter Lake" = "BL",
                     "Capitol Hill" = "CH",
                     "ChinaTown/ID" = "CID",
                     "Columbia City" = "CCY",
                     "Commercial Core" = "CC",
                     "Crown Hill" = "CRH",
                     "Denny Triangle" = "DT",
                     "Eastlake" = "EAS",
                     "First Hill" = "FH",
                     "Fremont" = "FRE",
                     "Green Lake" = "GL",
                     "Greenwood/Phinney" = "GP",
                     "Lake City" = "LC",
                     "Madison/Miller" = "MM",
                     "MLK at Holly" = "MLK",
                     "Morgan Junctions" = "MJ",
                     "North Beacon Hill" = "NBH",
                     "North Rainier" = "NR",
                     "Northgate" = "NG",
                     "Pike/Pine" = "PP",
                     "Pioneer Square" = "PS",
                     "Rainier Beach" = "RB",
                     "Ravenna" = "RAV",
                     "Roosevelt" = "ROO",
                     "South Lake Union" = "SLU",
                     "South Park" = "SP",
                     "University Campus" = "UC",
                     "University NW" = "UNW",
                     "Upper Queen Anne" = "UQA",
                     "Uptown" = "UP",
                     "Wallingford" = "WAL",
                     "West Seattle Junction" = "WSF",
                     "Westwood/Highland Park" = "WH",                     
                     )),
    
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),  
    plotOutput("Change.Plot", width="100%")
  )
))
