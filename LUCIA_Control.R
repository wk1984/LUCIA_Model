################################################################################                                                                                                      ###                                                                          ###  
#                                                                              #
#    LUCIA MODEL: Command                                                      #
#                                                                              #
#       Runs the LUCIA Model                                                   #
#                                                                              #
#        Most Recent Update: 11/2/2013                                         #
#                                                                              # 
###############################################################################'
# 0.0 Load Files, Library and Set Parameters -----------------------------------

# 0.1 Load Libraries -----------------------------------------------------------

  library(RODBC)
  library(maptools)
  library(rgeos)
  library(stringr)
  library(rgl)

# 0.2 Load Files ---------------------------------------------------------------

  source(paste0("D://Code//R//General//Geographic//WA//KingCounty//",
              "Coding_Functions.R"))  
  source("D://Code//R//Research//LUCIA_Model//Parcel_Geometry.R")
  source("D://Code//R//Research//LUCIA_Model//Parcel_History.R")
  source("D://Code//R//Research//LUCIA_Model//tabulateChanges.R")

# To Come
  #source("c://Dropbox//Code//WA//KingCounty//PSS_Model//Tabular_Change.R")
  #source("c://Dropbox//Code//WA//KingCounty//PSS_Model//Parcel_History.R")
  #source("c://Dropbox//Code//WA//KingCounty//PSS_Model//Change_Counter.R")

# 0.3 Set Parameters -----------------------------------------------------------

  Beg.Year <- 1999
  End.Year <- 2013

################################################################################
# 1.0 Read in and Prepare Necessary GIS Files ----------------------------------

# 1.1 Read in Parcel Files -----------------------------------------------------

  beg.parcels <- readShapePoly(paste0("D://Data//Wa//seattle//geographic//",
                                      "parcels//seattle_parcels_", Beg.Year,
                                      ".shp"))

  end.parcels <- readShapePoly(paste0("D://Data//Wa//seattle//geographic//",
                                      "parcels//seattle_parcels_", End.Year,
                                      ".shp"))

# 1.2 Read in Boundary Files ---------------------------------------------------

  Seattle <- readShapePoly(paste0("D://Data//wa//seattle//geographic//",
                                        "boundaries//Seattle_Big_Boundary.shp"))

  UVs <- readShapePoly(paste0("D://Data//wa//seattle//geographic//",
                                       "UrbanVillages//All_Urban_Villages.shp"))

# 1.3 Add XY To Parcels --------------------------------------------------------

 # 1.3.1 Beg Year
  bXY <- t(sapply(slot(beg.parcels, "polygons"), function(i) slot(i, "labpt")))
  beg.parcels@data$X <- bXY[ ,1]
  beg.parcels@data$Y <- bXY[ ,2]

 # 1.3.2 Beg Year
  eXY <- t(sapply(slot(end.parcels, "polygons"), function(i) slot(i, "labpt")))
  end.parcels@data$X <- eXY[ ,1]
  end.parcels@data$Y <- eXY[ ,2]

# 1.4 Add PINX to Parcels ------------------------------------------------------

  beg.parcels@data$PINX <- paste0("..", beg.parcels@data$PIN)
  end.parcels@data$PINX <- paste0("..", end.parcels@data$PIN)

# 1.5 Read in Tabular Data -----------------------------------------------------

 # 1.5.1 Beginning Year
  odbc <- odbcConnectAccess2007(paste0("D://data//wa//king//assessor//king",
                                Beg.Year + 1, ".accdb"))
  B.Data <- sqlQuery(odbc, paste0("SELECT Major, Minor, PresentUse,",
                                "CurrentZoning, SqFtLot FROM Parcel", 
                                  Beg.Year + 1))
  B.Data <- pinCreate(B.Data)
  odbcClose(odbc)

 # 1.5.2 End Year
  odbc <- odbcConnectAccess2007(paste0("D://data//wa//king//assessor//king",
                                     End.Year, ".accdb"))
  E.Data <- sqlQuery(odbc, paste0("SELECT Major, Minor, PresentUse,",
                                  "CurrentZoning, SqFtLot FROM Parcel", 
                                  End.Year))
  E.Data <- pinCreate(E.Data)
  odbcClose(odbc)

################################################################################
# 2.0 Calculate Parcel ListParcel Geometry and History -------------------------

# 2.1 Select Area of Interest

  clip <- UVs[1,]

# 2.2 Calculate Parcel List

  Parcel.List <- createParcelList(Beg.Year, End.Year)

# 2.3 Calculate Parcel Geometry

  Par.Geom <- calculateGeometry(clip, Beg.Year, End.Year, beg.parcels, 
                                end.parcels, B.Data, E.Data, .1, 3000)

# 2.4 Calculate Parcel History

  Par.Hist <- createParcelHistory(Parcel.List, Par.Geom, Beg.Year, End.Year)

################################################################################
# # 3.0 Calculate Raw Tabular Changes ------------------------------------------
 
  Change.Table <- tabulateChanges(clip, beg.parcels, end.parcels, Par.Hist,
                               Beg.Year, End.Year, CType="Raw")
 
################################################################################
# 4.0 Calculate Analytical Tables ----------------------------------------------

# 4.1 One dimensional Analysis -------------------------------------------------

  Size.Change <- tabulateChanges(clip, beg.parcels, end.parcels, Par.Hist,
                              Beg.Year, End.Year, CType="Size",
                              par=list(c(5, 25, 100), c(5000, 50000)))

# 4.2 Two dimensional analysis -------------------------------------------------

  PT.Change <- tabulateChanges(clip, beg.parcels, end.parcels, Par.Hist,
                            Beg.Year, End.Year, CType="Process.Time")

################################################################################
