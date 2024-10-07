#### Biodiversity and Tenure in Brazil ####

# script that extracts the biodiversity indicator values per different tenure regimes


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)
library(exactextractr)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# biodiversity data ----
# using the raw data here - 
setwd(paste0(wdmain,"/data/raw/Biodiversity_v202410"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
biodiv <- rast(rasters)
names(biodiv) <- gsub("_BR.tif", "", rasters)

# make extraction function
extractBD <- function(listOfShapes, directoryIn, directoryOut, crsBD, biodiv, outNamePrefix){
  
  # make loop to conduct extraction for list of shapes
  for(i in 1:length(listOfShapes))
  {
    # get data
    setwd(directoryIn)
    s <- st_read(listOfShapes[i])
    name <- gsub(".shp", "", listOfShapes[i])
    # get areas
    s$areakm2 <- as.numeric(st_area(s)/1000000)
    s <- st_transform(s, crsBD) 
    # extract
    bd <- exactextractr::exact_extract(biodiv, s, "mean")
    # join data
    s2 <- cbind(st_drop_geometry(s), bd)
    # write out table
    setwd(directoryOut)
    write.csv(s2, file = paste0(outNamePrefix, name, ".csv"), row.names = FALSE)
  }
}


#  extractions for PUBLIC lands with no overlaps ----
# just remember, not as simple as no actual overlaps...
# in the case of IRU + AST, IRU + undesignated i didn't identify the areas that didn't overlap and write these out individually
# however, from my calculations 
setwd(paste0(wdmain,"/data/processed/LT_no-overlaps/"))
f <- grep(".shp", list.files())

extractBD(listOfShapes = list.files()[f],
          directoryIn = paste0(wdmain,"/data/processed/LT_no-overlaps/"),
          directoryOut = paste0(wdmain,"/data/processed/bdExtractions-perPolygon_v202410/"),
          crsBD = crs(biodiv[[1]]),
          biodiv = biodiv,
          outNamePrefix = "public_no-overlaps_")

# extractions OVERLAPS  ----
setwd(paste0(wdmain,"/data/processed/LT_overlaps/"))
f <- grep(".shp", list.files())

extractBD(listOfShapes = list.files()[f], 
          directoryIn = paste0(wdmain,"/data/processed/LT_overlaps/"), 
          directoryOut = paste0(wdmain,"/data/processed/bdExtractions-perPolygon_v202410/"), 
          crsBD = crs(biodiv[[1]]), 
          biodiv = biodiv, 
          outNamePrefix = "overlaps_")

# extractions OVERLAPS across public & private categs ----

setwd(paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"))
f <- grep(".shp", list.files())

extractBD(listOfShapes = list.files()[f], 
          directoryIn = paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"), 
          directoryOut = paste0(wdmain,"/data/processed/bdExtractions-perPolygon_v202410/"), 
          crsBD = crs(biodiv[[1]]), 
          biodiv = biodiv, 
          outNamePrefix = "pubxpri_overlaps_")

# i've left this one for last because it takes the longest
# extractions PRIVATE no overlaps  ----
setwd(paste0(wdmain,"/data/processed/LT_no-overlaps_private/"))
f <- grep(".shp", list.files())

extractBD(listOfShapes = list.files()[f], 
          directoryIn = paste0(wdmain,"/data/processed/LT_no-overlaps_private/"), 
          directoryOut = paste0(wdmain,"/data/processed/bdExtractions-perPolygon_v202410/"), 
          crsBD = crs(biodiv[[1]]), 
          biodiv = biodiv, 
          outNamePrefix = "private_no-overlaps_")

