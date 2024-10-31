#### Biodiversity and Tenure in Brazil ####

# script that extracts the deforestation of 1985-2023 per different tenure regimes


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)
library(exactextractr)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# set up deforestation data ----

setwd(paste0(wdmain,"/data/processed/forestCover"))
forest <- rast("forest2agriculture.tif")

# i need the extraction to count how many cells are 12 (deforestation)
# and to count how many cells are 1, 10, 11, 21 (forest)
# everything else i dont need

reclass_matrix <- matrix(c(
1, 1,
10, 1,
11, 1,
21, 1, 
12, 2
), ncol = 2, byrow = TRUE)

forest <- terra::classify(forest, reclass_matrix, others = 0)

# make extraction function
listofShapes 

extractFor <- function(listOfShapes, directoryIn, directoryOut, crsBD, forest, outNamePrefix){
  
  # make loop to conduct extraction for list of shapes
  for(i in 1:length(listOfShapes))
  {
    # get data
    setwd(directoryIn)
    s <- st_read(listOfShapes[i])
    name <- gsub(".shp", "", listOfShapes[i])
    # get areas
    s <- st_transform(s, crsBD) 
    s$areakm2 <- as.numeric(st_area(s)/1000000)
    
    # extract
    f2a <- exactextractr::exact_extract(forest, s, function(values, coverage_fraction) {
      # Count the number of cells where raster value is 1
      count_1 <- sum(values == 1, na.rm = TRUE)
      # Count the number of cells where raster value is 2
      count_2 <- sum(values == 2, na.rm = TRUE)
      # Return as a data frame
      return(data.frame(for23 = count_1, defor = count_2))
    })
    # join data
    s2 <- cbind(st_drop_geometry(s), f2a)
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

extractFor(listOfShapes = list.files()[f],
          directoryIn = paste0(wdmain,"/data/processed/LT_no-overlaps/"),
          directoryOut = paste0(wdmain,"/data/processed/forestExtractions-perPolygon/"),
          crsBD = crs(forest),
          forest = forest,
          outNamePrefix = "public_no-overlaps_")

# extractions OVERLAPS  ----
setwd(paste0(wdmain,"/data/processed/LT_overlaps/"))
f <- grep(".shp", list.files())

extractFor(listOfShapes = list.files()[f], 
          directoryIn = paste0(wdmain,"/data/processed/LT_overlaps/"), 
          directoryOut = paste0(wdmain,"/data/processed/forestExtractions-perPolygon/"), 
          crsBD = crs(forest),
          forest = forest,
          outNamePrefix = "overlaps_")

# extractions OVERLAPS across public & private categs ----

setwd(paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"))
f <- grep(".shp", list.files())

extractFor(listOfShapes = list.files()[f], 
          directoryIn = paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"), 
          directoryOut = paste0(wdmain,"/data/processed/forestExtractions-perPolygon/"), 
          crsBD = crs(forest),
          forest = forest,
          outNamePrefix = "pubxpri_overlaps_")

# i've left this one for last because it takes the longest
# extractions PRIVATE no overlaps  ----
setwd(paste0(wdmain,"/data/processed/LT_no-overlaps_private/"))
f <- grep(".shp", list.files())

extractFor(listOfShapes = list.files()[f], 
          directoryIn = paste0(wdmain,"/data/processed/LT_no-overlaps_private/"), 
          directoryOut = paste0(wdmain,"/data/processed/forestExtractions-perPolygon/"), 
          crsBD = crs(forest),
          forest = forest,
          outNamePrefix = "private_no-overlaps_")

