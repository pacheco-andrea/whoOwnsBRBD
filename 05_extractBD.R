#### Biodiversity and Tenure in Brazil ####

# script that extracts the biodiversity indicator values per different tenure regimes
# output: 
# note run time? 

# issues:
# - need to filter out properties that are <1km

# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)
library(exactextractr)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# things to consider:
# what is the best solution for polygons with an area <1km?

# biodiversity data ----
# for whatever reason, the re-projected data wasnt working with the extraction
# using the raw data here - which is likely to be updated once again
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
biodiv <- rast(rasters[-grep("Beta", rasters)])
names(biodiv) <- gsub("_All_groups.*", "", rasters[-grep("Beta", rasters)])

#  extractions for PUBLIC lands with no overlaps ----
# just remember, not as simple as no actual overlaps...
# in the case of IRU + AST, IRU + undesignated i didn't identify the areas that didn't overlap and write these out individually
# however, from my calculations 
setwd(paste0(wdmain,"/data/processed/LT_no-overlaps/"))
f <- grep(".shp", list.files())

for(i in 1:length(f))
{
  setwd(paste0(wdmain,"/data/processed/LT_no-overlaps/"))
  s <- st_read(list.files()[f][i])
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # # check if there are empty geometries
  # empties <- grep("TRUE", is.na(st_dimension(s)))
  # if(length(empties) > 0) {s <- s[-empties,] }
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  lapply(bd, class)
  
  # join data to features
  s2 <- cbind(s, bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  st_write(s2, paste0("public_no-overlaps_", name), append = FALSE)
  print(name)
}

# extractions PRIVATE no overlaps  ----
setwd(paste0(wdmain,"/data/processed/LT_no-overlaps_private/"))
f <- grep(".shp", list.files())
for(i in 1:length(f))
{
  # get polygon data
  setwd(paste0(wdmain,"/data/processed/LT_no-overlaps/"))
  s <- st_read(list.files()[f][i])
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  # lapply(bd, class)
  
  # join data to features
  s2 <- cbind(s, bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  st_write(s2, paste0("public_no-overlaps_", name), append = FALSE)
  print(name)
}

# extractions OVERLAPS  ----

setwd(paste0(wdmain,"/data/processed/LT_overlaps/"))
f <- grep(".shp", list.files())
for(i in 1:length(f))
{
  # get polygon data
  setwd(paste0(wdmain,"/data/processed/LT_overlaps/"))
  s <- st_read(list.files()[f][i])
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  # lapply(bd, class)
  
  # join data to features
  s2 <- cbind(s, bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  st_write(s2, paste0("overlaps_", name), append = FALSE)
  print(name)
}

# extractions OVERLAPS across public & private categs ----

setwd(paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"))
f <- grep(".shp", list.files())
for(i in 1:length(f))
{
  # get polygon data
  setwd(paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"))
  s <- st_read(list.files()[f][i])
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  # lapply(bd, class)
  
  # join data to features
  s2 <- cbind(s, bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  st_write(s2, paste0("pubxpri_overlaps_", name), append = FALSE)
  print(name)
}
