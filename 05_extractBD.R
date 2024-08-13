#### Biodiversity and Tenure in Brazil ####

# script that extracts the biodiversity indicator values per different tenure regimes
# output: 
# note run time? 

# issues:
# because i will need to filter out properties that are <1km, i calculate the variable for area
# also, DONT WRITE THE GEOMETRY

# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)
library(exactextractr)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


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
  # get area of the features
  s$areakm2 <- st_area(s)/1000000
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  # join data to features
  s2 <- cbind(st_drop_geometry(s), bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  write.csv(s2, file = paste0("public_no-overlaps_", name), row.names = FALSE)
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
  s$areakm2 <- st_area(s)/1000000
  
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  # lapply(bd, class)
  
  # join data to features
  s2 <- cbind(st_drop_geometry(s), bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  write.csv(s2, file = paste0("public_no-overlaps_", name), row.names = FALSE)
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
  s$areakm2 <- st_area(s)/1000000
  
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")

  # join data to features
  s2 <- cbind(st_drop_geometry(s), bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  write.csv(s2, file = paste0("overlaps_", name), row.names = FALSE)
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
  s$areakm2 <- st_area(s)/1000000
  
  name <- list.files()[f][i]
  s <- st_transform(s, crs(biodiv[[1]]))
  
  # extraction
  bd <- exactextractr::exact_extract(biodiv, s, "mean")
  # lapply(bd, class)
  
  # join data to features
  s2 <- cbind(st_drop_geometry(s), bd)
  # lapply(s2, class)
  # plot(s2)
  
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  write.csv(s2, file = paste0("pubxpri_overlaps_", name), row.names = FALSE)
  print(name)
}
