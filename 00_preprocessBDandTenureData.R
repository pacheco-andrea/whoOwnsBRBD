#### Biodiversity and Tenure in Brazil ####
# script that reads updated version of land tenure data (data is from 2022, but we've received the processed version in 10.2023)
# and ubirajara's new biodiversity indicator data (version 10.2023)
# output is simplified and harmonized versions of these data: simple columns of land tenure, and reprojected rasters to south america albers equal area
# I rasterize the tenure polygons, but only for mapping, not for accounting of areas
# author: Andrea Pacheco
# first run: 23.09.2022
# last run: 10.10.2023

# libraries
library(terra)
library(sf)
library(dplyr)


# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Tenure data ----
# (STILL MISSING OTHER CATEGORIES OF LAND TENURE?)
# data is split into a .shp for each state. These were sourced from CSR UFMG and downloaded manually

setwd(paste0(wdmain,"/data/raw/LandTenure_v20231009/"))
l <- list.files()
files <- l[-c(grep(".toml", l))]

# 1. read and write out simpler files # HERE FIGURE OUT IF I SHOULD WRITE OUT AS JSON OR SQLITE????
# make sure to keep state-by-state structure in order to facilitate future parallelization 


# loop through reading and writing simplified shapefiles
for (i in 1:length(unique(files[grep(".shp",files)])))
{
  setwd(paste0(wdmain,"/data/raw/LandTenure_v20231009/"))
  shps <- grep(".shp", files)  # read in all the shapefiles (with only specific columns established in the read_my_json funct above), bind rows into one df
  # read one shape at a time
  stateShp <- read_my_shp(files[shps[i]])
  # keep same name
  name <- paste(files[shps[i]])
  # write out
  setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023/"))
  st_write(stateShp, name)
  print(i)
}

# Biodiversity indicator data ----
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
list.files()
richness <- rast("Species_Richness_All_groups_MARS.tif")
richness
plot(richness)
res(richness) # .01*.01degrees - which means at ~1km. Now it's in WGS84. 
# transform to south america albers equal area for making calculations 
# test first with one:
my_crs <- terra::crs(stateShp, proj = TRUE)
# richness_eq <- project(richness, my_crs)
# plot(richness_eq)

setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
rastFiles <- grep(".tif$", list.files())
list.files()[rastFiles]
for(i in 1:length(rastFiles))
{
  setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
  r <- rast(paste0(list.files()[rastFiles][i]))
  r2 <- project(r, my_crs)
  name <- gsub(".tif", "_SAalbers.tif", paste0(list.files()[rastFiles][i]))
  print(i)
  setwd(paste0(wdmain,"/data/processed/BiodivIndicators_SA-albers"))
  writeRaster(r2, filename = name)
}

# rasterize tenure data ----
# THIS SHOULD BE FOR PLOTTING MAPS - NOT FOR CALCULATIONS!
# EDIT HERE WHEN ADDING NEW TENURE CATEGORIES
# make mask
mask <- r2*0

setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023/"))
# make a raster for each of the shapes
rlist <- list()
for (i in 1:length(grep(".shp",list.files()))) 
{
  shps <- grep(".shp", list.files())  
  stateShp <- read_my_shp(list.files()[shps[i]])
  rlist[[i]] <- rasterize(stateShp, mask, "tipo")
}
# merge individual states into one whole map of brazil
br <- merge(sprc(rlist)) # needs to be a SpatRaster Collection first 
plot(br) # it's fine that is numerizes categories because categories will be numbered alphabetically.
setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(br, filename = "landTenure_AST-IRU-PCT_SAalbers_1km.tif")


