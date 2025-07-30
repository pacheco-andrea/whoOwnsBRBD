#### Biodiversity and Tenure in Brazil ####

# instead of extracting the biodiversity pixels to each of the tenure categories' parcels/polygons
# i need to run a very simple check where i just create boxplots from the biodiversity pixels themselves, 
# each under the different tenure categories 
# how do i do this/what's the best approach here?

# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)

# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# biodiversity data ----
# using the raw data here - 
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20241010"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
biodiv <- rast(rasters)
names(biodiv) <- gsub("_BR.tif", "", rasters)

# get biomes
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]
st_crs(biomes)
biomes <- biomes[,1]

# reproject to equal area
biomes <- st_transform(biomes, my_crs_SAaea)
biodiv <- terra::project(biodiv, biomes)


# tenure raster ----
# simplest would also be to use the tenure rasters
# get tenure rasters: ----


setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- list.files()
tifs <- grep(".tif$", l)
R1km <- l[tifs][grep("1km", l[tifs])] 
# exclude SIGEF and SNCI properties
t <- lapply(R1km[-grep("properties", R1km)], rast)
# terra::plot(t)
names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", R1km[-grep("properties", R1km)]))

# stack the rasters ----

t_reprojected <- project(t[[1]], crs(mask))
crs(t_reprojected) == crs(mask)
ext(t_reprojected)
ext(mask)
ext(biodiv[[1]])

# extend biodiversity to match tenure
bio_extended <- terra::extend(biodiv[[1]], t_reprojected, snap = "out", fill = NA)
ext(bio_extended)
bio_ext_cropped <- terra::crop(bio_extended, t_reprojected)
ext(bio_ext_cropped)

mask_test <- mask(bio_ext_cropped, t_reprojected)
# ok still not working...

# my options are:
# resample (introduce error)
# try masking with the polygons in the workflow of the extraction... >>> this was the most straightforward way!

