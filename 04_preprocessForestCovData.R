#### Biodiversity and Tenure in Brazil ####

# this script pre-processes biodiversity indicator data produced by Ubirajara Oliveira at CSR
# outputs are:
# 1. maps of biodiversity in south america equal albers
# 2. re-proected version of the data i will actually use: richness, endemisms, and the losses of these


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)
library(viridis)
library(classInt)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# Biodiversity indicators ----
setwd(paste0(wdmain,"/data/raw/landCover_mapbiomas"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
landCover <- rast(rasters)
res(landCover) # .0002 degrees 

# reclassify this raster into forest, agriculture, and other

reclass_matrix <- matrix(c(
  1, 10,    # Category 1: values 1 -> 10
  3, 10,    # Category 1: values 3 -> 10
  4, 10,    # Category 1: values 4 -> 10
  5, 10,    # Category 1: values 5 -> 10
  6, 10,    # Category 1: values 6 -> 10
  49, 10,   # Category 1: values 49 -> 10
  10, 10,   # Category 1: values 10 -> 10
  11, 10,   # Category 1: values 11 -> 10
  12, 10,   # Category 1: values 12 -> 10
  32, 10,   # Category 1: values 32 -> 10
  29, 10,   # Category 1: values 29 -> 10
  50, 10,   # Category 1: values 50 -> 10
  14, 20,   # Category 2: values 14 -> 20
  15, 20,   # Category 2: values 15 -> 20
  18, 20,   # Category 2: values 18 -> 20
  19, 20,   # Category 2: values 19 -> 20
  39, 20,   # Category 2: values 39 -> 20
  20, 20,   # Category 2: values 20 -> 20
  40, 20,   # Category 2: values 40 -> 20
  62, 20,   # Category 2: values 62 -> 20
  41, 20,   # Category 2: values 41 -> 20
  36, 20,   # Category 2: values 36 -> 20
  46, 20,   # Category 2: values 46 -> 20
  47, 20,   # Category 2: values 47 -> 20
  35, 20,   # Category 2: values 35 -> 20
  48, 20,   # Category 2: values 48 -> 20
  9, 20,    # Category 2: values 9 -> 20
  21, 20   # Category 2: values 21 -> 20
), ncol = 2, byrow = TRUE)

# Reclassify the 1985 raster
# u <- unique(landCover[[2]])
forestCover85 <- terra::classify(landCover[[1]], reclass_matrix, others = 0)
# u85 <- unique(forestCover85)
setwd(paste0(wdmain,"/data/processed/forestCover"))
writeRaster(forestCover85, "forestCover1985.tif", filetype="GTiff", overwrite=TRUE)

# reclassify the 2023 raster
reclass_matrix <- matrix(c(
  1, 1,    # Category 1: values 1 -> 10
  3, 1,    # Category 1: values 3 -> 10
  4, 1,    # Category 1: values 4 -> 10
  5, 1,    # Category 1: values 5 -> 10
  6, 1,    # Category 1: values 6 -> 10
  49, 1,   # Category 1: values 49 -> 10
  10, 1,   # Category 1: values 10 -> 10
  11, 1,   # Category 1: values 11 -> 10
  12, 1,   # Category 1: values 12 -> 10
  32, 1,   # Category 1: values 32 -> 10
  29, 1,   # Category 1: values 29 -> 10
  50, 1,   # Category 1: values 50 -> 10
  14, 2,   # Category 2: values 14 -> 20
  15, 2,   # Category 2: values 15 -> 20
  18, 2,   # Category 2: values 18 -> 20
  19, 2,   # Category 2: values 19 -> 20
  39, 2,   # Category 2: values 39 -> 20
  20, 2,   # Category 2: values 20 -> 20
  40, 2,   # Category 2: values 40 -> 20
  62, 2,   # Category 2: values 62 -> 20
  41, 2,   # Category 2: values 41 -> 20
  36, 2,   # Category 2: values 36 -> 20
  46, 2,   # Category 2: values 46 -> 20
  47, 2,   # Category 2: values 47 -> 20
  35, 2,   # Category 2: values 35 -> 20
  48, 2,   # Category 2: values 48 -> 20
  9, 2,    # Category 2: values 9 -> 20
  21, 2   # Category 2: values 21 -> 20
), ncol = 2, byrow = TRUE)
forestCover23 <- terra::classify(landCover[[2]], reclass_matrix, others = 0)
# u23 <- unique(forestCover23)
setwd(paste0(wdmain,"/data/processed/forestCover"))
writeRaster(forestCover23, "forestCover2023.tif", filetype="GTiff", overwrite=TRUE)

# reread in
setwd(paste0(wdmain,"/data/processed/forestCover"))
f23 <- rast("forestCover2023.tif")
f85 <- rast("forestCover1985.tif")
terra::plot(f85)
terra::plot(f23)

# Map: ----
# i need a map that is 1) red 10-f85 that is now 20-ag23, green all 10-23
f2ag <- f23_2 + f85_2
setwd(paste0(wdmain,"/data/processed/forestCover"))
writeRaster(f2ag, "forest2agriculture.tif", filetype="GTiff", overwrite=TRUE)
f2ag <- rast("forest2agriculture.tif")

u <- unique(f2ag)
plot(f2ag)

# plot map 

f2agCols <- data.frame(0, "gray50",
                       20, "gray50", 
                       10, "green", 
                       11, "green",
                       21, "red",
                       22, "gray50",
                       12, "green")
terra::plot(f2ag, 
            type = "classes",
            axes=F,
            mar = c(0, 0, 0, 7.1),
            col = f2agCols)


setwd(paste0(wdmain, "output/maps/"))
png("richness_v20241004.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(f2ag, 
            type = "classes",
            axes=F,
            mar = c(0, 0, 0, 7.1),
            col = f2agCols)
dev.off()


