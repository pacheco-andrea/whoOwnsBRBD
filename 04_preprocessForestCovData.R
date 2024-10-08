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

# Reclassify the raster
# u <- unique(landCover[[1]])
forestCover85 <- terra::classify(landCover[[1]], reclass_matrix, others = 30)
u85 <- unique(forestCover85)

forestCover23 <- terra::classify(landCover[[2]], reclass_matrix, others = 30)
u23 <- unique(forestCover23)


# Save the reclassified raster (optional)
setwd(paste0(wdmain,"/data/processed/forestCover"))
writeRaster(forestCover85, "forestCover1985.tif", filetype="GTiff", overwrite=TRUE)
writeRaster(forestCover23, "forestCover2023.tif", format="GTiff", overwrite=TRUE)


# CONTINUE HERE
# map and write out a map of forest cover in 1985, and 2023
plot(forestCover85)



breaks <- classIntervals(values(richness, na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("richness_v20241004.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(richness[[1]], 
     type = "continuous",
     range = c(min(values(richness), na.rm = T),max(values(richness), na.rm = T)),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = viridis(n = as.integer(max(values(richness), na.rm = T)-min(values(richness), na.rm = T)), direction = -1))
dev.off()


# map and write out endemism map
summary(endemism)
plot(endemism)

breaks <- classIntervals(values(endemism, na.rm = T))
breaks$brks


setwd(paste0(wdmain, "output/maps/"))
png("endemism_v20241004.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(endemism[[1]], 
     type = "continuous",
     range = c(0,1),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = magma(n=1000, direction = -1))
dev.off()

# map and write out phylogenetic diversity map
summary(phylogenetic)
plot(phylogenetic)

breaks <- classIntervals(values(phylogenetic, na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("phylodiversity_v20240819.png", units = "px", width = 1500, height = 1400, res = 300)
terra::plot(phylogenetic[[1]], 
     type = "continuous",
     range = c(min(values(phylogenetic), na.rm = T),max(values(phylogenetic), na.rm = T)),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = magma(n=1000, direction = -1))
dev.off()

# writing out re-projected data to use for extractions never worked, so this is unnecessary
