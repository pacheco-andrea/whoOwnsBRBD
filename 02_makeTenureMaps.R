#### Biodiversity and Tenure in Brazil ####
# script that makes maps of land tenure categs in Brazil and biodiv indicators
# author: Andrea Pacheco
# first run: 18.10.2022
# last run: 12.10.2023

# libraries
library(terra)
library(ggplot2)
library(tidyterra)
library(sf)
library(geodata)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# upon first run, get tenure rasters and merge them into one:
setwd(paste0(wdmain, "/data/processed/landTenureCategsRaster/"))
t <- rast("landTenure_AST-IRU-PCT_SAalbers_1km.tif")
plot(t)
# make categorical
t <- as.factor(t)
levels(t)[[1]]$tipo <- c("AST", "IRU", "PCT")

# make tenure map
tenureColors = c("#F0F0F0", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d", "#8DA0CB", "#FC8D62" ))
plot(t)

# bra <- gadm(country = "BRA", level = 1, path = tempdir())
cols <- c("#FFD700","#8DA0CB", "#FC8D62")

terra::plot(t, col=cols)

# add biomes 
biomes <- read_biomes(year=2019)
# br <- br[-7,]$geom
biomes <- st_transform(biomes, crs = crs(t, proj = T))
plot(biomes)
v <- vect(biomes)
terra::plot(t, col=cols)
terra::lines(v, lwd=1)



# setwd(paste0(wdmain, "/output/"))
# png(file = "tenure_map.png", width = 1500, height = 1500, units = "px", res = 300)
# tenure_plot
# dev.off()


