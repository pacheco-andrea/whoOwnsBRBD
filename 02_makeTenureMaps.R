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
l <- grep(".tif$", list.files())
test <- rast(l)
t <- rast("landTenure_AST-IRU-PCT_SAalbers_1km.tif")
plot(t)

# make tenure map
tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(t, proj = T))
plot(biomes)
v <- vect(biomes)
# add indigenous
ind <- rast("landTenure_indigenous_SAalbers_1km.tif")

# add PAs
pas <- rast("landTenure_PAs_SAalbers_1km.tif")

terra::plot(t, col = c("#FC8D62", "#8DA0CB", "#1d6c7d"), plg = list(legend = c("AST","IRU", "PCT"), x = "left"))
plot(pas, add = T, col = c("#8C7E5B", "#1B9E77"), plg = list(legend = c(0,0), x = "left"))
plot(ind, add = T, col = "#E78AC3", plg = list(legend = c(0,0,0,0), x = "bottomleft"))
terra::lines(v, lwd=.1)



# setwd(paste0(wdmain, "/output/"))
# png(file = "tenure_map.png", width = 1500, height = 1500, units = "px", res = 300)
# tenure_plot
# dev.off()


