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
t <- rast(list.files()[l])
terra::plot(t)

# make tenure map
tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(t, proj = T))
plot(biomes)
v <- vect(biomes)
# add indigenous

plot(t$tipo, 
            col = c("#FC8D62", "#8DA0CB", "#FFD700"), 
            type = "classes", 
            mar=NA,
            box = F,
            axes = F,
            plg = list(legend = c("AST","IRU", "PCT"), x="left", y=-5))
plot(t$group, 
     # add = T, 
     col = c("#1B9E77","#8C7E5B"), alpha = 0.5,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("PI","US"), x = "bottomleft"))

plot(t$modalidade, 
     # add = T, 
     col = "#E78AC3", alpha = .9,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("Indigenous"), x = "bottomright"))
terra::lines(v, lwd=.1)

setwd(paste0(wdmain, "/output/"))
png(file = "tenure_map_20231016.png", width = 2000, height = 2000, units = "px", res = 300)
plot(t$tipo, 
     col = c("#FC8D62", "#8DA0CB", "#FFD700"), 
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("AST","IRU", "PCT"), x="left", y=-5))
plot(t$group, 
     add = T, 
     col = c("#1B9E77","#8C7E5B"), alpha = 0.5,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("PI","US"), x = "bottomleft"))

plot(t$modalidade, 
     add = T, 
     col = "#E78AC3", alpha = .9,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("Indigenous"), x = "bottomright"))
terra::lines(v, lwd=.1)
dev.off()


