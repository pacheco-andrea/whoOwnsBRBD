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
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# upon first run, get tenure rasters and merge them into one:
setwd(paste0(wdmain, "/data/processed/landTenureCategsRaster/"))
t <- rast("landTenure_AST-IRU-PCT_SAalbers_1km.tif")
plot(t)
# make categorical
t <- as.factor(t)
levels(t)[[1]]$tipo <- c("AST", "IRU", "PCT")

# make tenure map
plot(t)

# bra <- gadm(country = "BRA", level = 1, path = tempdir())
cols <- c("#FFD700","#8DA0CB", "#FC8D62","#F0F0F0")

ggplot() +
geom_spatraster(mapping = aes(fill = tipo), 
                data = t,
                na.rm = F,
                scale_fill_manual(values = cols),
                show.legend = T)

ggplot(t) +
  geom_rect(data = t, 
              mapping = aes(fill = tipo),
              na.rm = F, 
              show.legend = T)

plot(t, col=cols)

setwd(paste0(wdmain, "/output/"))
writeRaster(tenure_raster, "tenure_data_BR.tif")
tenure_raster <- raster("tenure_data_BR.tif")


setwd(paste0(wdmain, "/data/brazil_biomes_shp/"))
biomshp <- readOGR("Brazil_biomes.shp")
biomshp <- spTransform(biomshp, proj4string(tenure_raster))

tenure_plot <- rasterVis::levelplot(tenure_raster,
                     xlab = NULL, ylab = NULL,
                     scales = list(draw = F), 
                     par.settings = list(axis.line = list(col = "transparent")),
                     colorkey = F,
                     col.regions = c("#F0F0F0", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d", "#8DA0CB", "#FC8D62" )) + latticeExtra::layer(sp.polygons(biomshp, col = "gray20", lwd = 1))
tenure_plot 

setwd(paste0(wdmain, "/output/"))
png(file = "tenure_map.png", width = 1500, height = 1500, units = "px", res = 300)
tenure_plot
dev.off()


