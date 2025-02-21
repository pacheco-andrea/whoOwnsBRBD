#### Biodiversity and Tenure in Brazil ####

# script to directly relate the biodiversity data with the Forest Code compliance (as a percentage) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(cowplot)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")



# get new % compliance data ----
setwd(paste0(wdmain, "data/raw/nivel_conformidade_imoveis_panorama2024"))
compliance <- rast("nivel_conformidade_imoveis_panorama2024.tif")
plot(compliance)
# transform
compliance2 <- project(compliance, my_crs_SAaea)
my_colors <- colorRampPalette(c('#c51b7d','#de77ae','#f1b6da','#fde0ef','#e6f5d0','#b8e186','#7fbc41','#4d9221'))(100)
plot(compliance2, col = my_colors)
# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = my_crs_SAaea)

plot(biomes)
plot(compliance2, col = my_colors, add =T)
plot(biomes, add =T)

# create surplus raster
surplus <- ifel(compliance2 >= 0, compliance2, NA)
surplus_colors <- colorRampPalette(c('#e6f5d0','#b8e186','#7fbc41','#4d9221'))(50)
plot(biomes)
plot(surplus, col = surplus_colors, add =T)
plot(biomes, add =T)

# create deficit raster
deficit <- ifel(compliance2 <= 0, compliance2, NA)
deficit_colors <- colorRampPalette(c('#c51b7d','#de77ae','#f1b6da','#fde0ef'))(50)
plot(biomes)
plot(deficit, col = deficit_colors, add =T)
plot(biomes, add =T)


# get biodiversity data ----
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20241010"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
richness <- rast(rasters[grep("Richness", rasters)])
richness <- project(richness, my_crs_SAaea)
endemism <- rast(rasters[grep("Endemism", rasters)])
endemism <- project(endemism, my_crs_SAaea)


# make maps with biodiversity ----
# the deficit & biodiv data have diff extents & res, so i need to resample it 
richness_resampled <- resample(richness, compliance2, method = "bilinear")
endemism_resampled <- resample(endemism, compliance2, method = "bilinear")


setwd(paste0(wdmain, "/output/maps"))
png("MapSurplusAreas_rich_x_pCompliance.png", width = 3000, height = 3000, units = "px", res = 500)
surplusBD <- richness_resampled[[1]]*(surplus/100)
surplus_colors <- colorRampPalette(c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494'))(25)
plot(biomes, col = "gray90", border = NA)
plot(surplusBD, col = surplus_colors, add =T)
plot(biomes, add =T, lwd = 0.5)
dev.off()

setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_rich_x_pCompliance.png", width = 3000, height = 3000, units = "px", res = 500)
deficitBD <- richness_resampled[[2]]*abs((deficit/100)) # this richness layer is the baseline
deficit_colors <- colorRampPalette(c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177'))(50)
plot(biomes, col = "gray90", border = NA)
plot(deficitBD, col = deficit_colors, add =T)
plot(biomes, add =T, lwd = 0.5)
dev.off()

# endemism: check if any difference
setwd(paste0(wdmain, "/output/maps"))
png("MapSurplusAreas_ende_x_pCompliance.png", width = 3000, height = 3000, units = "px", res = 500)
surplusBD <- endemism_resampled[[1]]*(surplus/100)
surplus_colors <- colorRampPalette(c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494'))(25)
plot(biomes, col = "gray90", border = NA)
plot(surplusBD, col = surplus_colors, add =T)
plot(biomes, add =T, lwd = 0.5)
dev.off()
setwd(paste0(wdmain, "/output/maps"))

png("MapDeficitAreas_ende_x_pCompliance.png", width = 3000, height = 3000, units = "px", res = 500)
deficitBD <- endemism_resampled[[2]]*abs((deficit/100)) # this richness layer is the baseline
deficit_colors <- colorRampPalette(c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177'))(50)
plot(biomes, col = "gray90", border = NA)
plot(deficitBD, col = deficit_colors, add =T)
plot(biomes, add =T, lwd = 0.5)
dev.off()


