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
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# Biodiversity indicators ----
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
richness <- rast(rasters[grep("Rich", rasters)])
endemism <- rast(rasters[grep("Ende", rasters)])
names(endemism) <- gsub("_All_groups_RF.tif", "", rasters[grep("Ende", rasters)])

# reproject 
crs(richness) <- my_crs_SAaea
res(richness) # .01*.01degrees - which means at ~1km. 
# map and write out richness
setwd(paste0(wdmain, "output/maps/"))
png("richness.png", units = "px", width = 1500, height = 1500, res = 300)
plot(richness, 
     type = "continuous",
     range = c(0,5000),
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = viridis(n = 7203, direction = -1))
dev.off()


# reproject 
crs(endemism) <- my_crs_SAaea
# map and write out endemism
setwd(paste0(wdmain, "output/maps/"))
png("endemism.png", units = "px", width = 2800, height = 1500, res = 300)
par(mfrow = c(1, 2))
plot(endemism$Phylogenetic_Endemism, 
     type = "continuous",
     range = c(0,1),
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = magma(n=1000, direction = -1))
plot(endemism$Weight_Endemism, 
     type = "continuous",
     axes=F,
     range = c(0,1),
     mar = c(0, 0, 0, 7.1),
     col = magma(n = 1000, direction = -1))
dev.off()

# write out data I will actually use for extractions
setwd(paste0(wdmain, "data/processed/BiodivIndicators_albers"))
writeRaster(richness, "BDBR_richness_current.tif", overwrite = T)
writeRaster(endemism$Phylogenetic_Endemism, "BDBR_phyEndemism_current.tif", overwrite = T)
writeRaster(endemism$Weight_Endemism, "BDBR_Endemism_current.tif", overwrite = T)
