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
setwd(paste0(wdmain,"/data/raw/Biodiversidade_BR"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
richness <- rast(rasters[grep("richness_current", rasters)])
endemism <- rast(rasters[grep("Ende_current", rasters)])
phylogenetic <- rast(rasters[grep("phylodiversity_current", rasters)])

res(richness) # .004degrees 
# map and write out richness map
summary(richness)
summary(endemism)
plot(richness)

breaks <- classIntervals(values(richness, na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("richness_v20240819.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(richness, 
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
png("endemism_v20240819.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(endemism, 
     type = "continuous",
     range = c(0,1),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = magma(n=1000, direction = -1))
dev.off()

# map and write out endemism map
summary(phylogenetic)
plot(phylogenetic)

breaks <- classIntervals(values(phylogenetic, na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("phylodiversity_v20240819.png", units = "px", width = 1500, height = 1400, res = 300)
plot(phylogenetic, 
     type = "continuous",
     range = c(min(values(phylogenetic), na.rm = T),max(values(phylogenetic), na.rm = T)),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = magma(n=1000, direction = -1))
dev.off()

# writing out re-projected data to use for extractions never worked, so this is unnecessary
