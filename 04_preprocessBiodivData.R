#### Biodiversity and Tenure in Brazil ####

# this script pre-processes biodiversity indicator data produced by Ubirajara Oliveira at CSR
# outputs are:
# 1. maps of biodiversity in south america equal albers
# 2. re-projected version of the data i will actually use: richness, endemism, and the losses of these


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
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20241010"))
rasters <- list.files()[grep("tif$", list.files())]
rasters
richness <- rast(rasters[grep("Richness", rasters)])
endemism <- rast(rasters[grep("Endemism", rasters)])
phylogenetic <- rast(rasters[grep("Phylodiversity", rasters)])

par(mfrow = c(1,3))
plot(richness)

b_at <- richness[[1]]-richness[[2]]
plot(b_at)
par(mfrow = c(1,2))
plot(b_at)
plot(richness[[3]])
dev.off()

res(richness) # .01 degrees 

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(richness[[1]], proj = T))

biomes2 <- st_simplify(biomes, dTolerance = 10000)
plot(biomes2)
v <- vect(biomes2)



# current biodiversity ----
# map and write out richness map

breaks <- classIntervals(values(richness[[1]], na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("richness_v20241010.png", units = "px", width = 1500, height = 1500, res = 300)
terra::lines(v, lwd=.1)
terra::plot(richness[[1]], 
     type = "continuous",
     range = c(min(breaks$brks),max(breaks$brks)),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     plg = list(cex = 1.2),
     col = turbo(n = as.integer(max(breaks$brks)-min(breaks$brks))))
terra::lines(v, lwd=.1)
dev.off()


# map and write out endemism map

breaks <- classIntervals(values(endemism[[1]], na.rm = T))
breaks$brks


setwd(paste0(wdmain, "output/maps/"))
png("endemism_v20241010.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(endemism[[1]], 
     type = "continuous",
     range = c(min(breaks$brks), max(breaks$brks)),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     plg = list(cex = 1.2),
     col = turbo(n = as.integer(max(breaks$brks)-min(breaks$brks))))
terra::lines(v, lwd=.1)
dev.off()

# map and write out phylogenetic diversity map

breaks <- classIntervals(values(phylogenetic[[1]], na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("phylodiversity_v20241010.png", units = "px", width = 1500, height = 1400, res = 300)
terra::plot(phylogenetic[[1]], 
     type = "continuous",
     range = c(min(breaks$brks),max(breaks$brks)),
     breaks = breaks$brks,
     axes=F,
     mar = c(0, 0, 0, 7.1),
     col = turbo(n = as.integer(max(breaks$brks)-min(breaks$brks))))
terra::lines(v, lwd=.1)
dev.off()

# map and write out losses to compare to the deforestation map ----

# richness
breaks <- classIntervals(values(richness[[3]], na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("richnessDecline_v20241010.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(richness[[3]], 
            type = "continuous",
            range = c(min(breaks$brks),max(breaks$brks)),
            breaks = breaks$brks,
            axes=F,
            mar = c(0, 0, 0, 7.1),
            col = viridis(n = as.integer(max(breaks$brks)-min(breaks$brks))))
terra::lines(v, lwd=.001, col = "gray80")
dev.off()

# endemism
breaks <- classIntervals(values(endemism[[3]], na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("endemismDecline_v20241010.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(endemism[[3]], 
            type = "continuous",
            range = c(min(breaks$brks),max(breaks$brks)),
            breaks = breaks$brks,
            axes=F,
            mar = c(0, 0, 0, 7.1),
            col = viridis(n = as.integer(max(breaks$brks)-min(breaks$brks))))
terra::lines(v, lwd=.001, col = "gray80")
dev.off()

# phylodiversity
breaks <- classIntervals(values(phylogenetic[[3]], na.rm = T))
breaks$brks

setwd(paste0(wdmain, "output/maps/"))
png("phylodiversityDecline_v20241010.png", units = "px", width = 1500, height = 1500, res = 300)
terra::plot(phylogenetic[[3]], 
            type = "continuous",
            range = c(min(breaks$brks),max(breaks$brks)),
            breaks = breaks$brks,
            axes=F,
            mar = c(0, 0, 0, 7.1),
            col = viridis(n = as.integer(max(breaks$brks)-min(breaks$brks))))
terra::lines(v, lwd=.001, col = "gray80")
dev.off()

