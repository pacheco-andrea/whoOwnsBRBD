#### Biodiversity and Tenure in Brazil ####

# this script pre-processes biodiversity indicator data produced by Ubirajara Oliveira at CSR
# outputs are:
# 1. maps of biodiversity
#


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)
library(RColorBrewer)
library(viridis)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# Biodiversity indicators ----
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
rasters <- list.files()[grep("tif$", list.files())]
richness <- rast(rasters[grep("Rich", rasters)])
endemism <- rast(rasters[grep("Ende", rasters)])
names(endemism) <- gsub("_All_groups_RF.tif", "", rasters[grep("Ende", rasters)])

# reproject and map richness
crs(richness) <- my_crs_SAaea
plot(richness, 
     type = "continuous",
     range = c(0,5000),
     col = viridis(n = 7203, direction = -1))
res(richness) # .01*.01degrees - which means at ~1km. 
# write out
setwd(paste0(wdmain, "output/maps/"))



# reproject and map endemism
crs(endemism) <- my_crs_SAaea
par(mfrow = c(1, 2))
plot(endemism$Phylogenetic_Endemism, 
     type = "continuous",
     range = c(0,1),
     col = magma(n=1000, direction = -1))
plot(endemism$Weight_Endemism, 
     type = "continuous",
     range = c(0,1),
     col = magma(n = 1000, direction = -1))
dev.off()
# transform to south america albers equal area for making calculations 
my_crs <- terra::crs(stateShp, proj = TRUE)
# richness_eq <- project(richness, my_crs)
# plot(richness_eq)
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
rastFiles <- grep(".tif$", list.files())
list.files()[rastFiles]
for(i in 1:length(rastFiles))
{
  setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
  r <- rast(paste0(list.files()[rastFiles][i]))
  r2 <- project(r, my_crs)
  name <- gsub(".tif", "_SAalbers.tif", paste0(list.files()[rastFiles][i]))
  print(i)
  setwd(paste0(wdmain,"/data/processed/BiodivIndicators_SA-albers"))
  writeRaster(r2, filename = name)
}
