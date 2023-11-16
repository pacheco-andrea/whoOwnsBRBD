#### Biodiversity and Tenure in Brazil ####

# this script brings together preprocessed tenure data and rasterizes them so they can be mapped
# outputs are: 
# one categorical raster for each tenure category
# a map of tenure categories (at this stage it should indicate overlaps!)

# libraries
library(terra)
library(ggplot2)
#library(tidyterra)
library(sf)
#library(geodata)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")



# 1. make mask to base rest of rasters on
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009")) # use my biodiversity data
r <- rast(list.files()[grep("Richness", list.files())[1]])
r <- project(r, my_crs_SAaea)
mask <- r*0

# rasterize IRU-AST-PCT
setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
# need to bind all the state sfs into one in order to:
# 1. rasterize without raster changing categories across states
# 2. generate a unique ID for each polygon...?
s <- list()
for (i in 1:length(grep(".shp",list.files()))) # approx runtime on the server = 20min
{
  setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
  shps <- grep(".shp", list.files())  
  stateShp <- read_my_shp(list.files()[shps[i]])
  name <- list.files()[shps[i]]
  s[[i]] <- stateShp
  # # also writing out shapefiles only of PCT because I need to more easily compare these against sustainable use PAs
  # setwd(paste0(wdmain,"/data/processed/PCT_landTenureCategs_v2023/"))
  # st_write(stateShp[which(stateShp$tipo == "PCT"),], name)
}
# merge individual states into one whole map of brazil
s2 <- do.call(rbind, s)
# need to create a new id column because
length(unique(s2$X_uid_)) == nrow(s2)
s2$id <- 1:nrow(s2)
setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_allBR"))
st_write(s2, "landTenureCategs_v2023_allBR.shp", row.names = F, append = FALSE) # consider re-splitting this into states?

# rasterize and write out raster for map
r <- terra::rasterize(s2, mask, "tipo")
cols <- c("#FFD700","#8DA0CB", "#FC8D62")
plot(r, col=cols) 
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
writeRaster(r, filename = "landTenure_AST-IRU-PCT_SAalbers_1km.tif")

# figure out the deal with PCT lands 
setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_PCT/"))
l <- list.files()
grep(".shp", l)
pct <- lapply(l[grep(".shp", l)], st_read)
pct <- do.call(rbind, pct)
nrow(pct)
setwd(paste0(wdmain,"/data/processed/pct_lands"))
st_write(pct, "pct_BR.shp")
# so, importing this shapefile into qgis i can tell there's a lot, but not 100% overlap of PCTs and sustainable use areas. many are within SUs

# rasterize ucs and inds 
uc_r <- rasterize(uc2, mask, "group")
plot(uc_r)
setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(uc_r, filename = "landTenure_PAs_SAalbers_1km.tif")

ind_r <- rasterize(ind2, mask, "modalidade") # CHECK what exactly i want to rasterize
plot(ind_r)

# public forests 
flp_r <- rasterize(flp2, mask, "categoria")
plot(flp_r)
setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(und, filename = "landTenure_Undes_SAalbers_1km.tif")