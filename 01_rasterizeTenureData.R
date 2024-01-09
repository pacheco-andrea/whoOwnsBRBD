#### Biodiversity and Tenure in Brazil ####

# this script brings together preprocessed tenure data and rasterizes them so they can be mapped
# outputs are: 
# one categorical raster for each tenure category
# a map of tenure categories (at this stage it should indicate overlaps!)

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")



# 1. make mask to base rest of rasters on
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009")) # use my biodiversity data
r <- rast(list.files()[grep("Richness", list.files())[1]])
r <- project(r, my_crs_SAaea)
mask <- r*0

# 2. upon first run:
# rasterize IRU-AST-PCT ----
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
# need to create a new id column 
length(unique(s2$X_uid_)) == nrow(s2)
s2$id <- 1:nrow(s2)
setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_allBR"))
st_write(s2, "landTenureCategs_v2023_allBR.shp", row.names = F, append = FALSE) # consider re-splitting this into states?

# rasterize and write out raster for map
r <- terra::rasterize(s2, mask, "tipo")
cols <- c("#FFD700","#8DA0CB", "#FC8D62")
plot(r, col=cols) 
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
writeRaster(r, filename = "landTenure_AST-IRU-PCT_SAalbers_1km.tif", overwrite = TRUE)

# # figure out the deal with PCT lands 
# setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_PCT/"))
# l <- list.files()
# grep(".shp", l)
# pct <- lapply(l[grep(".shp", l)], st_read)
# pct <- do.call(rbind, pct)
# nrow(pct)
# setwd(paste0(wdmain,"/data/processed/pct_lands"))
# st_write(pct, "pct_BR.shp")
# # so, importing this shapefile into qgis i can tell there's a lot, but not 100% overlap of PCTs and sustainable use areas. many are within SUs

# rasterize Conservation units (PAs) ----
setwd(paste0(wdmain, "data/processed/landTenure_UC/"))
uc <- st_read("landTenure_UCs_MMA_20231212_SAalbers.shp", crs = "+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
plot(uc$geometry)
uc_r <- rasterize(uc, mask, "group")
plot(uc_r)
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
writeRaster(uc_r, filename = "landTenure_UC_SAalbers_1km.tif", overwrite = TRUE)

# rasterize indigenous lands----
setwd(paste0(wdmain, "data/processed/landTenure_IND/"))
ind <- st_read("landTenure_indigenous_20231212_SAalbers.shp")
ind <- st_transform(ind, my_crs_SAaea)
ind$LTcateg <- "indigenous"
ind_r <- rasterize(ind, mask, "LTcateg") # one version for the category
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
writeRaster(ind_r, filename = "landTenure_IND_SAalbers_1km.tif", overwrite = TRUE)

# public forests ----
setwd(paste0(wdmain, "data/processed/landTenure_UND-OTH/"))
flp <- st_read("landTenure_UND-OTH_SAalbers.shp")
flp_r <- rasterize(flp, mask, "protecao")
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
writeRaster(flp_r, filename = "landTenure_UND-OTH_SAalbers_1km.tif")



# 3. make map of tenure categories ----
tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")

# get tenure rasters:
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- grep(".tif$", list.files())
t <- rast(list.files()[l]) 
terra::plot(t)

names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", list.files()[l]))

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(t, proj = T))
plot(biomes)
v <- vect(biomes)


# plot maps one at a time, one on top of the other
names(t)

par(mfrow = c(1,1))
plot(t$UC, # conservation units
     col = c("#8C7E5B", "#1B9E77"), 
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("PA strict protection","PA sustainable use"), x="left", y=-5))

# just to check which PA categories were actually overlapping and it does seem like it includes parks (only concerning categ)
# see more here: https://terrasindigenas.org.br/#pesquisa
# plot(t$landTenure_PAs_correctCategs, 
#      # col = c("#8C7E5B", "#1B9E77"),
#      type = "classes",
#      mar=NA,
#      box = F,
#      axes = F,
#      plg = list(x="bottomleft"))
# terra::lines(v, lwd=.1)

plot(t$IND,
     add = T,
     col = c("#E78AC3"), alpha = .8,
     type = "classes",
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("indigenous"), x = "bottomright"))

     # plg = list(legend = c("declared","delimited", "under study", "encaminhada","homologated", "regularized"), x = "bottomright"))


plot(t$`landTenure_AST-IRU-PCT`, 
     # add = T,
     col = c("#FC8D62", "#8DA0CB", "#FFD700"), alpha = .8,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("rural settlement", "rural property", "Other TPLCs"), x = "bottomright"))

plot(t$`landTenure_undesignated-military-other`, 
     add = T,
     col = c("black", "#1d6c7d", "gray80"), alpha = .8,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("other uses", "undesignated public lands", "military"), x = "topright"))

terra::lines(v, lwd=.1)


