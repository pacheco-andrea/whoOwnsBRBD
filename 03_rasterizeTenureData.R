#### Biodiversity and Tenure in Brazil ####

# FOR THE ONE WALL-TO-WALL with the minimum necessary of overlaps (as this will become one category)
# i could st_union/combine each category and rasterize that for the map

# this script brings together preprocessed tenure data and rasterizes them so they can be mapped
# outputs are: 
# one categorical raster for each tenure category
# a map of tenure categories (at this stage it should indicate overlaps!)

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
library(dplyr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# 1. upon first run:
# rasterize IRU-AST-PCT ----
# setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
# need to bind all the state sfs into one in order to:
# 1. rasterize without raster changing categories across states
# 2. generate a unique ID for each polygon...?
# s <- list()
# for (i in 1:length(grep(".shp",list.files()))) # approx runtime on the server = 20min
# {
#   setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
#   shps <- grep(".shp", list.files())
#   stateShp <- read_my_shp(list.files()[shps[i]])
#   name <- list.files()[shps[i]]
#   s[[i]] <- stateShp
#   # # also writing out shapefiles only of PCT because I need to more easily compare these against sustainable use PAs
#   # setwd(paste0(wdmain,"/data/processed/PCT_landTenureCategs_v2023/"))
#   # st_write(stateShp[which(stateShp$tipo == "PCT"),], name)
# }
# # merge individual states into one whole map of brazil
# s2 <- do.call(rbind, s)
# # need to create a new id column 
# length(unique(s2$X_uid_)) == nrow(s2)
# s2$id <- 1:nrow(s2)
# setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_allBR"))
# st_write(s2, "landTenureCategs_v2023_allBR.shp", row.names = F, append = FALSE) # consider re-splitting this into states?

# rasterize and write out raster for map
# setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_allBR/"))
# s2 <- st_read("landTenureCategs_v2023_allBR.shp")
# # keep only rural properties (IRU)
# s2 <- s2[which(s2$tipo == "IRU"),]
# r <- terra::rasterize(s2, mask, "tipo")
# plot(r)
# setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
# writeRaster(r, filename = "landTenure_IRU_SAalbers_1km.tif", overwrite = TRUE)

#  deal with PCT lands: if i exclude them am i losing information? yes. D: this is so complicated.but don't have to deal with that for making the raster maps
# importing this shapefile into qgis i can tell there's a lot, but not 100% overlap of PCTs and sustainable use areas. many are within SUs
# shouldn't there be different rules for the FC for sustainable use areas?
# setwd(paste0(wdmain,"/data/processed/PCT_landTenureCategs_v2023/"))
# l <- list.files()
# grep(".shp", l)
# pct <- lapply(l[grep(".shp", l)], st_read)
# pct <- do.call(rbind, pct)
# nrow(pct)
# setwd(paste0(wdmain,"/data/processed/pct_lands"))
# st_write(pct, "pct_BR.shp")

# rasterize other preprocessed tenure data ----

# setwd(paste0(wdmain, "data/processed/processed2/public"))
# l <- list.files()
# public <- lapply(l[grep(".shp", l)], st_read)
# names(public) <- gsub(".shp","", l[grep(".shp", l)])
# # i actually want to keep each of these categories as separate rasters so that i can plot them systematically later
# for(i in 1:length(public))
# {
#   public[[i]] <- st_transform(public[[i]], my_crs_SAaea)
#   r <- rasterize(public[[i]], mask, "LTcateg")
#   setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
#   writeRaster(r, filename = paste0(names(public)[i], "_SAalbers_1km.tif"), overwrite = TRUE)
# }


# MISSING here: RPPN, SNCI and SIGEF?



# 2. Make tenure map  ----
tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")

# get tenure rasters:
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- grep(".tif$", list.files())
t <- lapply(list.files()[l], rast) # very weird, changing behavior with lists from terra
# terra::plot(t)
names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", list.files()[l]))

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(mask, proj = T))
plot(biomes)
v <- vect(biomes)


# plot maps one at a time, one on top of the other
names(t)

par(mfrow = c(1,1))
plot(t$protectedAreas, # conservation units
     col = c("#8C7E5B", "#1B9E77"), 
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("PA strict protection","PA sustainable use"), x="left", y=-10))

plot(t$indigenous,
     add = T,
     col = c("#E78AC3"), alpha = .8,
     type = "classes",
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("indigenous"), x = "bottomright"))

plot(t$ruralSettlements, 
     add = T,
     col = c("#FC8D62"), alpha = .8,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("rural settlements"), x = "bottomright"))

plot(t$IRU, 
     add = T,
     col = c("#8DA0CB"), alpha = .8,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("rural properties CAR"), x = "bottomright"))

plot(t$`undesignated-oth`, 
     add = T,
     col = c("red", "#1d6c7d", "gray80"), alpha = .8,
     type = "classes", 
     mar=NA,
     box = F,
     axes = F,
     plg = list(legend = c("other uses", "undesignated public lands", "military"), x = "topright"))

terra::lines(v, lwd=.1)
dev.off()


