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

# rasterize preprocessed tenure data ----

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


# MISSING here: NEXT STEPS
# IRU already done above
# SNCI
# SIGEF?



# 3. make map of tenure categories ----
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

# identify the overlaps more systematically ----

# PUBLIC OVERLAPS
# make the rasters overlap through the different possible combinations:
names(t)
overlaps <- list()
for(i in 1:(length(t)-1))
{
  overlaps[[i]] <- terra::intersect(t[[i]], t[[i+1]]) # 1+2, 2+3, 3+4, 4+5 BUT ALSO NEED
}
length2 <- length(overlaps)
for(i in length2:6)
{
  overlaps[[i+1]] <- terra::intersect(t[[1]], t[[i-1]]) # 1+3 and 1+4 and 1+5 Now missing 
}

overlaps[[8]] <- terra::intersect(t[[2]], t[[4]]) # 2+4 2+5
overlaps[[9]] <- terra::intersect(t[[2]], t[[5]])
names(t)

names(overlaps) <- c("indigenous-IRU", "IRU-PAs", "PAS-ruralSettlements", "ruralSettlements-undesignated",
                     "indigenous-PAs", "indigenous-ruralSettlements", "indigenous-undesignated",
                    "IRU-ruralSettlements", "IRU-undesignated")
myOverlaps <- mosaic(sprc(overlaps), fun = sum)
plot(myOverlaps) # this would then be all the overlaps of public lands, 

# but i can always identify which ones specifically overlap from the layers i created above
# visualize these and identify which is what
par(mfrow = c(2,5))
plot(myOverlaps) 
for(i in 1:length(overlaps))
{
  plot(overlaps[[i]], main = paste0(names(overlaps)[i]), legend = F)
}

dev.off()

# alright, this visualization is indeed very very helpful
# clearly the big overlaps are with IRU-and PAs
# and IRU and undesignated lands 
# some, fewer overlaps with indigenous
# and indigenous PAs


# should quantify these overlaps in amounts of km though
count <- data.frame("tenure" = c(names(overlaps)),
                    "non-overlaps" = 0, 
                    "overlaps" = 0)

for(i in 1:length(overlaps))
{
  table <- table(values(overlaps[[i]]))
  count[i,2] <- as.numeric(table[1])
  count[i,3] <- as.numeric(table[2])
  
}
count
# count$proportion <- count$overlaps/count$non.overlaps # but this doesn't count the total number of area under that tenure
# i remember i used to do this by counting the frequencies of the rasters
# but probably need to do this not from the overlap raster
class(count$non.overlaps)
count <- arrange(count, desc(overlaps))
count$tenure <- factor(count$tenure, levels = count$tenure)

ggplot(count, aes(tenure, overlaps, fill = tenure)) +
  geom_bar(stat = "identity") +
  labs(title = " Overlaps in categories (in km2)") +
  theme(panel.background = element_rect(fill = "transparent"), legend.position = "inside")

