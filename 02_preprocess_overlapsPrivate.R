#### Biodiversity and Tenure in Brazil ####

# private lands # 
# same processing of self-overlaps and no-overlaps as done for public lands

# libraries
library(terra)
library(sf)
library(geos)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(fasterize)
library(exactextractr)

# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))


# A) UPON FIRST RUN, Read and standardize data: ----
# # private PAs
rppn <- st_read("landTenure_RPPN/landTenure_RPPN_20231212_SAalbers.shp")
rppn <- st_transform(rppn, my_crs_SAaea) # fix projection
rppn$id <- paste0("RPPN-", 1:nrow(rppn))
rppn <- select(rppn, c("LTcateg", "id", "geometry"))
setwd(paste0(wdmain,"/data/processed/processed2/private"))
st_write(rppn, "private_protectedAreas.shp", append = F)
rppn$area <- as.numeric(st_area(rppn)/1000000)
summary(rppn)
sum(rppn$area)
rppn2 <- rppn[which(rppn$area >= 1),]
summary(rppn2)
# rppn.selfOVerlaps <- st_intersection(rppn)

# # sigef properties (around 1M)
# setwd(paste0(wdmain,"/data/processed/"))
# sigef <- st_read("landTenure_SIGEF/landTenure_SIGEF_20231312_SAalbers.shp")
# sigef <- st_transform(sigef, my_crs_SAaea)
# sigef$id <- paste0("SIGEF-", 1:nrow(sigef))
# sigef <- select(sigef, c("LTcateg", "id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/private"))
# st_write(sigef, "SIGEF_properties.shp", append = F)
# 
# # snci properties
# setwd(paste0(wdmain,"/data/processed/"))
# snci <- st_read("landTenure_SNCI/")
# snci <- st_transform(snci, my_crs_SAaea)
# snci$id <- paste0("SNCI-", 1:nrow(snci))
# snci <- select(snci, c("LTcateg", "id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/private"))
# st_write(snci, "SNCI_properties.shp", append = F)
# 
# 
# # quilombola lands
# setwd(paste0(wdmain,"/data/processed/"))
# quilombola <- st_read("landTenure_QUI/")
# quilombola <- st_transform(quilombola, my_crs_SAaea)
# quilombola$id <- paste0("QUI-", 1:nrow(quilombola))
# quilombola <- select(quilombola, c("LTcateg", "id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/private"))
# st_write(quilombola, "quilombolaLands.shp", append = F)

# # IRU - rural properties
# setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT"))
# l <- list.files()
# iru <- lapply(l[grep(".shp", l)], st_read)
# iru <- do.call(rbind, iru)
# iru
# unique(iru$LTcateg)
# iru <- iru[which(iru$LTcateg == "IRU"),]
# iru$id <- paste0("IRU-", 1:nrow(iru))
# iru <- select(iru, c("LTcateg","id", "geometry"))
# emptyGeos <- which(st_is_empty(iru))
# iru <- iru[-emptyGeos,]
# setwd(paste0(wdmain,"/data/processed/processed2/private"))
# st_write(iru, "ruralProperties.shp", append = F)




# B) Cleaning of self-overlaps from private lands data ----
setwd(paste0(wdmain,"/data/processed/processed2/private"))
l <- list.files()
l[grep(".shp", l)]


# RPPNs ----
priPAs <- st_read("private_protectedAreas.shp")
st_is_valid(priPAs)
priPAs <- st_make_valid(priPAs)
priPAs[which(st_is_valid(priPAs)),]
priPAs.overlaps <- st_intersection(priPAs)
# plot(priPAs.overlaps[,"n.overlaps"])

# df with no overlaps
priPAs_no.overlaps <- priPAs.overlaps[which(priPAs.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]
unique(st_geometry_type(priPAs_no.overlaps))
# write out this data
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
st_write(priPAs_no.overlaps, "privatePAs.shp", append = F)

# get the self-overlaps 
unique(st_geometry_type(priPAs.overlaps[which(priPAs.overlaps$n.overlaps > 1),]))
pgeometries <- priPAs.overlaps[which(priPAs.overlaps$n.overlaps > 1),] 
# note, out of 1694 features, only 5 were actual polygons - the rest were linestrings, etc.
# when plotted, these are practically insignificant, but == 125km2 
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
priPAs.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
plot(priPAs.selfOverlaps$geometry)

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(priPAs.selfOverlaps, "privatePAs_selfOverlaps.shp", append = F)

# Quilombola lands ----
setwd(paste0(wdmain,"/data/processed/processed2/private"))

qui <- st_read("quilombolaLands.shp")
qui.overlaps <- st_intersection(qui)
# plot(qui.overlaps[,"n.overlaps"])

# df with no overlaps
qui_no.overlaps <- qui.overlaps[which(qui.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]
unique(st_geometry_type(qui_no.overlaps))
extract.pgeometries <- st_collection_extract(qui_no.overlaps, "POLYGON")
qui_no.overlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
# write out this data
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
st_write(qui_no.overlaps, "quilombola.shp", append = F)

# get the self-overlaps 
unique(st_geometry_type(qui.overlaps[which(qui.overlaps$n.overlaps > 1),]))
pgeometries <- qui.overlaps[which(qui.overlaps$n.overlaps > 1),] 
# these summed to 62
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
qui.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
plot(qui.selfOverlaps$geometry)

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(qui.selfOverlaps, "quilombola_selfOverlaps.shp", append = F)

# rural Properties ----
# do not need cleaning for self-overlaps - so they should just be placed in the right folder... 


# (don't further process SIGEF or SNCI because unneeded) ----

# private PAs vs quilombola lands 
priPAs <- st_read("privatePAs.shp")
qui <- st_read("quilombola.shp")
# normally, I would do the following: priPAs_x_qui <- rbind(priPAs, qui)
# but i first check how much overlap there is to begin with by doing:
priPAs_x_qui_overlaps <- st_intersection(priPAs, qui)
plot(priPAs_x_qui_overlaps[,"LTcateg"]) # doesn't seem to be much overlap in any case (15km2) 
sum(st_area(priPAs_x_qui_overlaps)) # 15km2
# this means it doesn't make much sense to write out the overlapping polygons



# D) Find overlapping polygons across 3 categories ----
# private PAs, quilombolas, and rural properties

# Upon first run: 
# get rural properties data
setwd(paste0(wdmain,"/data/processed/processed2/private"))
iru <- st_read("ruralProperties.shp")
iru <- st_transform(iru, my_crs_SAaea)
qui <- st_read("quilombolaLands.shp")
qui <- st_transform(qui, my_crs_SAaea)
# remember, intersecting them as one object is my only way of getting the parts that don't intersect with each other
# st_difference does not yield the result i need
# just to check how much *area* actually intersects I can run the intersection with the two as separate objects
# test_intersection <- st_intersection(qui, iru) 
# sum(st_area(test_intersection))/1000000 # this is 8,000 km2, which is significant
# plot(test_intersection$geometry)

# table <- data.frame("categ" = NA, "area_Int"= NA, "area_Categ" = NA,"area_B" = NA)
# table[1,2] <- sum(st_area(test_intersection)) # 8,000 km2
# table[1,3] <- sum(st_area(iru))
# table[1,4] <- sum(st_area(qui))
# table[1,2]/table[1,4] # 28% of quilombos are intersecting with rural properties

# try to run the intersection as one object:
# test_intersection <- st_intersection(rbind(qui, iru))
# test_intersection
# IN SUM:
# the error "Error: GEOS exception" persists in this case as well. 
# the problem is that it is completely unmanageable to debug the 6,000,000 iru properties
# so there's no manageable way to get the *polygon-level* information on which polygons do NOT intersect with each other
# i.e., a dataset with the column that provides the number of intersections, allowing me to 
# write out (semi) "clean" versions of: 1) overlapping and 2) non-overlapping IRU (against private PAs and quilombos)

# BUT i can still run the one-object intersections in order to write out the overlapping parts

# overlap IRU x quilombolas ----
quiru <- st_intersection(qui, iru)
quiru
plot(quiru$geometry)
unique(st_geometry_type(quiru))
extract.pgeometries <- st_collection_extract(quiru, "POLYGON")
quiru2 <- extract.pgeometries %>% group_by(LTcateg, id, LTcateg.1, id.1) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
# check if any empty features?
empty <- st_is_empty(quiru2)
quiru2[empty,]
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(quiru2, "quilombola-ruralProperties.shp", append = F)
rm(quiru2)

# overlap IRU x priv PAs ----
# ok, clearly there will be errors and intersections here. - bc using not clean priPAs??
# need to think about what it means (legally) private PAs in rural properties...
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
priPAs <- st_read("privatePAs.shp")
priPAs <- st_transform(priPAs, my_crs_SAaea)
pairu <- st_intersection(priPAs, iru)
pairu
plot(pairu$geometry)
unique(st_geometry_type(pairu))
extract.pgeometries <- st_collection_extract(pairu, "POLYGON")
pairu2 <- extract.pgeometries %>% group_by(LTcateg, id, LTcateg.1, id.1) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(pairu2, "privatePAs-ruralProperties.shp", append = F)


# Rasterization of rural properties (IRU) ----

# upon first run, rasterization of IRU (note, computationally heavy)
# get raster mask I already have from the getting started script
r1 <- r*0
# disaggregate so that it's a more fine resolution
res(r1)
disag_r1 <- disagg(r1, fact = 35.9293) # this is the level of disaggregation to have a 30m resolution
# rasterize 30m res version
iru_R <- rasterize(iru, disag_r1)
# write out
setwd(paste0(wdmain,"/data/processed/processed2/private"))
writeRaster(iru_R, "ruralProperties.tif")
iru_R
plot(iru_R)

