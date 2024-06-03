#### Biodiversity and Tenure in Brazil ####

# private lands # 


# libraries
library(terra)
library(sf)
library(geos)
library(dplyr)
library(ggplot2)
library(tidyverse)


# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))


# A) UPON FIRST RUN, Read and standardize data: ----
# # private PAs
# rppn <- st_read("landTenure_RPPN/landTenure_RPPN_20231212_SAalbers.shp")
# rppn <- st_transform(rppn, my_crs_SAaea) # fix projection
# rppn$id <- paste0("RPPN-", 1:nrow(rppn))
# rppn <- select(rppn, c("LTcateg", "id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/private"))
# st_write(rppn, "private_protectedAreas.shp", append = F)
# # rppn.selfOVerlaps <- st_intersection(rppn)
# 
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
st_write(priPAs_no.overlaps, "private_protectedAreas.shp", append = F)

# get the self-overlaps 
unique(st_geometry_type(priPAs.overlaps[which(priPAs.overlaps$n.overlaps > 1),]))
pgeometries <- priPAs.overlaps[which(priPAs.overlaps$n.overlaps > 1),] 
# note, out of 1694 features, only 5 were actual polygons - the rest were linestrings, etc.
# when plotted, these are practically insignificant
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
st_write(qui_no.overlaps, "quilombolaLands.shp", append = F)

# get the self-overlaps 
unique(st_geometry_type(qui.overlaps[which(qui.overlaps$n.overlaps > 1),]))
pgeometries <- qui.overlaps[which(qui.overlaps$n.overlaps > 1),] 
# note, out of 1694 features, only 5 were actual polygons - the rest were linestrings, etc.
# when plotted, these are practically insignificant
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
qui.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
plot(qui.selfOverlaps$geometry)

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(qui.selfOverlaps, "quilombola_selfOverlaps.shp", append = F)

# rural Properties ----
# do not need cleaning for self-overlaps - so they should just be placed in the right folder... 

# SIGEF properties ----
# process for the sake of comparing - but don't ultimately use in analysis
setwd(paste0(wdmain,"/data/processed/processed2/private"))
sigef <- st_read("SIGEF_properties.shp")
sigef
# summary(st_is_valid(sigef)) # they're all valid
# test <- st_intersection(sigef[1:10000,])
sigef <- st_union(sigef) # to simplify?
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
st_write(sigef, "SIGEF_properties.shp", append = F)

# SNCI properies ----
# maybe do this later as it's not a priority?



# C) Get polygons that DO NOT overlap with each other
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
l <- list.files()
l[grep(".shp", l)]

# private PAs vs quilombola lands 
priPAs <- st_read("private_protectedAreas.shp")
qui <- st_read("quilombolaLands.shp")
# normally, I would do the following: priPAs_x_qui <- rbind(priPAs, qui)
# but i first check how much overlap there is to begin with by doing:
priPAs_x_qui_overlaps <- st_intersection(priPAs, qui)
plot(priPAs_x_qui_overlaps[,"LTcateg"]) # doesn't seem to be much overlap in any case (15km2) 
sum(st_area(priPAs_x_qui_overlaps)) # essentially 15km2
# this means it doesn't make much sense to make the actual intersection
# meaning, in turn, that private PAs and quilombos don't overlap, and they belong in the no-overlaps_private folder

# quilombola lands vs rural properties
setwd(paste0(wdmain,"/data/processed/processed2/private"))
iru <- st_read("ruralProperties.shp")
iru <- st_transform(iru, my_crs_SAaea)
qui <- st_transform(qui, my_crs_SAaea)

# intersecting them as one object is my only way of getting the parts that don't intersect
# i always doubt this again, and make myself think that i could get this using st_difference. but no. i need to intersect.
test_diff <- st_difference(qui, iru)
test_intersection1 <- st_intersection(rbind(qui, iru))
test_intersection <- st_intersection(qui, iru)
plot(test_intersection$geometry)

table <- data.frame("categ" = NA, "area_Int"= NA, "area_Categ" = NA,"area_B" = NA)
table[1,2] <- sum(st_area(test_intersection)) # 8,000 km2
table[1,3] <- sum(st_area(iru))
table[1,4] <- sum(st_area(qui))
table[1,2]/table[1,4] # 28% of quilombos are intersecting with rural properties

# D) Find overlapping polygons across categories ----