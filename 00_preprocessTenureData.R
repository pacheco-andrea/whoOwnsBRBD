#### Biodiversity and Tenure in Brazil ####

# this script brings together the pre-processed tenure data and
# 1) cleans and fixes self-overlaps of data
# 2) identifies the overlappiing areas across datasets: NOTE, should do this one at a time in order to 
# 3) creates the categories for these overlaps themselves

# to create the outputs:

# 1) one version with no overlaps for rasterizing (or rather, with overlaps as one category)
# 2) one version with overlaps for extracting 


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

# A) Read and standardize PAs/UCs and indigenous lands: ----

uc <- st_read("landTenure_UC/landTenure_UCs_MMA_20231212_SAalbers.shp", stringsAsFactors = F, options = "ENCODING=latin1")
# fix empty geometries
uc <- st_transform(uc, my_crs_SAaea) # fix projection
uc$subcateg <- uc$LTcateg # add/clear up this column
uc$LTcateg <- uc$group
uc$id <- paste0("UC-", 1:nrow(uc))
uc <- select(uc, c("LTcateg", "id", "geometry"))


ind <- st_read("landTenure_IND/landTenure_indigenous_20231212_SAalbers.shp")
ind <- st_transform(ind, my_crs_SAaea)
ind$LTcateg <- "indigenous"
ind$id <- paste0("IN-", 1:nrow(ind))
ind <- select(ind, c("LTcateg","id", "geometry"))


# B) Cleaning ----
# B.1) Clean self-overlaps within UCs datasets ----

# first fix two features which prevented running st_intersection
uc[c(1047,1055),]
uc.p <- uc[which(uc$id == "UC-1047" | uc$id == "UC-1055"),]
uc <- uc[which(uc$id != "UC-1047" & uc$id != "UC-1055"),,] # remove problem polygons from entire dataset
uc.p <- st_simplify(uc.p, preserveTopology = T, dTolerance = 100) # fix the polygons by simplifying
uc <- rbind(uc, uc.p) # add back to the original data

# identify the self-overlaps of UCs
uc.overlaps <- st_intersection(uc)
# plot(uc.overlaps["n.overlaps"])

# make datasets that don't overlap, one for each category of PA
UCstrict_no.overlaps <- uc.overlaps[which(uc.overlaps$LTcateg == "PI" & uc.overlaps$n.overlaps == 1),]
UCsustuse_no.overlaps <- uc.overlaps[which(uc.overlaps$LTcateg == "US" & uc.overlaps$n.overlaps == 1),]
nrow(rbind(UCstrict_no.overlaps,UCsustuse_no.overlaps))
length(unique(uc$id)) == length(unique(uc.overlaps$id)) # NOTE: we lose 2 areas that were completely within others

# B.2) Clean self-overlaps within indigenous lands ----

# clean self-overlapping inds
# ind.overlaps <- st_intersection(ind)
# # find issues in the indigenous lands too
# for (i in 1:nrow(ind))
# {
#   intersection_issue <- st_intersection(ind[1:i,])
#   print(i)
# }
ind.p <- ind[which(ind$id == "IN-109"),]
ind <- ind[which(ind$id != "IN-109"), ]
ind.p <- st_simplify(ind.p, preserveTopology = T, dTolerance = 100)
ind.overlaps <- st_intersection(rbind(ind, ind.p))
# plot(ind.overlaps["n.overlaps"])
ind_no.overlaps <- ind.overlaps[which(ind.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]

# C) Find overlapping and non-overlapping polygons ----
# C.1) Overall overlaps between UCs and indigenous: ----

# using the UCs and indigenous that have been pre-cleaned from self-intersections, just in case
overall.overlaps <- st_intersection(uc.overlaps[which(uc.overlaps$n.overlaps == 1),1:2], ind_no.overlaps)
unique(st_geometry_type(overall.overlaps))
polys <- overall.overlaps[grep("GEOMETRY", (st_geometry_type(overall.overlaps))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra poygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
overall.overlaps <- overall.overlaps[-grep("GEOMETRY", (st_geometry_type(overall.overlaps))),] # remove them
overall.overlaps <- rbind(overall.overlaps, polys.fixed) # bind them back in

setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(overall.overlaps, "PAs-indigenous.shp", append = F)



# C.2) Strictly protected x  indigenous ----
ind_x_strict <- rbind(UCstrict_no.overlaps[,c("LTcateg", "id", "geometry")], ind_no.overlaps)
# only run in order to debug this intersection 
# for (i in 1285:nrow(rbind(ind_x_strict2, ind.solved)))
# {
#   intersection_issue <- st_intersection(rbind(ind_x_strict2, ind.solved[2:4,])[1:i,])
#   print(i)
# }
ind.problems <- ind_x_strict[which(ind_x_strict$id == "IN-183" | ind_x_strict$id == "IN-294" | ind_x_strict$id == "IN-295" | ind_x_strict$id == "IN-424"),]
ind.solved <- st_simplify(ind.problems, preserveTopology = T, dTolerance = 100) 
ind.solved <- st_collection_extract(ind.solved, "POLYGON") # fix the linestring geometries in IN-294 
# (note, don't need to aggregate by id here, as resuted in same amount of obs)
ind_x_strict2 <- ind_x_strict[which(ind_x_strict$id != "IN-183" & ind_x_strict$id != "IN-294" & ind_x_strict$id != "IN-295" & ind_x_strict$id != "IN-424"),]
# how sustainable use ucs overlap with indigenous
ind.ucstrict_overlaps <- st_intersection(rbind(ind_x_strict2, ind.solved)) # ok although this didn't work, i manually checked whether these overlapped with UCs, and no
# so for now my solution is to simply run the intersection for this section
ind.ucstrict_overlaps <- st_intersection(ind_x_strict2) 
# plot(ind.ucstrict_overlaps["n.overlaps"])
# make sure to deal with ind that don't overlap together later


# Write  out only PI UCs that don't overlap
UCstrict_CLEAN <- ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps == 1 & ind.ucstrict_overlaps$LTcateg == "PI"),]
# however, there was an error in writing out because there are some linestring geometries
polys <- UCstrict_CLEAN[grep("GEOMETRY", (st_geometry_type(UCstrict_CLEAN))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra poygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
UCstrict_CLEAN <- UCstrict_CLEAN[-grep("GEOMETRY", (st_geometry_type(UCstrict_CLEAN))),] # remove them
UCstrict_CLEAN <- rbind(UCstrict_CLEAN, polys.fixed) # bind them back in

# write out strictly protected areas that don't overlap with indigenous
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(UCstrict_CLEAN[,1:2], "PA_strict.shp", append = F) 
# plot(UCstrict_CLEAN$geometry)


# C.3) Sustainable use x indigenous ----
ind_x_sust <- rbind(UCsustuse_no.overlaps[,c("LTcateg", "id", "geometry")], ind_no.overlaps)
# ind_x_sust_overlaps <- st_intersection(ind_x_sust)
# debug
# for (i in 737:nrow(ind_x_sust))
# {
#   intersection_issue <- st_intersection(ind_x_sust[1:i,])
#   print(i)
# }
more.problems <- ind_x_sust[which(ind_x_sust$id == "IN-314" | ind_x_sust$id == "UC-1104" | ind_x_sust$id == "IN-295" | ind_x_sust$id == "IN-424"),]
ind_x_sust <- ind_x_sust[which(ind_x_sust$id != "IN-314" & ind_x_sust$id != "UC-1104" & ind_x_sust$id != "IN-295" & ind_x_sust$id != "IN-424"),]
ind.solved2 <- st_simplify(more.problems, preserveTopology = T, dTolerance = 100)
ind.solved2 <- st_collection_extract(ind.solved2, "POLYGON")
ind.solved2 <- ind.solved2 %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() 
ind.solved2 <- st_as_sf(inner_join(st_drop_geometry(more.problems), ind.solved2, by = "id")) 

ind_x_sust_overlaps <- st_intersection(rbind(ind_x_sust, ind.solved2)) # also isn't working, so i'll have to manually check 
# plot(more.problems["id"]) # no overlaps, except for ID-314 with an APA (which i'm disregarding anyway)
ind_x_sust_overlaps <- st_intersection(ind_x_sust) # run for indigenous-sustainable use overlaps
# plot(ind_x_sust_overlaps["n.overlaps"])

# separate categories, write out
ind_x_sust_CLEAN <- ind_x_sust_overlaps[which(ind_x_sust_overlaps$n.overlaps == 1 & ind_x_sust_overlaps$LTcateg == "US"),]
ind_x_sust_CLEAN <- rbind(ind_x_sust_CLEAN[,1:2], ind.solved2[which(ind.solved2$LTcateg == "US"),])
# fix error in linestring geometries
polys <- ind_x_sust_CLEAN[grep("GEOMETRY", (st_geometry_type(ind_x_sust_CLEAN))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra poygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
ind_x_sust_CLEAN <- ind_x_sust_CLEAN[-grep("GEOMETRY", (st_geometry_type(ind_x_sust_CLEAN))),] # remove them
ind_x_sust_CLEAN <- rbind(ind_x_sust_CLEAN, polys.fixed) # bind them back in

# write out sustainable use areas that don't overlap with indigenous:
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(ind_x_sust_CLEAN[,1:2], "PA_sustuse.shp", append = F)



# C.4) indigenous that don't overlap ----
# indigenous no overlap with strict PAs + indigenous with geometry issues but that don't overlap with strict PAs
i1 <- rbind(ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps == 1 & ind.ucstrict_overlaps$LTcateg == "indigenous"),c("LTcateg", "id")], ind.solved)
# indigenous no overlap with sustainable use + indigenous with geometry issues but no overlap with sustainable use
i2 <- rbind(ind_x_sust_overlaps[which(ind_x_sust_overlaps$n.overlaps == 1 & ind_x_sust_overlaps$LTcateg == "indigenous"),c("LTcateg", "id")], ind.solved2[which(ind.solved2$LTcateg == "indigenous"),])
indigenous_CLEAN <- rbind(i1,i2)
length(unique(indigenous_CLEAN$id))
nrow(indigenous_CLEAN)
# make sure i keep the observation which was "eaten off" resulting from the st_intersection
# which would have made a smaller in area observation
indigenous_CLEAN$area <- st_area(indigenous_CLEAN)
indigenous_CLEAN <- st_as_sf(as.data.frame(indigenous_CLEAN %>% arrange(area) %>% group_by(id) %>% slice(1)))
# fix error in linestring geometries
polys <- indigenous_CLEAN[grep("GEOMETRY", (st_geometry_type(indigenous_CLEAN))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra poygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
indigenous_CLEAN <- indigenous_CLEAN[-grep("GEOMETRY", (st_geometry_type(indigenous_CLEAN))),] # remove them
indigenous_CLEAN <- rbind(indigenous_CLEAN, polys.fixed) # bind them back in
plot(indigenous_CLEAN$geometry)

setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(indigenous_CLEAN[,1:2], "indigenous.shp", append = F)


# ----
# FOR THE ONE WALL-TO-WALL with the minimum necessary of overlaps (as this will become one category)
# i could st_union/combine each category and rasterize that for the map

# a series of rules in order to interpret what the overlaps mean
# if UCs and IND overlap: should resolve, prioritize UCs
# if UCs and IRU-AST-PCT overlap: should note these overlaps as private on public (but only keep IRU and AST, and if AST then it's public on public)
# if UCs and RPPN overlap
# UCs + SIGEF: note overlap as private on public
# Ucs + SNCI: note overlap as private on public
# UCs + UND-OTHER: note as public on public

# SIGEF + SNCI: merge as private from INCRA
# IRU-AST + SIGEF-SNCI: merge as private on private but not problematic - simply due to different systems # this would potentially take foreeever
# 

# the only issue i found from manual inspection was:
plot(indigenous_CLEAN[which(indigenous_CLEAN$id == "IN-484"),])

overall.overlaps[grep("UC-136", overall.overlaps$id),]
plot(overall.overlaps[grep("UC-136", overall.overlaps$id),]$geometry)

# i think what's happening is that when i bind the two non-overlaps for the clean indigenous,
# and then i ask it to keep the larger one...?
# because what's happening is that the indigenous 484 isn't being "bitten off" in the way that it should
# only certain parts were bitten off?
# so need to pick up here
plot(i1[grep("484", i1$id),]$geometry)
plot(i2[grep("484", i2$id),]$geometry) # i thiiiink it needs to be this one? or both of them together? what a mess. 
