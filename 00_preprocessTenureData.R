#### Biodiversity and Tenure in Brazil ####

# this script brings together the pre-processed tenure data and
# 1) cleans and fixes self-overlaps of data
# 2) identifies the overlapping areas across datasets: NOTE, should do this one at a time in order to keep track of overlaps
# 3) creates the categories for these overlaps themselves

# this creates the outputs:

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

# create df with no overlaps
uc_no.overlaps <- uc.overlaps[which(uc.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]

# rescue polygons that overlapped and make them their own category
unique(st_geometry_type(uc.overlaps[which(uc.overlaps$n.overlaps > 1),]))
pgeometries <- uc.overlaps[which(uc.overlaps$n.overlaps > 1),]
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
uc.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
plot(uc.selfOverlaps$geometry, col = "gray30")

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(uc.selfOverlaps, "PAs_selfOverlaps.shp", append = F)

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
# create df with no overlaps
ind_no.overlaps <- ind.overlaps[which(ind.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]

# rescue polygons that overlapped and make them their own category
unique(st_geometry_type(ind.overlaps[which(ind.overlaps$n.overlaps > 1),]))
pgeometries <- ind.overlaps[which(ind.overlaps$n.overlaps > 1),]
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
ind.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
plot(ind.selfOverlaps$geometry, col = "gray30")

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(ind.selfOverlaps, "indigenous_selfOverlaps.shp", append = F)


# C) Find overlapping and non-overlapping polygons indigenous - PAs ----

# C.1) Overall overlaps between UCs and indigenous: ----

# using the UCs and indigenous that have been pre-cleaned from self-intersections, just in case
overall.overlaps <- st_intersection(uc_no.overlaps, ind_no.overlaps)
unique(st_geometry_type(overall.overlaps))
# identify these linestrings and fix
polys <- overall.overlaps[grep("GEOMETRY", (st_geometry_type(overall.overlaps))),] 
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
# aggregate resulting extra polygons by the id
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() 
# keep all the original information
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) 
# remove the wrong geometries
overall.overlaps <- overall.overlaps[-grep("GEOMETRY", (st_geometry_type(overall.overlaps))),] 
# bind the correct geometries back in
overall.overlaps <- rbind(overall.overlaps, polys.fixed)

setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(overall.overlaps, "PAs-indigenous.shp", append = F)

# C.2) get UCs and indigenous lands that DO NOT overlap ----

uc_x_ind <- rbind(uc_no.overlaps, ind_no.overlaps)
# remove these polygons from the data because they prevent st_intersection to run
# no worries, they will be inserted back in later
uc_x_ind2 <- uc_x_ind[which(uc_x_ind$id != "IN-183" & uc_x_ind$id != "IN-294" & uc_x_ind$id != "IN-295" & uc_x_ind$id != "IN-424" &
                            uc_x_ind$id != "IN-314" & uc_x_ind$id != "UC-770" & uc_x_ind$id != "UC-1104"& uc_x_ind$id != "UC-859"),]
# # only run in order to debug this intersection 
# for (i in 768:nrow(uc_x_ind2))
# {
#   intersection_issue <- st_intersection(uc_x_ind2[1:i,])
#   print(i)
# }
uc_x_ind_problems <- uc_x_ind[which(uc_x_ind$id == "IN-183" | uc_x_ind$id == "IN-294" | uc_x_ind$id == "IN-295" | uc_x_ind$id == "IN-424" |
                              uc_x_ind$id == "IN-314" | uc_x_ind$id == "UC-770" | uc_x_ind$id == "UC-1104" | uc_x_ind$id == "UC-859"),]

uc_x_ind_overlaps <- st_intersection(uc_x_ind2)
# plot(uc_x_ind_overlaps[,"n.overlaps"])


# now filter out data that doesn't overlap
# (note, i already have everything that *does* overlap)
no.overlaps <- uc_x_ind_overlaps[which(uc_x_ind_overlaps$n.overlaps == 1),]
# get back polygons that made the intersection bug
no.overlaps2 <- rbind(no.overlaps[,c("LTcateg", "id", "geometry")], uc_x_ind_problems)

# fix resulting linestring geometries 
uc_x_ind_overlaps_CLEAN <- no.overlaps2
unique(st_geometry_type(uc_x_ind_overlaps_CLEAN))
# get all the geometries which are not polygons (i.e., lines and points)
pgeometries <- uc_x_ind_overlaps_CLEAN[which(st_geometry_type(uc_x_ind_overlaps_CLEAN) != "MULTIPOLYGON" &
                                               st_geometry_type(uc_x_ind_overlaps_CLEAN) != "POLYGON"),]
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON") # extract only the polygons
fixed.pgeometries <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() 
# replace problem geos with the fixed geos in clean df
uc_x_ind_overlaps_CLEAN <- uc_x_ind_overlaps_CLEAN[which(st_geometry_type(uc_x_ind_overlaps_CLEAN) == "MULTIPOLYGON" |
                                                            st_geometry_type(uc_x_ind_overlaps_CLEAN) == "POLYGON"),]
uc_x_ind_overlaps_CLEAN <- rbind(uc_x_ind_overlaps_CLEAN, fixed.pgeometries)
unique(st_geometry_type(uc_x_ind_overlaps_CLEAN))

uc_x_ind_overlaps_CLEAN
length(unique(uc_x_ind_overlaps_CLEAN$id))



# write out one at a time to keep things organized!
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(uc_x_ind_overlaps_CLEAN[which(uc_x_ind_overlaps_CLEAN$LTcateg == "PI"),], "PA_strict.shp", append = F)
st_write(uc_x_ind_overlaps_CLEAN[which(uc_x_ind_overlaps_CLEAN$LTcateg == "US"),], "PA_sustuse.shp", append = F)
st_write(uc_x_ind_overlaps_CLEAN[which(uc_x_ind_overlaps_CLEAN$LTcateg == "indigenous"),], "indigenous.shp", append = F)



# ----

# a series of rules in order to interpret what the overlaps mean
# SIGEF + SNCI: merge as private from INCRA
# IRU-AST + SIGEF-SNCI: merge as private on private but not problematic - simply due to different systems # this would potentially take foreeever

