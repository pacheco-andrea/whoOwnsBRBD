#### Biodiversity and Tenure in Brazil ####



# this script brings together the following pre-processed tenure data 
# indigenous lands, protected areas, other undesignated lands, and finally AST from CSR

# and 
# 1) cleans and fixes self-overlaps of data
# 2) identifies the overlapping areas across datasets: NOTE, should do this one at a time in order to keep track of overlaps
# 3) creates the categories for these overlaps themselves

# this creates the outputs:

# 1) one df (respectively) with all the self-overlaps 
# 2) one df (respectively) with no self-overlaps 
# 3) one df (respectively) with the overlaps^n (i.e., indigenous overlaps with PAs, indigenous overlaps with undesignated, etc.)


# libraries
library(terra)
library(sf)
library(geos)
library(dplyr)
library(ggplot2)
library(tidyverse)


# load directories and other settings
Sys.setenv(LANG = "en") # because i have no admin rights on the server...
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))

# A) Read and standardize data: ----

# protected areas
uc <- st_read("landTenure_UC/landTenure_UCs_MMA_20231212_SAalbers.shp", stringsAsFactors = F, options = "ENCODING=latin1")
uc <- st_transform(uc, my_crs_SAaea) # fix projection
uc$subcateg <- uc$LTcateg # add/clear up this column
uc$LTcateg <- uc$group
uc$id <- paste0("UC-", 1:nrow(uc))
uc <- select(uc, c("LTcateg", "id", "geometry"))

# indigenous lands
ind <- st_read("landTenure_IND/landTenure_indigenous_20231212_SAalbers.shp")
ind <- st_transform(ind, my_crs_SAaea)
ind$LTcateg <- "indigenous"
ind$id <- paste0("IN-", 1:nrow(ind))
ind <- select(ind, c("LTcateg","id", "geometry"))

# undesignated lands
und <- st_read("landTenure_UND-OTH/landTenure_UND-OTH_SAalbers.shp")
und <- st_transform(und, my_crs_SAaea)
und$LTcateg <- und$protecao
und$id <- paste0("UND-", 1:nrow(und))
und <- select(und, c("LTcateg", "id", "geometry"))

# rural settlements
setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT"))
l <- list.files()
ast <- lapply(l[grep(".shp", l)], st_read)
ast <- do.call(rbind, ast)
ast
unique(ast$LTcateg)
ast <- ast[which(ast$LTcateg == "AST"),]
ast$id <- paste0("AST-", 1:nrow(ast))
ast <- select(ast, c("LTcateg","id", "geometry"))

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



# B.3) Clean self-overlaps within UND ----

# find issues in undesignated lands too
st_is_valid(und)
und2 <- st_make_valid(und)
st_is_valid(und2)

und <- und2[which(st_is_valid(und2)),] # just making sure I only include what's valid
und.overlaps <- st_intersection(und)
length(unique(und.overlaps$id))
plot(und.overlaps[,"n.overlaps"])

# df with no overlaps
und_no.overlaps <- und.overlaps[which(und.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]
unique(st_geometry_type(und_no.overlaps))
# write out this data
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(und_no.overlaps, "undesig-other.shp", append = F)

# get the self-overlaps of undesignated lands
unique(st_geometry_type(und.overlaps[which(und.overlaps$n.overlaps > 1),]))
pgeometries <- und.overlaps[which(und.overlaps$n.overlaps > 1),] 
# note, out of 1694 features, only 5 were actual polygons - the rest were linestrings, etc.
# when plotted, these are practically insignificant
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
und.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
plot(und.selfOverlaps[,"LTcateg"])

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(und.selfOverlaps, "undesig-other_selfOverlaps.shp", append = F)


# B.4) Clean self-overlaps within AST


# C) Find overlapping non-overlapping polygons  ----

# C.1) Overlaps between UCs and indigenous: ----

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

# OTHER OVERLAPS ----
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
overlap_indPAS <- st_read("PAs-indigenous.shp")

setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
ind <- st_read("indigenous.shp")
pastr <- st_read("PA_strict.shp")
pasus <- st_read("PA_sustuse.shp")
und_no.overlaps <- st_read("undesig-other.shp")

# how do all these public lands overlap?

pasind <- rbind(ind, pastr, pasus)


pub.overlaps <- st_intersection(pasind, und_no.overlaps)
plot(pub.overlaps$geometry)
# will need to clean this of any weird geometries
unique(st_geometry_type(pub.overlaps))



# D) Get polygons that DO NOT overlap with each other (tenure^n) ----

# PAs and indigenous ----
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

# HERE MISSING: # ----

# PAs x undesignated

# indigenous x undesignated

# undesignated x AST

# PAs x AST

# indigenous x AST




rm(list=ls())

# get the public data overlaps


