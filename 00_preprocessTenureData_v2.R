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
uc_no.overlaps <- uc.overlaps[which(uc.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]

# # make datasets that don't overlap, one for each category of PA
# UCstrict_no.overlaps <- uc.overlaps[which(uc.overlaps$LTcateg == "PI" & uc.overlaps$n.overlaps == 1),]
# UCsustuse_no.overlaps <- uc.overlaps[which(uc.overlaps$LTcateg == "US" & uc.overlaps$n.overlaps == 1),]
# nrow(rbind(UCstrict_no.overlaps,UCsustuse_no.overlaps))
# length(unique(uc$id)) == length(unique(uc.overlaps$id)) # NOTE: we lose 2 areas that were completely within others

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
overall.overlaps <- st_intersection(uc_no.overlaps, ind_no.overlaps)
unique(st_geometry_type(overall.overlaps))
polys <- overall.overlaps[grep("GEOMETRY", (st_geometry_type(overall.overlaps))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra poygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
overall.overlaps <- overall.overlaps[-grep("GEOMETRY", (st_geometry_type(overall.overlaps))),] # remove them
overall.overlaps <- rbind(overall.overlaps, polys.fixed) # bind them back in

setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(overall.overlaps, "PAs-indigenous.shp", append = F)

# C.ALTERNATIVE VERSION: find all no.overlaps at the same time ----
uc_x_ind <- rbind(uc_no.overlaps, ind_no.overlaps)
# there are always geometry errors with these, so they need to be handled separately
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


# first filter out data that doesn't overlap
no.overlaps <- uc_x_ind_overlaps[which(uc_x_ind_overlaps$n.overlaps == 1),]
# get back polygons that made the intersection bug
no.overlaps2 <- rbind(no.overlaps[,c("LTcateg", "id", "geometry")], uc_x_ind_problems)

# need to fix resulting linestring geometries 
uc_x_ind_overlaps_CLEAN <- no.overlaps2
unique(st_geometry_type(uc_x_ind_overlaps_CLEAN))
# get all the geometries which are not polygons (i.e., lines and points)
pgeometries <- uc_x_ind_overlaps_CLEAN[which(st_geometry_type(uc_x_ind_overlaps_CLEAN) != "MULTIPOLYGON" &
                                               st_geometry_type(uc_x_ind_overlaps_CLEAN) != "POLYGON"),]
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON") # extract only the polygons
extract.pgeometries <- extract.pgeometries %>% group_by(id) %>% summarize(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() 
# get back all the original information from these problem geometries
fixed.pgeometries <- st_as_sf(inner_join(st_drop_geometry(uc_x_ind_overlaps_CLEAN), extract.pgeometries, by = "id")) 
# replace problem geos with the fixed geos in clean df
uc_x_ind_overlaps_CLEAN <- uc_x_ind_overlaps_CLEAN[which(st_geometry_type(uc_x_ind_overlaps_CLEAN) == "MULTIPOLYGON" |
                                                            st_geometry_type(uc_x_ind_overlaps_CLEAN) == "POLYGON"),]
uc_x_ind_overlaps_CLEAN <- rbind(uc_x_ind_overlaps_CLEAN, fixed.pgeometries)
unique(st_geometry_type(uc_x_ind_overlaps_CLEAN))

# deal with polygons that have been separated as a result of the intersection function 
uc_x_ind_overlaps_CLEAN
nrow(uc_x_ind_overlaps_CLEAN)
length(unique(uc_x_ind_overlaps_CLEAN$id))

# check the duplicated ids
head(uc_x_ind_overlaps_CLEAN[duplicated(uc_x_ind_overlaps_CLEAN$id),])
uc_x_ind_overlaps_CLEAN[which(uc_x_ind_overlaps_CLEAN$id == "UC-19"),] 
# union the stray polygons that became separated through the creation of the overlapping polygons
no.overlaps_clean <- uc_x_ind_overlaps_CLEAN %>% 
  group_by(LTcateg, id) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  st_as_sf() 
# plot(no.overlaps_clean$geometry)
no.overlaps_clean
length(unique(no.overlaps_clean$id))



# write out one at a time for cleannes!
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(no.overlaps_clean[which(no.overlaps_clean$LTcateg == "PI"),], "PA_strict.shp", append = F)
st_write(no.overlaps_clean[which(no.overlaps_clean$LTcateg == "US"),], "PA_sustuse.shp", append = F)
st_write(no.overlaps_clean[which(no.overlaps_clean$LTcateg == "indigenous"),], "indigenous.shp", append = F)


# deal with parts that do overlap ----
# i actually need to do an st_intersection of ("ucs that don't self-overlaps", "inds that dont self overlap")
# otherwise, i'll get ALL the polygons that had overlaps (with a column that tells me which one)
# what i want, is the AREAS that overlap, which i'll summarize into the corresponding id's
uc_no.overlaps
overlaps
nrow(overlaps)
length(unique(overlaps$id))
overlaps.clean <- overlaps %>%
  group_by(LTcateg, id) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_as_sf()
plot(overlaps.clean[,"LTcateg"]) # what?? i don't get this. why is is so much, so big?

setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(overlaps.clean, "UCs-indigenous.shp", append = F)

# dont forget about the errors you took out earlier!!!!
# Write  out only PI UCs that don't overlap
UCstrict_CLEAN <- ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps == 1 & ind.ucstrict_overlaps$LTcateg == "PI"),]
# however, there was an error in writing out because there are some linestring geometries
polys <- UCstrict_CLEAN[grep("GEOMETRY", (st_geometry_type(UCstrict_CLEAN))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra polygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
UCstrict_CLEAN <- UCstrict_CLEAN[-grep("GEOMETRY", (st_geometry_type(UCstrict_CLEAN))),] # remove them
UCstrict_CLEAN <- rbind(UCstrict_CLEAN, polys.fixed) # bind them back in

# write out strictly protected areas that don't overlap with indigenous
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
# st_write(UCstrict_CLEAN[,1:2], "PA_strict.shp", append = F) 
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

# ind_x_sust_overlaps <- st_intersection(rbind(ind_x_sust, ind.solved2)) # also isn't working, so i'll have to manually check 
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
# st_write(ind_x_sust_CLEAN[,1:2], "PA_sustuse.shp", append = F)



# C.4) indigenous that don't overlap ----
# indigenous no overlap with strict PAs + indigenous with geometry issues but that don't overlap with strict PAs
i1 <- rbind(ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps == 1 & ind.ucstrict_overlaps$LTcateg == "indigenous"),c("LTcateg", "id")], ind.solved)
# indigenous no overlap with sustainable use + indigenous with geometry issues but no overlap with sustainable use
i2 <- rbind(ind_x_sust_overlaps[which(ind_x_sust_overlaps$n.overlaps == 1 & ind_x_sust_overlaps$LTcateg == "indigenous"),c("LTcateg", "id")], ind.solved2[which(ind.solved2$LTcateg == "indigenous"),])
indigenous_CLEAN <- rbind(i1,i2)
length(unique(indigenous_CLEAN$id))
nrow(indigenous_CLEAN)
# make sure i keep the observation which was "eaten off" resulting from the st_intersection
# which initially i thought would have made a smaller in area observation, but not necessarily
# as i found out from plotting it later IND-484
indigenous_CLEAN$area <- st_area(indigenous_CLEAN)
test <- indigenous_CLEAN %>% arrange(area) %>% group_by(id)
par(mfrow = c(1,2))
plot(test[which(test$id == "IN-484"),][1,]$geometry)
plot(test[which(test$id == "IN-484"),][2,]$geometry)
dev.off()
indigenous_CLEAN <- st_as_sf(as.data.frame(indigenous_CLEAN %>% arrange(area) %>% group_by(id) %>% slice(1)))
# fix error in linestring geometries
polys <- indigenous_CLEAN[grep("GEOMETRY", (st_geometry_type(indigenous_CLEAN))),] # identify these linestrings
polys.extract <- st_collection_extract(polys, "POLYGON") # fix 
polys.extract <- polys.extract %>% group_by(id) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>% st_as_sf() # aggregate resulting extra poygons by the id
polys.fixed <- st_as_sf(inner_join(st_drop_geometry(polys), polys.extract, by = "id")) # keep all the original information
indigenous_CLEAN <- indigenous_CLEAN[-grep("GEOMETRY", (st_geometry_type(indigenous_CLEAN))),] # remove them
indigenous_CLEAN <- rbind(indigenous_CLEAN, polys.fixed) # bind them back in
# plot(indigenous_CLEAN$geometry)

setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
# st_write(indigenous_CLEAN[,1:2], "indigenous.shp", append = F)


# ----
# FOR THE ONE WALL-TO-WALL with the minimum necessary of overlaps (as this will become one category)
# i could st_union/combine each category and rasterize that for the map

# a series of rules in order to interpret what the overlaps mean
# SIGEF + SNCI: merge as private from INCRA
# IRU-AST + SIGEF-SNCI: merge as private on private but not problematic - simply due to different systems # this would potentially take foreeever


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
