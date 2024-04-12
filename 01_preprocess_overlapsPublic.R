#### Biodiversity and Tenure in Brazil ####

# public lands #

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


# A) UPON FIRST RUN, Read and standardize data: ----

# # protected areas
# setwd(paste0(wdmain,"/data/processed/"))
# uc <- st_read("landTenure_UC/landTenure_UCs_MMA_20231212_SAalbers.shp", stringsAsFactors = F, options = "ENCODING=latin1")
# uc <- st_transform(uc, my_crs_SAaea) # fix projection
# uc$subcateg <- uc$LTcateg # add/clear up this column
# uc$LTcateg <- uc$group
# uc$id <- paste0("UC-", 1:nrow(uc))
# uc <- select(uc, c("LTcateg", "id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/public"))
# st_write(uc, "protectedAreas.shp", append = F)
# 
# # indigenous lands
# setwd(paste0(wdmain,"/data/processed/"))
# ind <- st_read("landTenure_IND/landTenure_indigenous_20231212_SAalbers.shp")
# ind <- st_transform(ind, my_crs_SAaea)
# ind$LTcateg <- "indigenous"
# ind$id <- paste0("IN-", 1:nrow(ind))
# ind <- select(ind, c("LTcateg","id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/public"))
# st_write(ind, "indigenous.shp", append = F)
# 
# 
# # undesignated lands
# setwd(paste0(wdmain,"/data/processed/"))
# und <- st_read("landTenure_UND-OTH/landTenure_UND-OTH_SAalbers.shp")
# und <- st_transform(und, my_crs_SAaea)
# und$LTcateg <- und$protecao
# und$id <- paste0("UND-", 1:nrow(und))
# und <- select(und, c("LTcateg", "id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/public"))
# st_write(und, "undesignated-oth.shp", append = F)
# 
# # rural settlements
# setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT"))
# l <- list.files()
# ast <- lapply(l[grep(".shp", l)], st_read)
# ast <- do.call(rbind, ast)
# ast
# unique(ast$LTcateg)
# ast <- ast[which(ast$LTcateg == "AST"),]
# ast$id <- paste0("AST-", 1:nrow(ast))
# ast <- select(ast, c("LTcateg","id", "geometry"))
# setwd(paste0(wdmain,"/data/processed/processed2/public"))
# st_write(ast, "ruralSettlements.shp", append = F)

# B) Cleaning ----

# upon first run, clear workspace and read the data back in where needed



# B.1) Clean self-overlaps within UCs datasets ----
setwd(paste0(wdmain,"/data/processed/processed2/public"))
uc <- st_read("protectedAreas.shp")
# first fix two features which prevented running st_intersection
uc.p <- uc[which(uc$id == "UC-1047" | uc$id == "UC-1055"),]
uc <- uc[which(uc$id != "UC-1047" & uc$id != "UC-1055"),,] # remove problem polygons from entire dataset
uc.p <- st_simplify(uc.p, preserveTopology = T, dTolerance = 100) # fix the polygons by simplifying
uc <- rbind(uc, uc.p) # add back to the original data

# get the self-overlaps within UCs
uc.overlaps <- st_intersection(uc)

# create df with no overlaps within
uc_no.overlaps <- uc.overlaps[which(uc.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]

# create and clean df with only the self-overlaps, and make them their own category
unique(st_geometry_type(uc.overlaps[which(uc.overlaps$n.overlaps > 1),]))
pgeometries <- uc.overlaps[which(uc.overlaps$n.overlaps > 1),]
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
uc.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
# plot(uc.selfOverlaps$geometry, col = "gray30")

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(uc.selfOverlaps, "PAs_selfOverlaps.shp", append = F)



# B.2) Clean self-overlaps within indigenous lands ----
setwd(paste0(wdmain,"/data/processed/processed2/public"))
ind <- st_read("indigenous.shp")
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

# create and clean df with only the self-overlaps, and make them their own category
unique(st_geometry_type(ind.overlaps[which(ind.overlaps$n.overlaps > 1),]))
pgeometries <- ind.overlaps[which(ind.overlaps$n.overlaps > 1),]
extract.pgeometries <- st_collection_extract(pgeometries, "POLYGON")
ind.selfOverlaps <- extract.pgeometries %>% group_by(LTcateg, id) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
# plot(ind.selfOverlaps$geometry, col = "gray30")

# write out this data
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
st_write(ind.selfOverlaps, "indigenous_selfOverlaps.shp", append = F)



# B.3) Clean self-overlaps within UND ----
setwd(paste0(wdmain,"/data/processed/processed2/public"))
und <- st_read("undesignated-oth.shp")

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




# B.4) Clean self-overlaps within AST ----
# I determined this was unnecessary bc I tested the intersections with all other categories and they worked fine
# the st_intersection with itself, however, i never managed to get to work. 

setwd(paste0(wdmain,"/data/processed/processed2/public"))
ruset <- st_read("ruralSettlements.shp")
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
st_write(ruset, "ruralSettlements.shp", append = F)





# C) Find overlapping non-overlapping polygons  ----


# C.1) Overlaps between UCs and indigenous: ----

# arguably this was the biggest, most important one one, as entire areas were overlapping just from seeing it on the map
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

# C.2) other overlaps ----
# since a lot of these overlaps will actually just be slivers of polygons overlapping
# i need to know how "seriously" to take these. 
# hence, here i want to make a table that quantifies these overlaps as a proportion of the total area
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
overlap_indPAS <- st_read("PAs-indigenous.shp")

setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
l <- list.files()
l[grep(".shp", l)]
LT_no.overlaps <- lapply(l[grep(".shp", l)], st_read)
names(LT_no.overlaps) <- gsub(".shp", "", l[grep(".shp", l)])

# find out percent overlaps of rural settlements
table_rSettlements <- data.frame("categ" = NA, "area_Int"= NA, "area_Categ" = NA,"area_B" = NA)
for(i in 1:length(names(LT_no.overlaps))){
  if(names(LT_no.overlaps)[i] == "ruralSettlements"){next}
  
  table_rSettlements[i,1] <- paste0(names(LT_no.overlaps)[4],"_x_", names(LT_no.overlaps)[i])
  # fix crs
  LT_no.overlaps[[i]] <- st_transform(LT_no.overlaps[[i]], crs(LT_no.overlaps[[4]]))
  # make intersection
  intersection <- st_intersection(LT_no.overlaps[[i]],LT_no.overlaps[[4]])
  print(intersection)
  # get the sums of the areas in order to calculate percentages and also have the areas
  # note everything is converted to km2
  table_rSettlements[i,2] <- sum(st_area(intersection))/1000
  table_rSettlements[i,3] <- sum(st_area(LT_no.overlaps[[i]]))/1000
  table_rSettlements[i,4] <- sum(st_area(LT_no.overlaps[[4]]))/1000
}
# find out percent overlaps of undesignated lands
table_undesignated <- data.frame("categ" = NA, "area_Int"= NA, "area_Categ" = NA,"area_B" = NA)
for(i in 1:length(names(LT_no.overlaps))){
  if(names(LT_no.overlaps)[i] == "undesig-other"){next}
  
  table_undesignated[i,1] <- paste0(names(LT_no.overlaps)[5],"_x_", names(LT_no.overlaps)[i])
  # fix crs
  LT_no.overlaps[[i]] <- st_transform(LT_no.overlaps[[i]], crs(LT_no.overlaps[[5]]))
  # make intersection
  intersection <- st_intersection(LT_no.overlaps[[i]],LT_no.overlaps[[5]])
  print(intersection)
  # get the sums of the areas in order to calculate percentages and also have the areas
  # note everything is converted to km2
  table_undesignated[i,2] <- sum(st_area(intersection))/1000
  table_undesignated[i,3] <- sum(st_area(LT_no.overlaps[[i]]))/1000
  table_undesignated[i,4] <- sum(st_area(LT_no.overlaps[[5]]))/1000
}


# these tables show me that the only categories that overlap more than 1% are:
rbind(table_rSettlements, table_undesignated)
# rural settlements and sustainable use PAs, where 6.5% of rural settlements overlap with sust use PAs (and 3.5% of SU PAs overlap with rural settlements)
# as well as undesignated lands and rural settlements, where 1.35% of rural settlements are overlapped by undesignated lands
# therefore, let's write out this table in order to report it:
setwd(paste0(wdmain, "/output/"))
# also note, i'm using "B" for "base" comparison
write.csv(rbind(table_rSettlements, table_undesignated), "publicLandOverlaps_areakm2.csv", row.names = F)


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






# get the public data overlaps


