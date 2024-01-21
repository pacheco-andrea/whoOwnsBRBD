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

# A.1) SMALL TEST intersection of UCs and IND to figure out what needs to be done ----
# test_uc <- uc[136,] #136, 246
# test_ind <- ind[484,]
# test <- rbind(test_ind, test_uc)
# plot(test["LTcateg"])
# 
# uc.ind_intersects <- st_intersects(test_uc, test_ind) # one intersection = intersects with one other feature (not every single line)
# uc.ind_intersection <- st_intersection(test_uc, test_ind) # returns geometry of the shared portion of x and y
# uc.ind_intersection
# plot(uc.ind_intersection$geometry) # can clearly see that it is indeed ONLY the areas that overlap
# # is there a difference in behavior when using one or two objects in the function?
# bound_intersection <- st_intersection(test) # clearly, yes. there is a difference. 
# # the bound_intersection has three observations instead of one. 
# # it KEEPS the original features shapes!!!
# plot(bound_intersection$geometry)
# plot(bound_intersection["n.overlaps"]) # we get this column
# plot(bound_intersection[1,]$geometry) # the first observation is all the indigenous with NO overlaps
# plot(bound_intersection[2,]$geometry) # the second, is the one with overlap
# plot(bound_intersection[3,]$geometry) # the third, is the part of UC with no overlaps
# 
# # TEST difference bt UCs and IND
# uc.ind_diff <- st_difference(test_uc, test_ind)
# uc.ind_diff # returns one feature - which is essentially feature 3 in the above example. the parts where they don't overlap! (the parts of the x that don't overlap with y)
# 
# # make what i wanted to make last week:
# resolved_uc.ind <- bound_intersection
# resolved_uc.ind[which(resolved_uc.ind$n.overlaps > 1),]$LTcateg <- "US"
# plot(resolved_uc.ind["LTcateg"])
# 
# # eventually would need to code something smart to replace the category with the one it was originally
# # OR - TO SIMPLIFY CODING, just do one rbound category at a time...
# places.to.resolve <- resolved_uc.ind[which(resolved_uc.ind$n.overlaps > 1),]
# 
# for(i in 1:length(places.to.resolve))
# {
#   origin <- places.to.resolve[i,]$origins
#   # get the category it should be replaced 
#   names.origins <- grep(max(origin[[1]]), resolved_uc.ind$origins)
# }


# A.2) Clean existing self-overlaps within UCs datasets ----

# first fix two features which prevented running st_intersection
ucp1 <- uc[1047,]
ucp2 <- uc[1055,]
uc <- uc[-c(1047,1055),] # remove problem polygons from entire dataset
ucp1 <- st_simplify(ucp1, preserveTopology = T, dTolerance = 100) # fix the polygons by simplifying
ucp2 <- st_simplify(ucp2, preserveTopology = T, dTolerance = 100) # fix the polygons by simplifying
uc <- rbind(uc, ucp1, ucp2) # add back to the original data
# identify the self-overlaps of UCs
uc.overlaps <- st_intersection(uc)
# plot(uc.overlaps["n.overlaps"])
# make datasets that don't overlap, one for each category of PA
UCstrict_no.overlaps <- uc.overlaps[which(uc.overlaps$LTcateg == "PI" & uc.overlaps$n.overlaps == 1),]
UCsustuse_no.overlaps <- uc.overlaps[which(uc.overlaps$LTcateg == "US" & uc.overlaps$n.overlaps == 1),]
nrow(rbind(UCstrict_no.overlaps,UCsustuse_no.overlaps))
length(unique(uc$id)) == length(unique(uc.overlaps$id)) # NOTE: we lose 2 areas that were completely within others

# A.3) Clean self-overlaps within indigenous lands ----

# clean self-overlapping inds
# ind.overlaps <- st_intersection(ind)
# # find issues in the indigenous lands too
# for (i in 1:nrow(ind))
# {
#   intersection_issue <- st_intersection(ind[1:i,])
#   print(i)
# }
indp1 <- ind[109,]
ind <- ind[-109, ]
indp1 <- st_simplify(indp1, preserveTopology = T, dTolerance = 100)
ind.overlaps <- st_intersection(rbind(ind, indp1))
plot(ind.overlaps["n.overlaps"])
ind.clean <- ind.overlaps[which(ind.overlaps$n.overlaps == 1), c("LTcateg", "id", "geometry")]

# A.3.1) Find how strict ucs overlap with indigenous ----
ind.strict <- rbind(UCstrict_no.overlaps[,c("LTcateg", "id", "geometry")], ind.clean)
# only run in order to debug this intersection 
# for (i in 1285:nrow(rbind(ind.strict2, ind.solved)))
# {
#   intersection_issue <- st_intersection(rbind(ind.strict2, ind.solved[2:4,])[1:i,])
#   print(i)
# }
ind.problems <- ind.strict[which(ind.strict$id == "IN-183" | ind.strict$id == "IN-294" | ind.strict$id == "IN-295" | ind.strict$id == "IN-424"),]
ind.solved <- st_simplify(ind.problems, preserveTopology = T, dTolerance = 1000)
ind.solved <- st_collection_extract(ind.solved, "POLYGON") # fix the linestring geometries in IN-294
ind.strict2 <- ind.strict[which(ind.strict$id != "IN-183" & ind.strict$id != "IN-294" & ind.strict$id != "IN-295" & ind.strict$id != "IN-424"),]
# how sustainable use ucs overlap with indigenous
ind.ucstrict_overlaps <- st_intersection(rbind(ind.strict2, ind.solved)) # ok although this didn't work, i manually checked whether these overlapped with UCs, and no
# so for now my solution is to simply: 
ind.ucstrict_overlaps <- st_intersection(ind.strict2) # run the intersection for this section
plot(ind.ucstrict_overlaps["n.overlaps"])
# create separate categories and write everything out
# PI UCs that don't overlap
completelyClean_UCstrict <- ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps == 1 & ind.ucstrict_overlaps$LTcateg == "PI"),]
setwd(paste0(wdmain, "data/processed/LT_cleanOverlaps"))
# st_write(completelyClean_UCstrict[,1:2], "PA_strict.shp", append = F)




# indigenous that don't overlap
ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps == 1 & ind.ucstrict_overlaps$LTcateg == "IN"),]
setwd(paste0(wdmain, "data/processed/LT_cleanOverlaps"))
# add the ind.solved to the ind that don't overlap


# write the overlaps:
ind.ucstrict_only.overlaps <- ind.ucstrict_overlaps[which(ind.ucstrict_overlaps$n.overlaps > 1),]
setwd(paste0(wdmain, "data/processed/LT_cleanOverlaps"))
st_write(ind.ucstrict_no.overlaps, "UCstrict-indigenous_no-overlap.shp")




# A.3.2 Find how sustainable use overlap with indigenous ----
ind.sust <- rbind(UCsustuse_no.overlaps[,c("LTcateg", "id", "geometry")], ind.clean)
ind.sust_overlaps <- st_intersection(ind.sust)
# for (i in 737:nrow(ind.sust))
# {
#   intersection_issue <- st_intersection(ind.sust[1:i,])
#   print(i)
# }
more.problems <- ind.sust[which(ind.sust$id == "IN-314"),]
ind.sust <- ind.sust[which(ind.sust$id != "IN-314" & ind.sust$id != "UC-1104" & ind.sust$id != "IN-295" & ind.sust$id != "IN-424"),]

setwd(paste0(wdmain, "data/processed/LT_cleanOverlaps"))

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


# TEST: ----

