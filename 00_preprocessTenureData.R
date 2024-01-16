#### Biodiversity and Tenure in Brazil ####

# this script brings together the pre-processed tenure data 
# and creates the outputs:
# 1) one version with no overlaps for rasterizing (or rather, with overlaps as one category)
# 2) one version with overlaps for extracting 

# is this a script to deal with the overlaps overall?


# libraries
library(terra)
library(sf)
library(geos)
library(dplyr)


# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))

# Overlaps of PAs/UCs and indigenous lands: ----

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

# TEST intersection of UCs and IND----
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


# WHAT HAVE I LEARNED? ----
# i need to go with st_intersection! do it phases so that it's easy to know what's waht.

# PAS ARE SELF-OVERLAPPING. HOW DO I DEAL WITH THIs before dealing with the rest of overlaps??
# i should simply run an st_intersection on itself, and keep whatever doesn't overlap
uc <- st_buffer(uc, dist=0)
uc <- lwgeom::st_make_valid(uc)
unique(st_geometry_type(uc))
uc <- sf::st_cast(uc, "MULTIPOLYGON")
unique(st_is_valid(uc))
unique(st_is_empty(uc))
class(st_geometry_type(uc))

# find bug
uc2 <- uc[-1047,] #problem 1
for(i in 1050:nrow(uc3))
{
  find.bug <- st_intersection(uc2[1:i,])
  print(i)
}
uc3 <- uc2[-1054,]
for(i in 1053:nrow(uc3))
{
  find.bug <- st_intersection(uc3[1:i,])
  print(i)
}

uc.overlaps <- st_intersection(uc3)
# fix the features that were problematic
plot(uc[1047,]$geometry)
p1 <- st_simplify(uc[1047,], dTolerance = 10)
plot(p1$geometry)
plot(st_combine(uc[1047,]))
plot(uc2[1054,]$geometry)


uc.intersects <- st_intersects(uc)
length(uc.intersects[which(lengths(uc.intersects)>1)])
length(uc.intersects[which(lengths(uc.intersects)==1)])
t <- uc[which(lengths(uc.intersects)>1),] # subset the ucs for which there was an intersection
length(lengths(uc.intersects)>1)
r[which(unlist(st_intersects(s, r)) == 1)]
ints <- legnths(t3)
mytest_nonoverlaps <- mytest %>% filter(ints == 1) 

# split up into these parts
uc_strict <- uc[which(uc$LTcateg == "PI"),]
uc_sustuse <- uc[which(uc$LTcateg == "US"),]
test <- st_intersection(uc_strict)
plot(test["n.overlaps"])

# 
# ind <- st_buffer(ind, dist=0)
# repeat fixing the above
intersection_uc.ind  <-  # returns geometry of the shared portion of x and y
plot(intersection_uc.ind$geometry)
plot()
# try to remove the parts that overlapped from the original indigenous
ind_no.overlaps <- st_difference(ind[1:100,], intersection_uc.ind) # note st_diff is computationally expensive
plot(st_union(st_combine(ind_no.overlaps)))

par(mfrow = c(1,2))
plot(ind[1:100,])
plot(st_union(st_combine(ind_no.overlaps)))
# this is how far i got ###########

# find bug
for (i in 1250:nrow(mytest))
{
  non_overlaps <- st_intersection(mytest[1249:i,])
  print(i)
}

non_overlaps2 <- st_intersection(mytest[-1226,])

non_overlaps2 <- non_overlaps %>% filter()
filter(n.overlaps == 1)
plot(non_overlaps["polygon"])

# what's the difference with doing it with the two shapes separately?
t4 <- st_intersects(uc, ind)
str(t4)
# the difference is the number of observations i get - so for the intersects uc+ind,
# i get: per feature of uc, the number from ind that intersects

# should i just assign an ID, do the intersection, subtract it from the original, and then carry on?
# no because that's not exactly it
# it's more that i should USE the intersection to eat OFF of the indigenous

t5 <- st_intersects(uc[20,], ind) # smaller example of the first intersection i could find of a UC
tint <- st_intersection(uc[20,], ind) # do the intersection
ind_eaten <- st_difference(ind, tint) # geometry from x that does not intersect with y. eat off the intersection from the original (what shouldn't be there)
# test if the intersections remain in the one of interest
t6 <- st_intersects(uc[20,], ind_eaten)
lengths(t6) # more intersections AFTER?!?
tint2 <- st_intersection(uc[20,], ind_eaten)
plot(tint2$geometry)
eat_test <- st_difference(ind, t4)

# and THEN use intersects to quantify the amount of overlaps of other categories???
length(t5)

# shouldn't it actually be:
t5 <- st_difference(ind2[1:100,], uc2[1:100,], by_feature = TRUE) # EACH INDIVIDUAL INTERSECTION BECOMES AN OBSERVATION - WHICH IS WHY KEEPING AN ID WOULD COME IN HANDY
t6 <- st_difference(ind2[1:100,], uc2[1:100,], by_feature = FALSE)
# I DONT REALLY NEED THIS FOR THIS PARTICULAR CASE OF IND + UCS, BUT LATER ON, I WILL TO FIND OUT HOW MUCH AREA IS OVERLAPPINGPER EACH PROPERTY
# I WOULD HAVE TO GROUP THE ST_AREA COUNTS PER GROUP OF SPECIFIC ID
# check that worked
back <- t5 %>% group_by(id.1)

back <- t5 %>% mutate(feature_group = rep(1:nrow(ind2[1:100,]), times = sapply(geometry, function(x) length(x))))


uc[lengths(testintersection)==0,] # this keeps everythign where there's no intersection, correct?

# CHECK THIS: https://stackoverflow.com/questions/63024545/r-sf-find-polygons-outside-of-multiple-overlapping-polygons

# overlap - should quantify amount of overlap
induc_overlap <- st_overlaps(ind, uc) # ids if x and y share space, are of the same dimension, but are not completely contained by each other

# the easiest way I can think of prioritizing is 
# 1. create indigenous that doesn't touch ucs
ind_difference <- st_difference(ind, uc)
setwd(paste0(wdmain,"/data/processed/tests/"))
st_write(ind_difference, "testdifference_ind-uc.shp")

test_difference <- st_intersection(ind_difference, uc)

# ----
# ALL OF THIS FOR THE ONE WALL-TO-WALL with the minimum necessary of overlaps (as this will become one category)                       
# i need to come up with a series of rules
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

