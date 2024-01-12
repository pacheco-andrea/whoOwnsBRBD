#### Biodiversity and Tenure in Brazil ####

# this script brings together the pre-processed tenure data 
# and creates the outputs:
# 1) one version with no overlaps for rasterizing (or rather, with overlaps as one category)
# 2) one version with overlaps for extracting 

# is this a script to deal with the overlaps overall?


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)

# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# SF GEOMETRY OPERATIONS EXAMPLES ----
set.seed(131)

m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
p = st_polygon(list(m))
n = 100
l = vector("list", n)
for (i in 1:n)
  l[[i]] = p + 10 * runif(2)
s = st_sfc(l)
plot(s, col = sf.colors(categorical = TRUE, alpha = .5))
title("overlapping squares")

# functions
d = st_difference(s) # sequential differences: s1, s2-s1, s3-s2-s1, ...
plot(d, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping differences") 
# for these differences, the way to determine the order of prioritization would be to sort by variable/column
# im pretty sure what i need for prioritizing Ucs above indigenous is the difference operation
# so that Ucs take a bite out of indigenous, and then i can plot them without overlaps

# determine the order: sort by categories in the columns!! (but this is for one data set, would i have to join them first?)
# e.g. wdpa2 <- wdpa2 >%> arrange(category, year)

i = st_intersection(s) # all intersections
plot(i, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping intersections") # i don't get it. 
summary(lengths(st_overlaps(s, s))) # includes self-counts!
summary(lengths(st_overlaps(d, d)))
summary(lengths(st_overlaps(i, i)))
sf = st_sf(s)
i = st_intersection(sf) # all intersections
plot(i["n.overlaps"]) # i dont understand where they got this variable, how they created it
summary(i$n.overlaps - lengths(i$origins))

# A helper function that erases all of y from x:
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
e <- st_erase(i, sf)
poly = st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))
lines = st_multilinestring(list(
  cbind(c(0, 1), c(1, 1.05)),
  cbind(c(0, 1), c(0, -.05)),
  cbind(c(1, .95, 1), c(1.05, .5, -.05))
))
snapped = st_snap(poly, lines, tolerance=.1)
plot(snapped, col='red')
plot(poly, border='green', add=TRUE)
plot(lines, lwd=2, col='blue', add=TRUE) # this shows me snapping is NOT what i need

# THIS IS EXACTLY WHAT I NEED FOR TAKING BITES OUT OF SHAPES
set.seed(1)
m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
p = st_polygon(list(m))
n = 100
l = vector("list", n)
for (i in 1:n)
  l[[i]] = p + 2 * runif(2)


s = st_sf(polygon = 1:n, geometry = st_sfc(l))
s5 <- s[1:5, ]
plot(s5["polygon"])

non_overlaps <- st_intersection(s5) %>%
  filter(n.overlaps == 1)

plot(non_overlaps["polygon"])
non_overlaps$origins



# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))
grep("^landTenure", list.files())
tenfolds <- list.files()[grep("^landTenure_", list.files())]


# figure out spatial operations i need to use: ----
# try subsetting indigenous that are inside PAs
tenfolds
ind <- st_read("landTenure_IND/landTenure_indigenous_20231212_SAalbers.shp")
uc <- st_read("landTenure_UC/landTenure_UCs_MMA_20231212_SAalbers.shp")

ind <- st_transform(ind, my_crs_SAaea)
ind$LTcateg <- "indigenous"
uc <- st_transform(uc, my_crs_SAaea)
uc$subcateg <- uc$LTcateg
uc$LTcateg <- uc$group

# intersection?
testintersection  <- st_intersection(uc, ind) # returns geometry of the shared portion of x and y
nrow(testintersection)
plot(testintersection$geometry)
str(testintersection)
# remove the parts that overlapped from the original indigenous
ind_nooverlaps <- st_difference(ind[1:100,], testintersection)
nrow(ind)
nrow(ind_nooverlaps)
plot(st_union(ind_nooverlaps))

# INTERSECTS

i = st_intersection(sf) # all intersections
plot(i["n.overlaps"])
summary(i$n.overlaps - lengths(i$origins))

# try simply using an rbind
# st_join doesn't work, it's not keeping all the observations and the math isn't mathing
uc2 <- select(uc, c("LTcateg", "geometry"))
uc2$id <- paste0("UC-", 1:nrow(uc2))
uc2 <- st_buffer(uc2, dist = 0)
ind2 <- select(ind, c("LTcateg", "geometry"))
ind2$id <- paste0("IN-", 1:nrow(ind2))
ind2 <- st_buffer(ind2, dist = 0)

mytest <- rbind(uc2,ind2)
nrow(mytest)
plot(mytest$geometry)
t3 <- st_intersects(mytest)
length(lengths(t3)) # BINGO: THIS IS THE AMOUNTS OF INTERSECTS PER FEATURE - it also gives the specific feature it intersects with
mytest$ints <- lengths(t3)
plot(mytest[,"ints"]) # BINGO, this is amount of intersections per feature
mytest_nonoverlaps <- mytest %>% filter(ints == 1) # these are all the areas that didn't have overlaps

# try the intersection with this data
ucind_intersection <- st_intersection(uc2,ind2)
ind_nooverlaps <- st_difference(ind2[1:100,], ucind_intersection) # remove the overlapped parts from the indigenous data
ind_nooverlaps # will be many features > need to group by id
union_indnooverlap <- st_union(ind_nooverlaps)
plot(unionindnooverlap)

# erases y from x
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

ind3 <- st_simplify(ind2)
ucind_intersection2 <- st_simplify(ucind_intersection)
testerase_ucind <- st_erase(ind3, ucind_intersection2)
problem <- st_point(c(-740076.4338403953, 3669421.4526430634))
findproblem <- st_within(problem, ind2)
ucind_intersection[49,]
# try again to get the "bites"
mytest <- rbind(uc2,ind2)
invalid <- st_is_valid(mytest[1200:1250,])

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

