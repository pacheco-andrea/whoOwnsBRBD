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

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))
grep("^landTenure", list.files())
tenfolds <- list.files()[grep("^landTenure_", list.files())]


# figure out spatial operations i need to use: ----
# try subsetting indigenous that are inside PAs
tenfolds

# intersect vs. intersection?
testintersect <- st_intersects(ind, uc)
testintersection  <- st_intersection(uc, ind) # returns geometry of the shared portion of x and y
nrow(testintersection)
plot(testintersection$geometry)

# snapping?
testsnap <- st_snap(ind, uc, tolerance = 1000) # run time start ~10:45
plot(testsnap$geometry)
setwd(paste0(wdmain, "data/tmp"))
st_write(testsnap, "test_indigenousSnappedtoUCs.shp")
testsnap2 <- st_buffer(testsnap, dist=0)
# compare snapping result with previous number of intersections
a_int <- st_intersection(testsnap2, uc)
nrow(a_int)
nrow(testintersection)

par(mfrow = c(1,2))
plot(ind$geometry)
plot(testsnap2$geometry)
# there are even more intersections after i snapped indigenous to ucs!
# ok i think I understand a bit better now, the snapping isn't going to work universally for all UCs it could just work for those 
# where the boundaries should be each other...
par(mfrow = c(1,1))

# overlap - should quantify amount of overlap
testoverlap <- st_overlaps(ind, uc) # ids if x and y share space, are of the same dimension, but are not completely contained by each other

# the easiest way I can think of prioritizing is 
# 1. create indigenous that doesn't touch ucs
ind_difference <- st_difference(ind, uc)
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
title("non-overlapping differences") # for these differences, is there no way to determine the order? i guess i could manually think about reordering the datasets
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
plot(i["n.overlaps"])
summary(i$n.overlaps - lengths(i$origins))
# A helper function that erases all of y from x:
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
poly = st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))
lines = st_multilinestring(list(
  cbind(c(0, 1), c(1, 1.05)),
  cbind(c(0, 1), c(0, -.05)),
  cbind(c(1, .95, 1), c(1.05, .5, -.05))
))
snapped = st_snap(poly, lines, tolerance=.1)
plot(snapped, col='red')
plot(poly, border='green', add=TRUE)
plot(lines, lwd=2, col='blue', add=TRUE)
