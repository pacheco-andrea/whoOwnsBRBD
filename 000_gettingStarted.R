#### Getting started analyzing Biodiversity and Tenure in Brazil ####

# this script should be run before the others to load packages, etc.
# there should be no outputs, but should only load objects and functions
# packages should be loaded in each script
# author: Andrea Pacheco
# first run: 25.09.2023

# libraries
# Move on from any deprecated rgdal and raster parts of my workflow to sf and terra

# install.packages("terra")
# # install.packages("rgdal")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("RPostgreSQL")
# install.packages("exactextractr")
# install.packages("cowplot")
# install.packages("blogdown")
# install.packages("tidyterra")
# install.packages("rspatial/geodata")
# install.packages("geobr")
# install.packages("geos")
# install.packages("tidyverse")
# install.packages("fasterize")
# install.packages("classInt")
wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD/"

# function for reading in only specific columns
read_my_shp = function(f){
  s = st_read(f)
  return(s[,c("X_uid_","tipo","uf", "geometry")]) # choose specific columns to keep lighter versions of these data: an identifier, the state, and the geometry
}
# establish my projection: South America Albers Equal Area 
my_crs_SAaea <- "+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# 1. make 1 km2 mask to base rest of rasters on
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009")) # use my biodiversity data as a basis
r <- rast(list.files()[grep("Richness", list.files())[1]])
r <- project(r, my_crs_SAaea)
mask <- r*0

# LEARNING TO USE TERRA INSTEAD OF RASTER ----
# x <- rast()
# plot(x)
# x <- rast(ncol=36, nrow=18, xmin=-1000, xmax=1000,ymin=-100, ymax=900)
# res(x) <- 100
# crs(x) <- "+proj=utm +zone=48 +datum=WGS84"
# 
# # values function: useful for counting values in the raster (instead of freq)
# r <- rast(ncol=10, nrow=10)
# ncell(r)
# hasValues(r)
# values(r) <- 1:ncell(r)
# set.seed(0)
# values(r) <- runif(ncell(r))
# hasValues(r)
# sources(r) #check if in memory layers actually have cell values
# values(r)[10:20]
# plot(r, main = "raster with 100 cells")
# # other functions
# dim(r)
# xmax(r)
# 
# # get the name of an example file installed with the package
# # do not use this construction of your own files
# filename <- system.file("ex/meuse.tif", package="terra")
# filename
# r <- rast(filename)
# plot(r)
# 
# filename <- system.file("ex/logo.tif", package="terra")
# filename
# b <- rast(filename)
# b
# b[[2]] #layers on the raster still work with [[]]


# important/useful functions!
# crop = normal crop
# trim = removes OUTER cells with NAs, extend = the opposite
# merge = merges 2 or more rasters into one object. must have same res and origin.
# aggregate and disagg are the same as before
# resample (remember to not use this for creating a larger resolution - use aggregate for this.)

# mask function: punches holes from the mask to the desired raster
# r <- rast(ncols=10, nrows=10)
# m <- rast(ncols=10, nrows=10)
# values(r) <- 1:100
# set.seed(1965)
# x <- round(3 * runif(ncell(r)))
# x[x==0] <- NA
# values(m) <- x
# mr <- mask(r,m) # so, this shows the mask function punches holes from the mask to the desired raster 

# rasterize Polygons
# f <- system.file("ex/lux.shp", package="terra")
# v <- vect(f)
# r <- rast(v, ncols=75, nrows=100)
# z <- rasterize(v, r, "NAME_2")
# plot(z)

# SMALL TEST intersection of UCs and IND to figure out what needs to be done ----

# # Land tenure data folders ----
# setwd(paste0(wdmain,"/data/processed/"))
# 
# # A) Read and standardize PAs/UCs and indigenous lands: ----
# 
# uc <- st_read("landTenure_UC/landTenure_UCs_MMA_20231212_SAalbers.shp", stringsAsFactors = F, options = "ENCODING=latin1")
# # fix empty geometries
# uc <- st_transform(uc, my_crs_SAaea) # fix projection
# uc$subcateg <- uc$LTcateg # add/clear up this column
# uc$LTcateg <- uc$group
# uc$id <- paste0("UC-", 1:nrow(uc))
# uc <- select(uc, c("LTcateg", "id", "geometry"))
# 
# 
# ind <- st_read("landTenure_IND/landTenure_indigenous_20231212_SAalbers.shp")
# ind <- st_transform(ind, my_crs_SAaea)
# ind$LTcateg <- "indigenous"
# ind$id <- paste0("IN-", 1:nrow(ind))
# ind <- select(ind, c("LTcateg","id", "geometry"))


# test_uc <- uc[136,] #136, 246
# test_ind <- ind[484,]
# test <- rbind(test_ind, test_uc)
# plot(test["LTcateg"])
# 
# uc.ind_intersects <- st_intersects(test_uc, test_ind) # one intersection = intersects with one other feature (not every single line)
# uc.ind_intersection <- st_intersection(test_uc, test_ind) # returns geometry of the shared portion of x and y
# uc.ind_intersection
# plot(uc.ind_intersection$geometry) # can clearly see that it is indeed ONLY the areas that overlap
# # difference in behavior when using one or two objects in the function:
# bound_intersection <- st_intersection(test) 
# # the bound_intersection has three observations instead of one. 
# # it KEEPS the original features shapes
# plot(bound_intersection$geometry)
# plot(bound_intersection["n.overlaps"]) # we get this column
# plot(bound_intersection[1,]$geometry) # the first observation is all the indigenous with NO overlaps
# plot(bound_intersection[2,]$geometry) # the second, is the one with overlap
# plot(bound_intersection[3,]$geometry) # the third, is the part of UC with no overlaps
# 
# # TEST difference bt UCs and IND
# uc.ind_diff <- st_difference(test_uc, test_ind)
# uc.ind_diff # returns one feature - which is essentially feature 3 in the above example. the parts where they don't overlap
#  (the parts of the x that don't overlap with y)


# trick from eduardo?
# acc_sf <- st_transform(acc_sf, crs = 3857)
