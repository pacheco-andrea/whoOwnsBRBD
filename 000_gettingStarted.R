#### Getting started analyzing Biodiversity and Tenure in Brazil ####

# this script should be run before the others to load packages, etc.
# there should be no outputs, but maybe objects like masks loaded? we'll see
# author: Andrea Pacheco
# first run: 25.09.2023

# libraries
# Move on from any deprecated rgdal and raster parts of my workflow to sf and terra

# install.packages("raster") # apparently raster is now simply importing terra, to prevent any code from breaking
# install.packages("terra")
# install.packages("rgdal")
# install.packages("sf")
# install.packages("fasterize")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("RPostgreSQL")
# install.packages("exactextractr")
# install.packages("cowplot")

library(terra)
library(sf)
library(fasterize) # check this one - probably don't need it as the rasterize function prob works now
library(ggplot2)
library(dplyr)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"
#rasterOptions(tmpdir = "N:/eslu/priv/pacheco/biodivTenureBR/tmp", chunksize = 524288, maxmemory = 134217728)
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# LEARNING TO USE TERRA INSTEAD OF RASTER ----
x <- rast()
plot(x)
x <- rast(ncol=36, nrow=18, xmin=-1000, xmax=1000,ymin=-100, ymax=900)
res(x) <- 100
crs(x) <- "+proj=utm +zone=48 +datum=WGS84"

# values function:
r <- rast(ncol=10, nrow=10)
ncell(r)
hasValues(r)
values(r) <- 1:ncell(r)
set.seed(0)
values(r) <- runif(ncell(r))
hasValues(r)
sources(r) #check if in memory layers actually have cell values
values(r)[10:20]
plot(r, main = "raster with 100 cells")
# other functions
dim(r)
xmax(r)

# get the name of an example file installed with the package
# do not use this construction of your own files
filename <- system.file("ex/meuse.tif", package="terra")
filename
r <- rast(filename)
plot(r)

filename <- system.file("ex/logo.tif", package="terra")
filename
b <- rast(filename)
b
b[[2]] #layers on the raster still work with [[]]


# important/useful functions!
# crop = normal crop
# trim = removes OUTER cells with NAs, extend = the opposite
# merge = merges 2 or more rasters into one object. must have same res and origin.
# aggregate and disagg are the same as before
# resample (remember to not use this for creating a larger resolution - use aggregate for this.)

# not sure i get how mask function works
r <- rast(ncols=10, nrows=10)
m <- rast(ncols=10, nrows=10)
values(r) <- 1:100
set.seed(1965)
x <- round(3 * runif(ncell(r)))
x[x==0] <- NA
values(m) <- x
mr <- mask(r,m) # so, this shows the mask function basically punches holes from the mask to the desired raster



# rasterize Polygons
f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)
r <- rast(v, ncols=75, nrows=100)
z <- rasterize(v, r, "NAME_2")
plot(z)
