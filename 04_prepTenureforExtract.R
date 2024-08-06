#### Biodiversity and Tenure in Brazil ####

# this script was the origin of me carrying out these complicated series of polygon intersections across categories 
# i think  because it was the most accurate way to get the overlaps across polygon-based data (and keeping the individual property data) 

# this script should now:
# bring together all the diff datasets that i've preprocessed, takes stock of these and:
# 1) summarizes their area in a table
# 2) writes them out in a file format this is easy to handle for further BD extractions
# KEEP IN MIND: THE WHOLE POINT OF THIS WAS TO TREAT THE OVERLAPS AS SEPARATE CATEGORIES
# THIS JUSTIFIES ALL THE MESS OF DATA CLEANING I HAD TO DO - so the folder structure should reflect these distinct categories

# libraries
library(sf)
library(terra)
library(ggplot2)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# preprocessed tenure data
setwd(paste0(wdmain, "data/processed/"))
l <- list.files()
l[grep("LT_", l)]

list.files()
# A) area calculations ----
# create a matrix for these calculations:
overlap_matrix <- matrix(FALSE, nrow = length(tenureRast), ncol = length(tenureRast))
rownames(overlap_matrix) <- colnames(overlap_matrix) <- names(tenureRast)


# Public lands: no overlaps (LT_no-overlaps) ----
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
l <- list.files()
shps <- l[grep(".shp", l)]
# create data frame 
df_noOverlapsPublic <- data.frame(categ = gsub(".shp", "", shps), areakm2 = NA)
for(i in 1:length(shps))
{
  # read shape
  s <- st_read(shps[i])
  # calculate area
  a <- st_area(s)
  # place in data.frame and divide to convert to km2
  df_noOverlapsPublic[i,"areakm2"] <- sum(a)/1000000
}
df_noOverlapsPublic

# Private lands: no overlaps (LT_no-overlaps_private) -----
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
l <- list.files()
shps <- l[grep(".shp", l)]
df_noOverlapsPrivate <- data.frame(categ = gsub(".shp", "", shps), areakm2 = NA)
# create data frame 
for(i in 1:length(shps))
{
  # read shape
  s <- st_read(shps[i])
  # calculate area
  a <- st_area(s)
  # place in data.frame and divide to convert to km2
  df_noOverlapsPrivate[i,"areakm2"] <- sum(a)/1000000
}
df_noOverlapsPrivate

# Overlaps across categories (but within public and private lands) LT_overlaps ----

# self overlaps

# across categories


# Overlaps ACROSS PUBLIC AND PRIVATE categories ----



# PUT INDIVIDUAL DATA FRAMES TOGETHER HERE!

# B) folder structuring for extractions
