#### Biodiversity and Tenure in Brazil ####

# this script prepare land tenure data for extractions
# i need to create:
# a variable which tells me whether each analyzed polygon overlaps with some other category 
  # what the overlap is
  # the amount of overlap 
# an easily splittable dataset that can be sent to the extractions (should i split the CAR by states?)

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# get tenure data
setwd(paste0(wdmain, "data/processed/"))
l <- list.files()
tenDataFolds <- l[grep("landTenure_", l)]



# ok so to actually conduct the prioritization, it would be through a spatial join
# https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-join.html#case-2-polygons-target-vs-points-source


ggplot() +
  geom_sf(data = polygons, aes(fill = polygon_name))


#for(i in 1:length(ten))